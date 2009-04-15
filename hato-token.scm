;;;; hato-token.scm -- unicode-aware word tokenizer
;;
;; Copyright (c) 2005 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; token-fold str knil kons [kons-alpha kons-url kons-ip kons-email...]
;;
;;   KONS is a procedure of the form
;;     (kons str from to acc)
;;   where from and to are the substring indexes of str indicating the
;;   token.
;;
;;   The optional specialized procedures are used instead of KONS when
;;   the token is entirely ASCII alphabetic, a URL, an IP address, or an
;;   email address respectively, and all default to KONS.
;;
;;   Assumes STR is utf-8 encoded, and correctly breaks tokens on any
;;   Unicode punctuation or whitespace, or on boundaries between two
;;   different script types.  This produces good tokens for any language
;;   that uses space between words, and any mixture of such languages.
;;   It also works well for Japanese because of the way Japanese
;;   alternates ideographs and syllabic characters.  It works poorly for
;;   Chinese and Thai.  For Chinese a very simplistic splitter on common
;;   particles and pronouns may work, however Thai can't be parsed
;;   without dictionary lookups which is much too heavy for this tool.
;;   N-grams are the typical approach, and by breaking on each single
;;   character we get this automatically (because we use chains of
;;   tokens), however our chains are likely to be too short for Thai.  A
;;   consideration then for Thai characters is to break by logical
;;   syllables.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unicode character utilities

(eval-when (compile eval)

(define-macro (make-shifted-bit-vector ranges)
  (define (cadr* x) (if (pair? (cdr x)) (cadr x) (car x)))
  (let* ((lo (apply min (map car ranges)))
         (hi (fx+ 1 (apply max (map cadr* ranges))))
         (v (make-u8vector (arithmetic-shift (fx+ 8 (fx- hi lo)) -3) 0)))
    (let lp1 ((ls ranges))
      (if (null? ls)
          `(u8vector ,@(u8vector->list v))
          (let ((limit (fx- (cadr* (car ls)) lo)))
            (let lp2 ((i (fx- (caar ls) lo)))
              (let ((off (arithmetic-shift i -3))
                    (bit (arithmetic-shift 1 (bitwise-and i #b111))))
                (u8vector-set! v off (bitwise-ior (u8vector-ref v off) bit))
                (if (fx>= i limit) (lp1 (cdr ls)) (lp2 (fx+ i 1))))))))))

)

(define (bit-check? v i)
  (let ((off (arithmetic-shift i -3))
        (bit (arithmetic-shift 1 (bitwise-and i #b111))))
    (fx> (bitwise-and (u8vector-ref v off) bit) 0)))

(define lo-seperator-chars
  (make-shifted-bit-vector
    ((#x0000 #x0016) ; CONTROLS AND SPACE
     (#x0022) ; "
     (#x0028 #x0029) ; ()
     (#x003C) ; <
     (#x003E) ; >
     (#x005B) ; [
     (#x005D) ; ]
     (#x007B) ; {
     (#x007D) ; }
     (#x037E) ; GREEK QUESTION MARK
     (#x0387) ; GREEK ANO TELEIA
     (#x055D) ; ARMENIAN COMMA
     (#x055E) ; ARMENIAN QUESTION MARK
     (#x0589) ; ARMENIAN FULL STOP
     (#x05BE) ; HEBREW PUNCTUATION MAQAF
     (#x05C0) ; HEBREW PUNCTUATION PASEQ
     (#x05C3) ; HEBREW PUNCTUATION SOF PASUQ
     (#x05F3) ; HEBREW PUNCTUATION GERESH
     (#x05F4) ; HEBREW PUNCTUATION GERSHAYIM
     (#x060C) ; ARABIC COMMA
     (#x061B) ; ARABIC SEMICOLON
     (#x061F) ; ARABIC QUESTION MARK
     (#x06D4) ; ARABIC FULL STOP
     (#x0700 #x070D) ; SYRIAC END OF PARAGRAPH - SYRIAC HARKLEAN ASTERISCUS
     (#x0E4F) ; THAI CHARACTER FONGMAN
     (#x0E5A) ; THAI CHARACTER ANGKHANKHU
     (#x0E5B) ; THAI CHARACTER KHOMUT
     (#x0F04 #x0F12) ; TIBETAN MARKS
     (#x0F3A #x0F3D) ; TIBETAN MARKS
     (#x0F85) ; TIBETAN MARK PALUTA
     (#x104A #x104F) ; MYANMAR SIGNS
     (#x10FB) ; GEORGIAN PARAGRAPH SEPARATOR
     (#x1361 #x1368) ; ETHIOPIC WORDSPACE - ETHIOPIC PARAGRAPH SEPARATOR
     (#x166D) ; CANADIAN SYLLABICS CHI SIGN
     (#x166E) ; CANADIAN SYLLABICS FULL STOP
     (#x1680) ; OGHAM SPACE
     (#x169B) ; OGHAM FEATHER MARK
     (#x169C) ; OGHAM REVERSED FEATHER MARK
     (#x16EB) ; RUNIC SINGLE PUNCTUATION
     (#x16EC) ; RUNIC MULTIPLE PUNCTUATION
     (#x16ED) ; RUNIC CROSS PUNCTUATION
     (#x1735) ; PHILIPPINE SINGLE PUNCTUATION
     (#x1736) ; PHILIPPINE DOUBLE PUNCTUATION
     (#x17D4 #x17DA) ; KHMER SIGN KHAN - KHMER SIGN KOOMUUT
     (#x1800 #x180A) ; MONGOLIAN BIRGA - MONGOLIAN NIRUGU
     (#x180E) ; MONGOLIAN VOWEL SEPARATOR
     (#x1944 #x1945) ; LIMBU EXCLAMATION MARK - LIMBU QUESTION MARK
     (#x2000 #x200A) ; SPACES
     (#x2010 #x205F) ; HYPHEN - MATH SPACE
     (#x207D #x207E) ; SUPERSCRIPT PARENS
     (#x208D #x208E) ; SUBSCRIPT PARENS
     (#x2329) ; LEFT-POINTING ANGLE BRACKET
     (#x232A) ; RIGHT-POINTING ANGLE BRACKET
     (#x23B4 #x23B6) ; TOP/BOTOM SQUARE BRACKETS
     (#x2768 #x2775) ; MEDIUM PARENS/BRACKETS ORNAMENT
     (#x27E6 #x27EB) ; MATHEMATICAL BRACKETS
     (#x2983 #x2998) ; LEFT/RIGHT BRACKETS
     (#x29D8 #x29DB) ; LEFT/RIGHT WIGGLY FENCE
     (#x29FC #x29FD) ; LEFT/RIGHT-POINTING CURVED ANGLE BRACKET
     (#x3000 #x3002) ; IDEOGRAPHIC SPACE - IDEOGRAPHIC FULL STOP
     (#x3008 #x301F) ; LEFT ANGLE BRACKET - LOW DOUBLE PRIME QUOTATION MARK
     (#x3030) ; WAVY DASH
     (#x303D) ; PART ALTERNATION MARK
     (#x30A0) ; KATAKANA-HIRAGANA DOUBLE HYPHEN
     (#x30FB) ; KATAKANA MIDDLE DOT
     ;; XXXX experimental - some Chinese pronouns and particles
;      (#x4ED6) ; Chinese "he"
;      (#x4F60) ; Chinese "you"
;      (#x5979) ; Chinese "she"
;      (#x60A8) ; Chinese "you" honorific
;      (#x6211) ; Chinese "I"
;      (#x662F) ; Chinese "to be"
;      (#x7684) ; Chinese "possessive"
     )))

(define hi-separator-chars
  (make-shifted-bit-vector
    ((#xFD3E #xFD3F)  ; ORNATE LEFT/RIGHT PARENS
     (#xFE30 #xFE6B)  ; PRES-FORM VERT 2 DOT LEADER .. SMALL @
     (#xFF01 #xFF0F)  ; FULLWIDTH ! - FULLWIDTH -
     (#xFF1A #xFF20)  ; FULLWIDTH : - FULLWIDTH @
     (#xFF3B #xFF3F)  ; FULLWIDTH [ - FULLWIDTH _
     (#xFF5B #xFF65)  ; FULLWIDTH { - HALFWIDTH KATAKANA MIDDLE DOT
     )))

;;   (char-set:union char-set:punctation
;;                   char-set:punctation-terminal
;;                   char-set:whitespace)
;;
(define (char-separator? i)
  (if (fx<= i #x30FB)
    (bit-check? lo-seperator-chars i)
    (if (and (fx>= i #xFD3E) (fx<= i #xFF5B))
      (bit-check? hi-separator-chars (fx- i #xFD3E))
      #f)))

(eval-when (compile eval)

(define-macro (range-case i . ranges)
  (define (split-at! ls i)
    (let lp ((post ls) (pre '()) (i i))
      (if (<= i 0)
        (values (reverse pre) post)
        (lp (cdr post) (cons (car post) pre) (- i 1)))))
  (define (build-clauses ls tmp)
    (let ((len (length ls)))
      (case len
        ((1) (cadar ls))
        ((2) `(if (< ,tmp ,(caadr ls)) ,(cadar ls) ,(cadadr ls)))
        (else
         (receive (head tail) (split-at! ls (quotient (length ls) 2))
           `(if (< ,tmp ,(caar tail))
              ,(build-clauses head tmp)
              ,(build-clauses tail tmp)))))))
  (let lp ((ls (sort ranges (lambda (a b) (< (car a) (car b)))))
           (tmp (gensym)))
    `(let ((,tmp ,i))
       ,(build-clauses ls tmp))))

)

;; *Not* general purpose (most notably ignores the 'common script) but
;; works for our purposes since we never call it on punctuation or other
;; separators.

(define (char-script i)
  (range-case i
    (#x0000 'latin)                 (#x0300 'greek)
    (#x0400 'cyrillic)              (#x0530 'armenian)
    (#x0590 'hebrew)                (#x0600 'arabic)
    (#x0700 'syriac)                (#x0780 'thaana)
    (#x0900 'devanagari)            (#x0980 'bengali)
    (#x0A00 'gurmukhi)              (#x0A80 'gujarati)
    (#x0B00 'oriya)                 (#x0B80 'tamil)
    (#x0C00 'telugu)                (#x0C80 'kannada)
    (#x0D00 'malayalam)             (#x0D80 'sinhala)
    (#x0E00 'thai)                  (#x0E80 'lao)
    (#x0F00 'tibetan)               (#x1000 'myanmar)
    (#x10A0 'georgian)              (#x1100 'hangul)
    (#x1200 'ethiopic)              (#x13A0 'cherokee)
    (#x1400 'canadian-aboriginal)   (#x1680 'ogham)
    (#x16A0 'runic)                 (#x1700 'tagalog)
    (#x1800 'mongolian)             (#x1900 'limbu)
    (#x1950 'tai-le)                (#x1D00 'phonetic-extensions)
    (#x1E00 'latin)                 (#x1F00 'greek)
    (#x3000 'han)                   (#x3040 'hiragana)
    (#x30A0 'katakana)              (#x3100 'bopomofo)
    (#x3130 'hangul)                (#x31A0 'bopomofo)
    (#x31F0 'katakana)              (#x3200 'han)
    (#xA000 'yi)                    (#xB000 'hangul)
    (#xF900 'han)                   (#xFB00 'latin)
    (#xFB13 'armenian)              (#xFB1D 'hebrew)
    (#xFB50 'arabic)                (#xFE30 'han)
    (#xFE70 'arabic)                (#xFF00 'full-width-latin)
    (#xFF61 'half-width-katakana)   (#xFFA0 'half-width-hangul)
    ))

(define (with-utf8-char+next str i end proc)
  (define (bite i)
    (bitwise-and #b00111111 (char->integer (string-ref str i))))
  (let ((b1 (char->integer (string-ref str i))))
    (cond
      ((or (<= b1 #x80) (>= b1 #xF0)) (proc b1 (fx+ i 1)))
      ((fx< b1 #xD0)
       (let ((next (fx+ i 2)))
         (if (fx>= next end)
           (proc b1 next)
           (proc (fx+ (arithmetic-shift (bitwise-and b1 #b00011111) 6)
                      (bite (fx+ i 1)))
                 next))))
      (else
       (let ((next (fx+ i 3)))
         (if (fx>= next end)
           (proc b1 next)
           (proc (fx+ (arithmetic-shift (bitwise-and b1 #b00011111) 12)
                      (fx+
                       (arithmetic-shift (bite (fx+ i 1)) 6)
                       (bite (fx+ i 2))))
                 next)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Tokenizer

;;   hard-break on spaces, controls and parenthesis
;;   don't break on punctuation, but remove leading/trailing
;;   break on double-hyphen -- or ellipses ...
;;   check for url, ip and email patterns and pass to specialized kons
;;   also specialized kons for pure alpha strings

;; XXXX this is a messy hand-coded state-machine, should macroify to
;; make prettier.

(define (token-fold-full str knil kons kons-alpha kons-url kons-ip
                         kons-email start end separators punctuation
                         email-chars url-chars)
  (define (outside to acc)
    (if (fx>= to end)
      acc
      (let ((c (string-ref str to)))
        (cond
          ((char-alphabetic? c)
           (token-alpha to (fx+ to 1) acc))
          ((char-numeric? c)
           (token-number to (fx+ to 1) acc))
          ((or (fx<= (char->integer c) 32)
               (memv c separators)
               (memv c punctuation))
           (outside (fx+ to 1) acc))
          ((or (eqv? c #\.) (eqv? c #\-))
           (token-dot/dash to (fx+ to 1) 1 c acc kons token))
          ((fx>= (char->integer c) #x80)
           (hi-bit to to acc #f))
          (else
           (token to to acc))))))
  (define (token-dot/dash from to count prev acc kons cont)
    (if (fx>= to end)
      (kons str from to acc)
      (let ((c (string-ref str to)))
        (if (eqv? c prev)
          (if (or (fx= count 2) (and (fx= count 1) (eqv? prev #\-)))
            (let lp ((to (fx+ to 1)) (count (fx+ count 1)))
              (cond ((fx= to end)
                     (kons str from (fx- to count) acc))
                    ((eqv? (string-ref str to) prev)
                     (lp (fx+ to 1) (fx+ count 1)))
                    (else
                     (outside to (kons str from (fx- to count) acc)))))
            (token-dot/dash from (fx+ to 1) (fx+ count 1) prev acc kons cont))
          (if (eqv? prev #\.)
            (punctuation from to (fx+ to 1) acc kons cont)
            (cont from (fx+ to 1) acc))))))
  (define (punctuation from last to acc kons cont)
    (if (fx>= to end)
      (kons str from last acc)
      (let ((c (string-ref str to)))
        (cond
          ((memv c punctuation)
           (punctuation from last (fx+ to 1) acc kons cont))
          ((or (fx<= (char->integer c) 32) (memv c separators))
           (outside (fx+ to 1) (kons str from last acc)))
          (else
           (cont from (fx+ to 1) acc))))))
  (define (token-alpha from to acc)
    (if (fx>= to end)
      (kons-alpha str from to acc)
      (let ((c (string-ref str to)))
        (cond
          ((char-alphabetic? c)
           (token-alpha from (fx+ to 1) acc))
          ((or (fx<= (char->integer c) 32) (memv c separators))
           (outside (fx+ to 1) (kons-alpha str from to acc)))
          ((and (eqv? c #\:)
                (fx< (fx+ to 2) end)
                (eqv? (string-ref str (fx+ to 1)) #\/)
                (eqv? (string-ref str (fx+ to 2)) #\/))
           (url from (fx+ to 3) acc))
          ((eqv? c #\@)
           (email from (fx+ to 1) acc))
          ((or (eqv? c #\.) (eqv? c #\-))
           (token-dot/dash from (fx+ to 1) 1 c acc kons-alpha token-simple))
          ((memv c punctuation)
           (punctuation from to (fx+ to 1) acc kons-alpha token))
          ((or (char-numeric? c) (memv c email-chars))
           (token-simple from (fx+ to 1) acc))
          (else
           (token from (fx+ to 1) acc))))))
  (define (token-number from to acc)
    (if (fx>= to end)
      (kons str from to acc)
      (let ((c (string-ref str to)))
        (cond
          ((char-numeric? c)
           (token-number from (fx+ to 1) acc))
          ((or (fx<= (char->integer c) 32) (memv c separators))
           (outside (+ to 1) (kons str from to acc)))
          ((and (eqv? c #\.)
                (fx< (+ to 5) end)
                (char-numeric? (string-ref str (fx+ to 1))))
           (let lp ((to (fx+ to 2)) (count 2) (chars 1))
             (if (fx>= to end)
               ((if (fx= count 4) kons-ip kons) str from to acc)
               (let ((c (string-ref str to)))
                 (cond
                   ((char-numeric? c)
                    (if (fx< chars 3)
                      (lp (fx+ to 1) count (fx+ chars 1))
                      (token-simple from (fx+ to 1) acc)))
                   ((and (eqv? c #\.)
                         (fx< (fx+ to 1) end)
                         (char-numeric? (string-ref str (fx+ to 1))))
                    (if (fx< count 4)
                      (lp (fx+ to 2) (fx+ count 1) 1)
                      (token-simple from (fx+ to 1) acc)))
                   ((fx= count 4)
                    (cond
                      ((or (fx<= (char->integer c) 32) (memv c separators))
                       (outside (fx+ to 1) (kons-ip str from to acc)))
                      ((memv c punctuation)
                       (punctuation from to (fx+ to 1) acc kons-ip token))
                      (else (token-simple from (fx+ to 1) acc))))
                   (else
                    (token-simple from (fx+ to 1) acc)))))))
          ((eqv? c #\@)
           (email from (fx+ to 1) acc))
          ((eqv? c #\-)
           (token-dot/dash from (fx+ to 1) 1 c acc kons token-simple))
          ((memv c punctuation)
           (punctuation from to (fx+ to 1) acc kons token))
          ((or (char-alphabetic? c) (memv c email-chars))
           (token-simple from (fx+ to 1) acc))
          (else
           (token from (fx+ to 1) acc))))))
  (define (token-simple from to acc)
    (if (fx>= to end)
      (kons str from to acc)
      (let ((c (string-ref str to)))
        (cond
          ((or (char-alphabetic? c) (char-numeric? c) (memv c email-chars))
           (token-simple from (fx+ to 1) acc))
          ((or (fx<= (char->integer c) 32) (memv c separators))
           (outside (fx+ to 1) (kons str from to acc)))
          ((eqv? c #\@)
           (email from (fx+ to 1) acc))
          ((or (eqv? c #\.) (eqv? c #\-))
           (token-dot/dash from (fx+ to 1) 1 c acc kons token-simple))
          ((memv c punctuation)
           (punctuation from to (fx+ to 1) acc kons token))
          (else
           (token from (fx+ to 1) acc))))))
  (define (token from to acc)
    (if (fx>= to end)
      (kons str from to acc)
      (let ((c (string-ref str to)))
        (cond
          ((or (fx<= (char->integer c) 32) (memv c separators))
           (outside (fx+ to 1) (kons str from to acc)))
          ((or (eqv? c #\.) (eqv? c #\-))
           (token-dot/dash from (fx+ to 1) 1 c acc kons token))
          ((memv c punctuation)
           (punctuation from to (fx+ to 1) acc kons token))
          ((fx> (char->integer c) #xFF)
           (hi-bit from to c acc))
          (else
           (token from (fx+ to 1) acc))))))
  (define (url from to acc)
    (if (fx>= to end)
      (kons-url str from to acc)
      (let ((c (string-ref str to)))
        (cond
          ((or (char-alphabetic? c)
               (char-numeric? c)
               (memv c url-chars))
           (url from (fx+ to 1) acc))
          ((or (fx<= (char->integer c) 32) (memv c separators))
           (outside (fx+ to 1) (kons-url str from to acc)))
          ((or (eqv? c #\.) (eqv? c #\-))
           (token-dot/dash from (fx+ to 1) 1 c acc kons-url url))
          ((memv c punctuation)
           (punctuation from to (fx+ to 1) acc kons-url url))
          (else
           (token from (fx+ to 1) acc))))))
  (define (email from to acc)
    (if (fx>= to end)
      (kons-email str from to acc)
      (let ((c (string-ref str to)))
        (cond
          ((or (char-alphabetic? c) (char-numeric? c) (memv c email-chars))
           (email from (fx+ to 1) acc))
          ((or (fx<= (char->integer c) 32) (memv c separators))
           (outside (fx+ to 1) (kons-email str from to acc)))
          ((or (eqv? c #\.) (eqv? c #\-))
           (token-dot/dash from (fx+ to 1) 1 c acc kons-email email))
          ((memv c punctuation)
           (punctuation from to (fx+ to 1) acc kons-email token))
          (else
           (token from (fx+ to 1) acc))))))
  (define (hi-bit from to acc script)
    (with-utf8-char+next str to end
      (lambda  (c next)
        (if (fx>= next end)
          (if (fx= from to)
            acc
            (kons str from to acc))
          (if (char-separator? c)
            (if (fx= from to)
              (outside next acc)
              (outside next (kons str from to acc)))
            (let ((script2 (char-script c)))
              (if (eq? script script2)
                (hi-bit from next acc script)
                (if (fx= from to)
                  (hi-bit to next acc script2)
                  (hi-bit to next (kons str from to acc) script2)))))))))
  ;; run on port or string
  (if (port? str)
    (let ((p str))
      (let lp ()
        (set! str (read-line p))
        (unless (eof-object? str)
          (set! end (string-length str))
          (set! knil (outside start knil))
          (lp))))
    (outside start knil)))

;; provide friendly defaults
(define (token-fold str knil kons . o)
  (let-optionals* o
      ((kons-alpha kons)
       (kons-url kons)
       (kons-ip kons-url)
       (kons-email kons)
       (start 0)
       (end (and (string? str) (string-length str)))
       (separators '(#\( #\) #\< #\> #\[ #\] #\{ #\} #\"))
       (punctuation '(#\, #\; #\: #\? #\'))
       (email-chars '(#\_ #\+ #\= #\%))
       (url-chars '(#\_ #\+ #\= #\& #\~ #\/ #\% #\!)))
    (token-fold-full str knil kons kons-alpha kons-url kons-ip kons-email
                     start end separators punctuation email-chars url-chars)))

