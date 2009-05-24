
(use test wiki-parse)

(test-begin "wiki-parse")

(for-each
 (lambda (x)
   (test (car x) (cadr x) (wiki-parse (caddr x))))
 '(("wiki-word-bracket" ((p "What's " (wiki "call/cc" "call_2Fcc") "?"))
    "What's [[call/cc]]?")
   ("wiki-word-named" ((p "What's " (wiki "call-with-current-continuation" "call_2Fcc") "?"))
    "What's [[call-with-current-continuation|call/cc]]?")
   ("multiple-wiki-words"
    ((p "To " (wiki "infinity" "infinity") " and " (wiki "beyond" "beyond") "!"))
    "To [[infinity]] and [[beyond]]!")
   ("wiki-url" ((p "This is a bare external link: " (url "http://slashdot.org/" "http://slashdot.org/")))
    "This is a bare external link: http://slashdot.org/")
   ("wiki-url-named" ((p "This is a named external link: " (url "http://slashdot.org/" "/.")))
    "This is a named external link: [http://slashdot.org/ /.]")
   ("wiki-url-multi-line"
    ((p (url "http://uk.reuters.com/article/oilRpt/idUKT2250120090327"
             "highway travel is so cheap")))
    "[http://uk.reuters.com/article/oilRpt/idUKT2250120090327
highway travel is so cheap]")
   ("wiki-par" ((p "par1") (p "par2"))
    "par1\n\npar2")
   ("wiki-hr" ((p "par1") (hr) (p "par2"))
    "par1\n\n----\n\npar2")
   ("wiki-list" ((ul (li "item 1") (li "item 2")))
    "* item 1\n* item 2\n\n")
   ("wiki-bold" ((p "This should be " (b "bold") "."))
    "This should be *bold*.")
   ("wiki-italic" ((p "This should be " (i "italic") "."))
    "This should be /italic/.")
   ("wiki-underline" ((p "This should be " (u "underlined") "."))
    "This should be _underlined_.")
   ("code block"
    ((p "Check out this code:")
     (code "(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))")
     (p "Cool, no?"))
    "Check out this code:
    (define (fact n)
      (if (<= n 1)
          1
          (* n (fact (- n 1)))))
Cool, no?
")
   ("notes"
    ((p "In particular, the new margin-based annotations will\nmake reading and navigation much simpler."
        (note "I have\ndiscovered a truly marvellous proof of this, which this\nmargin is too narrow to contain." #f)))
    "In particular, the new margin-based annotations will
make reading and navigation much simpler.{{I have
discovered a truly marvellous proof of this, which this
margin is too narrow to contain.}}")))

(test-end)

