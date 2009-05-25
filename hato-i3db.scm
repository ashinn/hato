;;;; hato-i3db.scm -- int -> int x int database
;;
;; Copyright (c) 2003-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(require-extension posix srfi-4)

(module hato-i3db
  (i3db-open i3db-ref i3db-set! i3db-update! i3db-close i3db-hash
   make-i3db i3db?
   i3db-fd set-i3db-fd!
   i3db-mmap set-i3db-mmap!
   i3db-ptr set-i3db-ptr!
   i3db-spam set-i3db-spam!
   i3db-ham set-i3db-ham!
   i3db-mask set-i3db-mask!
   i3db-bytes set-i3db-bytes!
   i3db-salt set-i3db-salt!
   i3db-reader set-i3db-reader!
   i3db-writer set-i3db-writer!
   write-binary-uint32
   )

(import scheme chicken extras posix lolevel srfi-4)

(define (set-file-position! fd where . o)
  (set! (file-position fd) (if (pair? o) (cons where o) where)))

(define (pointer-u24-ref ptr)
  (bitwise-ior (arithmetic-shift (pointer-u16-ref ptr) 8)
               (pointer-u8-ref (pointer-offset ptr 2))))

(define (pointer-u24-set! ptr val)
  (pointer-u16-set! ptr (arithmetic-shift val -8))
  (pointer-u8-set! ptr (bitwise-and val #xFF)))

(define-constant *zero-block-size* 128)
(define-constant *zero-block* (make-string *zero-block-size* #\x0))

(define (file-write-zeros n fd)
  (cond
    ((<= n *zero-block-size*)
     (file-write fd *zero-block* n))
    (else
     (file-write fd *zero-block*)
     (file-write-zeros (- n *zero-block-size*) fd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <i3db>
  (make-i3db fd mmap ptr spam ham mask bytes salt reader writer)
  i3db?
  (fd i3db-fd set-i3db-fd!)
  (mmap i3db-mmap set-i3db-mmap!)
  (ptr i3db-ptr set-i3db-ptr!)
  (spam i3db-spam set-i3db-spam!)
  (ham i3db-ham set-i3db-ham!)
  (mask i3db-mask set-i3db-mask!)
  (bytes i3db-bytes set-i3db-bytes!)
  (salt i3db-salt set-i3db-salt!)
  (reader i3db-reader set-i3db-reader!)
  (writer i3db-writer set-i3db-writer!)
  )

;; key-size: # of bytes per key (detetmines hash modulus)
;; value-size: # of bytes per value (1, 2 or 4)
;;
;; format:
;;   magic (4 bytes)
;;   version (4 bytes)
;;   spam-count (4 bytes)
;;   ham-count (4 bytes)
;;   mask (4 bytes)
;;   value-size (4 bytes)
;;   salt (4 bytes)
;;   entries ...

(define-constant *i3db-magic* #x00112233)
(define-constant *i3db-version* 1)

(define (i3db-open file key-size value-size . o)
  (define (bytes->reader bytes)
    (case bytes
      ((1) pointer-u8-ref)
      ((2) pointer-u16-ref)
      ((3) pointer-u24-ref)
      ((4) pointer-u32-ref)
      (else (error "invalid bytes: " bytes))))
  (define (bytes->writer bytes)
    (case bytes
      ((1) pointer-u8-set!)
      ((2) pointer-u16-set!)
      ((3) pointer-u24-set!)
      ((4) pointer-u32-set!)
      (else (error "invalid bytes: " bytes))))
  (define write? (and (pair? o) (car o)))
  ;;(printf "i3db-open ~S ~S ~S\n" file key-size value-size)
  (if (file-exists? file)
      (let ((fd (file-open file (if write? open/rdwr open/rdonly))))
        (or
         (and-let* ((fd)
                    (mmap (map-file-to-memory
                           #f
                           (file-size file)
                           (if write?
                               (bitwise-ior prot/read prot/write) prot/read)
                           (if write? map/shared map/private)
                           fd))
                    (ptr (memory-mapped-file-pointer mmap))
                    (magic (pointer-u32-ref ptr))
                    ((= magic *i3db-magic*))
                    (version (pointer-u32-ref (pointer-offset ptr 4)))
                    ((= version *i3db-version*))
                    (spam (pointer-u32-ref (pointer-offset ptr 8)))
                    (ham (pointer-u32-ref (pointer-offset ptr 12)))
                    (mask (pointer-u32-ref (pointer-offset ptr 16)))
                    ((= mask (inexact->exact (- (expt 2 (* 8 key-size)) 1))))
                    (bytes (pointer-u32-ref (pointer-offset ptr 20)))
                    ((= bytes value-size))
                    (salt (pointer-u32-ref (pointer-offset ptr 24)))
                    (reader (bytes->reader bytes))
                    (writer (bytes->writer bytes)))
           ;;(printf "make-i3db ~S ~S ~S ~S ~S ~S\n" fd spam ham mask bytes salt)
           (make-i3db fd mmap ptr spam ham mask bytes salt reader writer))
         (begin
           (if fd (file-close fd))
           #f)))
      (let ((fd (file-open file (bitwise-ior open/rdwr open/creat))))
        (or
         (and-let* ((fd)
                    (mask (inexact->exact (- (expt 2 (* 8 key-size)) 1)))
                    (salt (random mask))
                    (bytes value-size)
                    (reader (bytes->reader bytes))
                    (writer (bytes->writer bytes)))
           (set-file-position! fd 0)
           (file-write-zeros (+ (* 4 7) (* 2 value-size (+ mask 1))) fd)
           (set-file-position! fd 0)
           (and-let* ((mmap (map-file-to-memory
                             #f
                             (+ (* 7 4) (* 2 value-size (+ mask 1)))
                             (bitwise-ior prot/read prot/write)
                             (bitwise-ior map/shared)
                             fd))
                      (ptr (memory-mapped-file-pointer mmap)))
             (pointer-u32-set! ptr *i3db-magic*)
             (pointer-u32-set! (pointer-offset ptr 4) *i3db-version*)
             (pointer-u32-set! (pointer-offset ptr 8) 0)
             (pointer-u32-set! (pointer-offset ptr 12) 0)
             (pointer-u32-set! (pointer-offset ptr 16) mask)
             (pointer-u32-set! (pointer-offset ptr 20) bytes)
             (pointer-u32-set! (pointer-offset ptr 24) salt)
             ;;(printf "make-i3db ~S ~S ~S ~S ~S ~S\n" fd 0 0 mask bytes salt)
             (make-i3db fd mmap ptr 0 0 mask bytes salt reader writer)))
         (begin
           (if fd (file-close fd))
           #f)))))

; Bernstein's hash (XXXX use something better)
(define (i3db-hash db key start end init)
  (let ((mask (i3db-mask db)))
    (let lp ((i start) (ans init))
      (if (fx>= i end)
        ans
        (lp (fx+ i 1)
            (fxand mask (fx+ (fx* 65 ans)
                             (char->integer (string-ref key i)))))))))

(define (i3db-index db bucket)
  ;;  header  +   sizeof(rec)   *  bucket index
  (fx+ (fx* 7 4) (fx* (fx* 2 (i3db-bytes db)) bucket)))

(define (i3db-set! db key s h)
  (let ((ptr (i3db-ptr db))
        (writer (i3db-writer db))
        (pos (i3db-index db key)))
    (writer (pointer-offset ptr pos) s)
    (writer (pointer-offset ptr (+ pos (i3db-bytes db))) h)))

(define (i3db-ref db key)
  (let ((ptr (i3db-ptr db))
        (reader (i3db-reader db))
        (pos (i3db-index db key)))
    (let* ((s (reader (pointer-offset ptr pos)))
           (h (reader (pointer-offset ptr (+ pos (i3db-bytes db))))))
      (values s h))))

(define (i3db-update! db key s-off h-off)
  (let ((ptr (i3db-ptr db))
        (reader (i3db-reader db))
        (writer (i3db-writer db))
        (mask (i3db-mask db))
        (pos (i3db-index db key))
        (bytes (i3db-bytes db)))
    (let* ((s (reader (pointer-offset ptr pos)))
           (h (reader (pointer-offset ptr (+ pos bytes))))
           (s2 (fxmax 0 (fx+ s s-off)))
           (h2 (fxmax 0 (fx+ h h-off)))
           (overflow? (or (> s2 mask) (> h2 mask)))
           (s3 (if overflow? (fx/ s2 2) s2))
           (h3 (if overflow? (fx/ h2 2) h2)))
      (unless (and (= s s2) (= h h2))
        (writer (pointer-offset ptr pos) s3)
        (writer (pointer-offset ptr (+ pos bytes)) h3)))))

(define (i3db-close db)
  (if (memory-mapped-file? (i3db-mmap db))
    (unmap-file-from-memory (i3db-mmap db)))
  (if (number? (i3db-fd db))
    (file-close (i3db-fd db))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplified version from srfi-56

(define write-binary-uint32
  (lambda (n fd)
;;     (let* ((vec (make-byte-vector 4 0))
;;            (u32 (byte-vector->u32vector vec)))
;;       (u32vector-set! u32 0 n)
;;       (file-write fd vec))
    (let ((vec (make-string 4)))
      (string-set! vec 0 (bitwise-and n #xFF))
      (string-set! vec 1 (bitwise-and (arithmetic-shift n -8) #xFF))
      (string-set! vec 2 (bitwise-and (arithmetic-shift n -16) #xFF))
      (string-set! vec 3 (bitwise-and (arithmetic-shift n -24) #xFF))
      (file-write fd vec))
    ))

)
