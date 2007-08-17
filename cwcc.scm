
;;;; code from working through
;;;; http://community.schemewiki.org/?call-with-current-continuation

;;; exit continuations -- a contrived example
;; a
(define (search-a wanted? lst)
  (call/cc
    (lambda (return)
      (for-each (lambda (element)
                  (if (wanted? element)
                    (return element)))
                lst)
      #f)))

;; b
(define (treat element like-it)
  (if (good-element? element)
    (like-it 'fnord)))

(define (search-b treat lst)
  (call/cc
    (lambda (return)
      (for-each (lambda (element)
                  (treat element return))
                lst)
      #f)))

;;; full continuations

(define return #f)
(+ 1 (call/cc
       (lambda (cont)
         (set! return cont)
         1)))

;;; coroutines
(define (hefty-computation do-other-stuff)
  (let loop ((n 5))
    (display "Hefty computation: ")
    (display n)
    (newline)
    (set! do-other-stuff (call/cc do-other-stuff))
    (display "Hefty computation (b)")
    (newline)
    (set! do-other-stuff (call/cc do-other-stuff))
    (display "Hefty computation (c)")
    (newline)
    (set! do-other-stuff (call/cc do-other-stuff))
    (if (> n 0)
      (loop (- n 1)))))

(define (superfluous-computation do-other-stuff)
  (let loop ()
    (for-each (lambda (graphic)
                (display graphic)
                (newline)
                (set! do-other-stuff (call/cc do-other-stuff)))
              '("Straight up>" "Quarter after." "Half past." "Quarter til."))
    (loop)))

