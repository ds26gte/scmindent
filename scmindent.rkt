#! /usr/bin/env racket

#lang racket/base

;Dorai Sitaram
;Oct 8, 1999
;last change 2019-11-17

;This script takes lines of Scheme or Lisp code from its
;stdin and produces an indented version thereof on its
;stdout.

(define *lisp-keywords* '())

(define (set-lisp-indent-number sym num-of-subforms-to-be-indented-wide)
  (let* ((x (symbol->string sym))
         (c (assf (lambda (a) (string-ci=? x a)) *lisp-keywords*)))
    (unless c
      (set! c (cons x (box 0)))
      (set! *lisp-keywords* (cons c *lisp-keywords*)))
    (set-box! (cdr c) num-of-subforms-to-be-indented-wide)))

(define (read-home-lispwords)
  (let ([init-file (or (getenv "LISPWORDS")
                       (build-path (find-system-path 'home-dir) ".lispwords"))])
    (when (file-exists? init-file)
      (call-with-input-file init-file
        (lambda (i)
          (let loop ()
            (let ([w (read i)])
              (unless (eof-object? w)
                (let ([a (car w)])
                  (cond [(number? a)
                         (for-each (lambda (x) (set-lisp-indent-number x a)) (cdr w))]
                        [(list? a)
                         (let ([n (cadr w)])
                           (for-each (lambda (x) (set-lisp-indent-number x n)) a))]
                        [else
                          (set-lisp-indent-number a (cadr w))]))
                (loop)))))))))

(define (past-next-atom s i n)
  (let loop ((i i))
    (if (>= i n) n
      (let ((c (string-ref s i)))
        (cond ((char=? c #\\) (loop (+ i 2)))
              ((memv c '(#\space #\tab #\( #\) #\[ #\] #\" #\' #\` #\, #\;))
               i)
              (else (loop (+ i 1))))))))

(define (get-lisp-indent-number s)
  (let ((c (assf (lambda (a) (string-ci=? s a)) *lisp-keywords*)))
    (cond (c (unbox (cdr c)))
          ((and (>= (string-length s) 3)
                (string-ci=? (substring s 0 3) "def")) 0)
          (else -1))))

(define (literal-token? s)
  (let ((x (let ((i (open-input-string s)))
             (begin0 (read i) (close-input-port i)))))
    (or (char? x) (number? x) (string? x))))

(define (calc-subindent s i n)
  (let* ((j (past-next-atom s i n))
         (lisp-indent-num -1)
         (delta-indent
          (if (= j i) 0
            (let ((w (substring s i j)))
              (if (and (>= i 2)
                       (memv (string-ref s (- i 2)) '(#\' #\`)))
                  0
                 (begin
                   (set! lisp-indent-num (get-lisp-indent-number w))
                   (case lisp-indent-num
                     ((-2) 0)
                     ((-1) (if (< j n) (+ (- j i) 1) 1))
                     (else 1))))))))
    (values delta-indent lisp-indent-num j)))

(define (num-leading-spaces s)
  (let ((n (string-length s)))
    (let loop ((i 0) (j 0))
      (if (>= i n) 0
        (case (string-ref s i)
          ((#\space) (loop (+ i 1) (+ j 1)))
          ((#\tab) (loop (+ i 1) (+ j 8)))
          (else j))))))

(define (string-trim-blanks s)
  (let ((n (string-length s)))
    (let ((j (let loop ((j 0))
               (if (or (>= j n)
                       (not (char-whitespace? (string-ref s j))))
                   j
                   (loop (+ j 1))))))
      (if (>= j n) ""
          (let ((k (let loop ((k (- n 1)))
                     (if (or (< k 0)
                             (not (char-whitespace? (string-ref s k))))
                         (+ k 1)
                         (loop (- k 1))))))
            (substring s j k))))))

(define-struct lparen
  (spaces-before lisp-indent-num num-finished-subforms)
  #:mutable)

(define (indent-lines)
  (let ((default-left-i -1) (left-i 0) (paren-stack '()) (inside-string? #f))
    (let line-loop ()
      (let ((curr-line (read-line)))
        (unless (eof-object? curr-line)
          (let* ((leading-spaces (num-leading-spaces curr-line))
                 (curr-left-i
                   (cond (inside-string? leading-spaces)
                         ((null? paren-stack)
                          (when (= left-i 0)
                            (when (= default-left-i -1)
                              (set! default-left-i leading-spaces))
                            (set! left-i default-left-i))
                          left-i)
                         (else (let* ((lp (car paren-stack))
                                      (lin (lparen-lisp-indent-num lp))
                                      (nfs (lparen-num-finished-subforms lp))
                                      (extra-w 0))
                                 (when (< nfs lin)
                                   (set! extra-w 2))
                                 (+ (lparen-spaces-before lp) extra-w))))))
            (set! curr-line (string-trim-blanks curr-line))
            (do ((i 0 (+ i 1)))
                ((= i curr-left-i))
              (write-char #\space))
            (display curr-line) (newline)
            ;
            (let ((n (string-length curr-line))
                  (escape? #f)
                  (token-interstice? #f))
              (let ((incr-finished-subforms (lambda ()
                                              (unless token-interstice?
                                                (cond ((and (pair? paren-stack)
                                                            (car paren-stack)) =>
                                                       (lambda (lp)
                                                         (let ((nfs (lparen-num-finished-subforms lp)))
                                                           (set-lparen-num-finished-subforms!
                                                             lp (+ nfs 1))))))
                                                (set! token-interstice? #t)))))
                (let loop ((i 0))
                  (unless (>= i n)
                    (let ((c (string-ref curr-line i)))
                      (cond (escape? (set! escape? #f) (loop (+ i 1)))
                            ((char=? c #\\)
                             (set! token-interstice? #f)
                             (set! escape? #t) (loop (+ i 1)))
                            (inside-string?
                              (when (char=? c #\")
                                (set! inside-string? #f)
                                (incr-finished-subforms))
                              (loop (+ i 1)))
                            ((char=? c #\;)
                             (incr-finished-subforms)
                             'break-loop)
                            ((char=? c #\")
                             (incr-finished-subforms)
                             (set! inside-string? #t)
                             (loop (+ i 1)))
                            ((memv c '(#\space #\tab))
                             (incr-finished-subforms)
                             (loop (+ i 1)))
                            ((memv c '(#\( #\[))
                             (incr-finished-subforms)
                             (let-values (((delta-indent lisp-indent-num j)
                                           (calc-subindent curr-line (+ i 1) n)))
                               (set! paren-stack
                                 (cons (make-lparen (+ 1 i curr-left-i delta-indent)
                                                    lisp-indent-num
                                                    -1)
                                       paren-stack))
                               (set! token-interstice? #t)
                               (let ((inext (+ i 1)))
                                 (when (> j inext)
                                   (set! inext j)
                                   (set! token-interstice? #f))
                                 (loop inext))))
                            ((memv c '(#\) #\]))
                             (set! token-interstice? #f)
                             (cond ((pair? paren-stack)
                                    (set! paren-stack (cdr paren-stack)))
                                   (else (set! left-i 0)))
                             (loop (+ i 1)))
                            (else (set! token-interstice? #f)
                                  (loop (+ i 1)))))))
                (incr-finished-subforms))))
          (line-loop))))))

(read-home-lispwords)

(indent-lines)

;eof
