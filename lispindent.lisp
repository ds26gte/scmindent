":";if test -z "$LISP"; then
":";  if test "$USER" = evalwhen; then LISP=ecl
":";  elif test "$(arch 2>/dev/null)" = ppc; then LISP=clozure
":";  else LISP=sbcl
":";  fi; fi
":";if test "$LISP" = clisp; then exec clisp -q $0
":";elif test "$LISP" = clozure; then exec ccl -b -Q -l $0
":";elif test "$LISP" = ecl; then exec ecl -shell $0
":";elif test "$LISP" = sbcl; then exec sbcl --script $0
":";fi

;Dorai Sitaram
;Oct 8, 1999
;last change 2010-10-30

;this script takes lines of Lisp or Scheme code from its
;stdin and produces an indented version thereof on its
;stdout

(defvar *lisp-keywords* '())

(defun define-with-lisp-indent-number (n syms)
  (dolist (sym syms)
    (let* ((x (symbol-name sym))
           (c (assoc x *lisp-keywords* :test #'string-equal)))
      (unless c
        (push (setq c (cons x nil)) *lisp-keywords*))
      (setf (cdr c) n))))

(define-with-lisp-indent-number 0
  '(block
    handler-bind
    loop))

(define-with-lisp-indent-number 1
  '(case
    defpackage do-all-symbols do-external-symbols dolist do-symbols dotimes 
    ecase etypecase eval-when
    flet
    handler-case
    labels lambda let let* let-values
    macrolet
    prog1
    typecase
    unless unwind-protect
    when with-input-from-string with-open-file with-open-socket
    with-open-stream with-output-to-string))

(define-with-lisp-indent-number 2
  '(assert
    defun destructuring-bind do do*
    if
    multiple-value-bind
    with-slots))

(with-open-file (i (merge-pathnames ".lispwords" (user-homedir-pathname))
                   :if-does-not-exist nil)
  (when i
    (loop
      (let ((w (or (read i nil) (return))))
        (define-with-lisp-indent-number (car w) (cdr w))))))

(defun past-next-token (s i n)
  (let ((escapep nil))
    (loop
      (when (>= i n) (return i))
      (let ((c (char s i)))
        (cond (escapep (setq escapep nil))
              ((char= c #\\) (setq escapep t))
              ((char= c #\#)
               (let ((j (+ i 1)))
                 (if (>= j n) (return i)
                   (let ((c (char s j)))
                     (cond ((char= c #\\) (setq escapep t i j))
                           (t (return i)))))))
              ((member c '(#\space #\tab #\( #\) #\[ #\] #\" #\' #\` #\, #\;))
               (return i))))
      (incf i))))

(defun lisp-indent-number (s &optional (possible-keyword-p t))
  (or (cdr (assoc s *lisp-keywords* :test #'string-equal))
      (if (zerop (or (search "def" s :test #'char-equal) -1))
          0
        (if possible-keyword-p
            (let ((p (position #\: s :from-end t)))
              (if p
                  (lisp-indent-number (subseq s (1+ p)) nil)
                -1))
          -1))))

(defun literal-token-p (s)
  (let ((colon-pos (position #\: s)))
    (if colon-pos
        (if (= colon-pos 0) t nil)
      (let ((s (read-from-string s)))
        (or (characterp s) (numberp s) (stringp s))))))

;(trace lisp-indent-number literal-token-p read-from-string past-next-token)

(defstruct lparen
  spaces-before
  num-aligned-subforms
  (num-finished-subforms 0))

(defun calc-subindent (s i n)
  (let* ((j (past-next-token s i n))
         (num-aligned-subforms 0)
         (left-indent
          (if (= j i) 1
            (let ((w (subseq s i j)))
              (if (and (>= i 2) (member (char s (- i 2)) '(#\' #\`))) 1
                (let ((nas (lisp-indent-number w)))
                  (cond ((>= nas 0) (setq num-aligned-subforms nas)
                         2)
                        ((literal-token-p w) 1)
                        ((= j n) 1)
                        (t (+ (- j i) 2)))))))))
    (values left-indent num-aligned-subforms (1- j))))

(defun num-leading-spaces (s)
  (let ((n (length s))
        (i 0))
    (loop
      (when (>= i n) (return 0))
      (case (char s i)
        (#\space (incf i))
        (#\tab (incf i 8))
        (t (return i))))))

(defun string-trim-blanks (s)
  (string-trim '(#\space #\tab #\newline #\return) s))

(defun indent-lines ()
  (let ((left-i 0) (paren-stack '()) (stringp nil))
    (loop
      (let* ((curr-line (or (read-line nil nil) (return)))
             (leading-spaces (num-leading-spaces curr-line))
             (curr-left-i
              (cond (stringp leading-spaces)
                    ((null paren-stack)
                     (when (= left-i 0) (setq left-i leading-spaces))
                     left-i)
                    (t (let* ((lp (car paren-stack))
                              (nas (lparen-num-aligned-subforms lp))
                              (nfs (lparen-num-finished-subforms lp))
                              (extra-w 0))
                         (when (< nfs nas) ;(and (>= nas 0) (< nfs nas))
                           (incf (lparen-num-finished-subforms lp))
                           (setq extra-w 2))
                         (+ (lparen-spaces-before lp)
                            extra-w))))))
        (setq curr-line (string-trim-blanks curr-line))
        (dotimes (k curr-left-i) (write-char #\space))
        (princ curr-line) (terpri)
        ;
        (let ((i 0) (n (length curr-line)) (escapep nil)
              (inter-word-space-p nil))
          (loop
            (when (>= i n) (return))
            (let ((c (char curr-line i)))
              (cond (escapep (setq escapep nil))
                    ((char= c #\\) (setq escapep t))
                    (stringp (when (char= c #\") (setq stringp nil)))
                    ((char= c #\;) (return))
                    ((char= c #\") (setq stringp t))
                    ((member c '(#\space #\tab) :test #'char=)
                     (unless inter-word-space-p
                       (setq inter-word-space-p t)
                       (let ((lp (car paren-stack)))
                         (when lp
                           (incf (lparen-num-finished-subforms lp))))))
                    ((member c '(#\( #\[) :test #'char=)
                     (setq inter-word-space-p nil)
                     (multiple-value-bind (left-indent num-aligned-subforms j)
                         (calc-subindent curr-line (1+ i) n)
                       (push
                        (make-lparen :spaces-before (+ i curr-left-i left-indent)
                                     :num-aligned-subforms num-aligned-subforms)
                        paren-stack)
                       (setq i j)))
                    ((member c '(#\) #\]) :test #'char=)
                     (setq inter-word-space-p nil)
                     (cond (paren-stack (pop paren-stack))
                           (t (setq left-i 0))))
                    (t (setq inter-word-space-p nil)))
              (incf i))))))))

(indent-lines)

;eof
