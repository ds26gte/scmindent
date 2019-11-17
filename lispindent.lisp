":";if test -z "$LISP"; then LISP=ecl; fi
":";if test "$LISP" = clisp; then exec clisp -q $0
":";elif test "$LISP" = clozure; then exec ccl -b -Q -l $0
":";elif test "$LISP" = ecl; then exec ecl -shell $0
":";elif test "$LISP" = sbcl; then exec sbcl --script $0
":";fi

;Dorai Sitaram
;Oct 8, 1999
;last change 2019-11-17

;this script takes lines of Lisp or Scheme code from its
;stdin and produces an indented version thereof on its
;stdout

(defvar *lisp-keywords* '())

(defun set-lisp-indent-number (sym num-of-subforms-to-be-indented-wide)
  (let* ((x (symbol-name sym))
         (c (assoc x *lisp-keywords* :test #'string-equal)))
    (unless c
      (push (setq c (cons x nil)) *lisp-keywords*))
    (setf (cdr c) num-of-subforms-to-be-indented-wide)))

(defun retrieve-env (s)
  (declare (string s))
  #+abcl (ext:getenv s)
  #+allegro (sys:getenv s)
  #+clisp (ext:getenv s)
  #+clozure (ccl:getenv s)
  #+cmucl (cdr (assoc (intern s :keyword)
                      ext:*environment-list* :test #'string=))
  #+ecl (ext:getenv s)
  #+mkcl (mkcl:getenv s)
  #+sbcl (sb-ext:posix-getenv s))

(defun read-home-lispwords ()
  (with-open-file (i (or (retrieve-env "LISPWORDS")
                         (merge-pathnames ".lispwords" (user-homedir-pathname)))
                     :if-does-not-exist nil)
    (when i
      (loop
        (let ((w (or (read i nil) (return))))
          (let ((a (car w)))
            (cond ((numberp a)
                   (mapc (lambda (x) (set-lisp-indent-number x a)) (cdr w)))
                  ((consp a)
                   (let ((n (cadr w)))
                     (mapc (lambda (x) (set-lisp-indent-number x n)) a)))
                  (t (set-lisp-indent-number a (cadr w))))))))))

(defun past-next-atom (s i n)
  (loop
    (when (>= i n) (return n))
    (let ((c (char s i)))
      (cond ((char= c #\\) (incf i))
            ((member c '(#\space #\tab #\( #\) #\[ #\] #\" #\' #\` #\, #\;))
             (return i))))
    (incf i)))

(defun get-lisp-indent-number (s &optional (possible-keyword-p t))
  (or (cdr (assoc s *lisp-keywords* :test #'string-equal))
      (if (zerop (or (search "def" s :test #'char-equal) -1))
          0
        (if possible-keyword-p
            (let ((p (position #\: s :from-end t)))
              (if p
                  (get-lisp-indent-number (subseq s (1+ p)) nil)
                -1))
          -1))))

(defun literal-token-p (s)
  (let ((colon-pos (position #\: s)))
    (if colon-pos
        (if (= colon-pos 0) t nil)
      (let ((s (read-from-string s)))
        (or (characterp s) (numberp s) (stringp s))))))

;(trace get-lisp-indent-number literal-token-p read-from-string past-next-atom)

(defstruct lparen
  spaces-before
  lisp-indent-num
  num-finished-subforms)

(defun calc-subindent (s i n)
  (let* ((j (past-next-atom s i n))
         (lisp-indent-num 0)
         (delta-indent
           (if (= j i) 0
             (let ((w (subseq s i j)))
               (if (or (and (>= i 2) (member (char s (- i 2)) '(#\' #\`)))
                       (literal-token-p w)) 0
                 (progn (setq lisp-indent-num (get-lisp-indent-number w))
                        (case lisp-indent-num
                          ((-2) 0)
                          ((-1) (if (< j n) (+ (- j i) 1) 1))
                          (t 1))))))))
    (values delta-indent lisp-indent-num j)))

(defun num-leading-spaces (s)
  (let ((n (length s))
        (i 0) (j 0))
    (loop
      (when (>= i n) (return 0))
      (case (char s i)
        (#\space (incf i) (incf j))
        (#\tab (incf i) (incf j 8))
        (t (return j))))))

(defun string-trim-blanks (s)
  (string-trim '(#\space #\tab #\newline #\return) s))

(defun indent-lines ()
  (let ((left-i 0) (paren-stack '()) (inside-stringp nil))
    (loop
      (let* ((curr-line (or (read-line nil nil) (return)))
             (leading-spaces (num-leading-spaces curr-line))
             (curr-left-i
               (cond (inside-stringp leading-spaces)
                     ((null paren-stack)
                      (when (= left-i 0) (setq left-i leading-spaces))
                      left-i)
                     (t (let* ((lp (car paren-stack))
                               (nas (lparen-lisp-indent-num lp))
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
              (token-interstice-p nil))
          (flet ((incr-finished-subforms ()
                                         (unless token-interstice-p
                                           (when paren-stack
                                             (incf (lparen-num-finished-subforms
                                                     (car paren-stack))))
                                           (setq token-interstice-p t))))
            ;
            (loop
              (when (>= i n) (return))
              (let ((c (char curr-line i)))
                (cond (escapep (setq escapep nil))
                      ((char= c #\\) (setq token-interstice-p nil escapep t))
                      (inside-stringp (when (char= c #\")
                                        (setq inside-stringp nil)
                                        (incr-finished-subforms)))
                      ((char= c #\;) (incr-finished-subforms) (return))
                      ((char= c #\") (incr-finished-subforms) (setq inside-stringp t))
                      ((member c '(#\space #\tab) :test #'char=)
                       (incr-finished-subforms))
                      ((member c '(#\( #\[) :test #'char=)
                       (incr-finished-subforms)
                       (multiple-value-bind (delta-indent lisp-indent-num j)
                         (calc-subindent curr-line (1+ i) n)
                         (push (make-lparen :spaces-before (+ 1 i curr-left-i delta-indent)
                                            :lisp-indent-num lisp-indent-num
                                            :num-finished-subforms -1)
                               paren-stack)
                         (setq token-interstice-p t)
                         (let ((inext (1+ i)))
                           (when (> j inext)
                             (setq inext j token-interstice-p nil))
                           (setq i (1- inext)))))
                      ((member c '(#\) #\]) :test #'char=)
                       (setq token-interstice-p nil)
                       (cond (paren-stack (pop paren-stack))
                             (t (setq left-i 0))))
                      (t (setq token-interstice-p nil))))
              (incf i))
            (incr-finished-subforms)))))))

(read-home-lispwords)

(indent-lines)

;eof
