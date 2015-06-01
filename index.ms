.=* Editing Lisp and Scheme files in vi

“But then, when was the last time you heard
of a lisp programmer that used vi?”
― Paul Fox, vile developer

❧❧❧

The ability to automatically indent code is indispensable when editing
files containing s-expression-based code such as Racket, Common Lisp, Scheme,
LFE,
ART*Enterprise, and other Lisps.

The text editor family vi provides the option \fClisp\fP, which in
conjunction with the options \fCautoindent\fP and \fCshowmatch\fP
provides a form of Lisp indenting, but except in the improved vi clone
Vim, this support is poor in at least two respects:

1. escaped
parentheses and double-quotes are not treated correctly; and

2. all
head-words are
treated identically.

Even the redoubtable Vim, which has improved its Lisp editing
support over the years, and provides the \fClispwords\fP option, continues to fail in
\*[urlh ./vim-indent-error.lisp]strange ways\&.

Fortunately, both vi and Vim let you delegate the responsibility
for indenting such code to an external filter program of your
choosing.  I provide here three such filtering scripts:
\&./scmindent.rkt written in Racket, ./lispindent.lisp in Common
Lisp, and ./scmindent.js in JavaScript.

The Racket
and CL scripts are
operationally identical and use the same type of customization
via the file \fC~/.lispwords\fP. The JavaScript version differs only in that its
customization file is named ./lispwords.json and uses
JSON.
Henceforth, I will refer to just \fCscmindent.rkt\fP with the understanding that
everything mentioned applies equally to \fClispindent.lisp\fP and
\fCscmindent.js\fP.

\fCscmindent.rkt\fP takes
Lisp text from its standard input and produces an indented version
thereof on its standard output.  (Thus, it is possible to use
\fCscmindent.rkt\fP as a command-line filter to “beautify” Lisp code, even if
you don’t use vi.)

Put \fCscmindent.rkt\fP in your \fCPATH\fP.

In Vim, set the \fCequalprg\fP option to the filter name, which causes the
indenting command \fC=\fP to invoke the filter rather than the built-in
indenter.

You might want to make the \fCequalprg\fP setting local to a file
based on its extension:

.EX
autocmd bufread,bufnewfile *.lisp,*.scm setlocal equalprg=scmindent.rkt
.EE

or its filetype:

.EX
autocmd filetype lisp,scheme setlocal equalprg=scmindent.rkt
.EE

In vi’s other than Vim, use the \fC!\fP command to invoke the filter on part or all of
your buffer: Type \fC!\fP to declare you’ll be filtering; a movement command
to scoop up the lines you’ll be filtering; then the filter name
(\fCscmindent.rkt\fP) followed by \fCReturn\fP.

.== How subforms are indented

Lisp indentation has a tacit, widely accepted convention that is not
lightly to be messed with, so \fCscmindent.rkt\fP strives to provide the same
style as emacs, with the same type of customization.

By default, the indentation procedure treats
a form split over two or more lines as
follows.  (A form, if it is a list, is considered to have a head subform and zero or
more argument subforms.)

1. If the head subform is followed by at
least one other subform on the same line, then subsequent lines in the
form are indented to line up directly under the first argument subform.

.EX
(some-user-function-1 arg1
                      arg2
                      ...)
.EE

2. If the head subform is a list and is on a line by itself, then
subsequent lines in the form are indented to
line up directly under the head subform.

.EX
((some-user-function-2)
 arg1
 arg2
 ...)
.EE

3. If the head subform is a symbol and is on a line by itself, then
subsequent lines in the form are indented one column past the beginning
of the head symbol.

.EX
(some-user-function-3
  arg1
  arg2
  ...)
.EE

4. If the head form can be deduced to be a literal, then subforms on
subsequent lines line up directly under it, e.g.

.EX
(1 2 3
 4 5 6)

‘(alpha
  beta
  gamma)
.EE

.== Keywords

However, some keyword symbols are treated differently.  Each such
keyword has a number N associated with it called its Lisp indent number,
which influences how its subforms are indented.  This is almost exactly
analogous to emacs’s \fClisp-indent-function\fP, except I’m using numbers
throughout.

If
the i’th argument subform starts
on a subsequent line, and i <= N, then it is indented 3 columns past the
keyword.  All subsequent
subforms are indented simply one column past the keyword.

.EX
(defun some-user-function-4 (x y)   ;defun is a 2-keyword
  body ...)

(defun some-user-function-5
    (x y)
  body ...)

(if test                            ;if is also a 2-keyword
    then-branch
  else-branch)
.EE

\fCscmindent.rkt\fP pre-sets the indent numbers of many well-known
Lisp keywords.  In addition, any symbol that starts with \fCdef\fP and whose
indent number has not
been explicitly set is assumed to
have an indent number of 0.

.== Customization

You can specify your own Lisp indent numbers for keywords in the file
\fC.lispwords\fP in your home directory.  \fC~/.lispwords\fP can contain any number of
lists: The first element of each list is the Lisp indent number that is
applied to the symbols in the rest of the list.  (Note that in contrast
to Vim’s flat list of \fClispwords\fP, \fC~/.lispwords\fP
allows for different categories of lispwords.  Vim’s \fClispwords\fP are
all of Lisp indent number 0.)

For example, a lot of users prefer the keyword \fCif\fP to have its then-
and else-clauses indented the same amount of 3 columns.  I.e.,
they want it to be a 3-keyword.  A \fC.lispwords\fP entry that would
secure this is:

.EX
(3 if)
.EE

To remove the keywordness of a symbol, you can assign it a Lisp indent
number < 0.  E.g.

.EX
(-1 if)
.EE

would also cause all of \fCif\fP’s subforms to be aligned.  (This is because
−1 causes subforms on subsequent lines to line up against the first
argument subform on the first line, and that happens to be 3 columns
past the beginning of a 2-column keyword like \fCif\fP.  The only difference
between −1 and 3 here is what happens when the \fCif\fP is on a line by
itself, with the test on the line following.  −1 indents subsequent
lines one column past the beginning of the \fCif\fP, whereas 3 continues to
indent them three columns past the beginning of the \fCif\fP.  Further
differences emerge between 3 and −1 when the \fCif\fP has more than three
argument subforms, as allowed by emacs lisp, where 2 and −1 immediately
prove to be better choices than 3.  The author has made 2 the default
because it is the only option that has the merit of indenting the then-
and else-subforms by differing amounts.)

❧❧❧

Last modified: 2015-06-01
