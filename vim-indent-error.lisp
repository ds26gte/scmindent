;last modified: 2015-10-30
;(n)vim with v:version == 704 still gets this wrong

(begin
  (display "alpha
           bravo
           charlie")
           "should line up under lparen before display, not under charlie")

; ex:lisp:tw=0
