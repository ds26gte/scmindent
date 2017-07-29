--[[
.lispwords.lua
Dorai Sitaram
Last modified: 2017-12-12

Associates an indent number with a Lisp keyword. If keyword A's indent number
is N, and a Lisp form introduced by A is split across lines, then if the Ith
subform starts one such line, that line is indented by 3 columns past A if I <=
N, and by 1 column past A otherwise. This file should be placed in $HOME.
--]]

return {
  ['call-with-input-file'] = 1,
  ['case'] = 1,
  ['do'] = 2,
  ['do*'] = 2,
  ['dolist'] = 1,
  ['flet'] = 1,
  ['fluid-let'] = 1,
  ['if'] = 2,
  ['lambda'] = 1,
  ['let'] = 1,
  ['let*'] = 1,
  ['letrec'] = 1,
  ['let-values'] = 1,
  ['multiple-value-bind'] = 2,
  ['unless'] = 1,
  ['when'] = 1,
  ['with-open-file'] = 1,
}
