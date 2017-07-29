#! /usr/bin/env lua

-- Dorai Sitaram
-- last modified 2017-07-29

-- this script takes lines of Lisp or Scheme code from its 
-- stdin and produces an indented version thereof on its
-- stdout

function file_exists(f)
  local h = io.open(f)
  if h ~= nil then
    io.close(h); return true
  else
    return false
  end
end

local lwfile = os.getenv('HOME') .. '/.lispwords.lua'

if file_exists(lwfile) then 
  loadfile(lwfile)()
else
  lispwords = {}
end

function string_trim_blanks(s) 
  return string.gsub(string.gsub(s, '^%s+', ''), '%s+$', '')
end

function literal_token_p(s)
  return string.find(s, '^[0-9#]')
end

function lisp_indent_number(s)
  s = string.lower(s)
  local n = lispwords[s]
  if n then return n
  elseif string.find(s, '^def') then return 0
  else return -1
  end
end

function past_next_token(s, i, n) 
  local escapep = false
  while true do
    if i > n then return i end
    local c = string.sub(s, i, i)
    if escapep then
      escapep = false; i = i+1
    elseif c == '\\' then
      escapep = true; i = i+1
    elseif c == '#' then
      if string.sub(s, i+1, i+1) == '\\' then
        escapep = true; i = i+2
      else
        return i
      end
    elseif c == "'" or string.find(c, '[][ \t()"`,;]') then
      return i
    else
      escapep = false; i = i+1
    end
  end
end

function calc_subindent(s, i, n) 
  local j = past_next_token(s, i, n)
  local num_aligned_subforms = 0
  local left_indent
  if j == i then
    left_indent = 1
  else
    local w = string.sub(s, i, j-1)
    local c2
    if i > 2 then
      local i2 = i-2; c2 = string.sub(s, i2, i2)
    end
    if c2 == "'" or c2 == '`' then
      left_indent = 1
    else
      local nas = lisp_indent_number(w)
      if nas >= 0 then
        num_aligned_subforms = nas; left_indent = 2
      elseif literal_token_p(w) then
        left_indent = 1
      elseif j > n then
        left_indent = 1
      else
        left_indent = j - i + 2
      end
    end
  end
  return left_indent, num_aligned_subforms, j
end

function num_leading_spaces(s)
  local n = #s
  local i = 0
  local j = 0
  while true do
    if i > n then return 0 end
    local c = string.sub(s, i, i)
    if c == ' ' then 
      i = i+1; j = j+1
    elseif c == '\t' then
      i = i+1; j = j+8
    else
      return j
    end
  end
end

function indent_lines()
  local left_i = 0; local paren_stack = {}; local stringp = false
  while true do
    local curr_line = io.read()
    if not curr_line then break end
    local leading_spaces = num_leading_spaces(curr_line)
    local curr_left_i
    if stringp then
      curr_left_i = leading_spaces
    elseif #paren_stack == 0 then
      if left_i == 0 then
        left_i = leading_spaces
      end
      curr_left_i = left_i
    else
      curr_left_i = paren_stack[1].spaces_before
      local extra_w = 0
      if paren_stack[1].num_finished_subforms < paren_stack[1].num_aligned_subforms then
        paren_stack[1].num_finished_subforms = paren_stack[1].num_finished_subforms + 1
        curr_left_i = curr_left_i + 2
      end
    end
    curr_line = string_trim_blanks(curr_line)
    io.write(string.rep(' ', curr_left_i), curr_line, '\n')
    --
    local n = #curr_line
    local escapep = false
    local inter_word_space_p = false
    local i = 1
    while i <= n do
      local c = string.sub(curr_line, i, i)
      if escapep then
        escapep = false; i = i+1
      elseif c == '\\' then
        escapep = true; i = i+1
      elseif stringp then
        if c == '"' then stringp = false end; i = i+1
      elseif c == ';' then
        break
      elseif c == '"' then
        stringp = true; i = i+1
      elseif c == ' ' or c == '\t' then
        if not inter_word_space_p then
          inter_word_space_p = true
          if #paren_stack > 0 then
            paren_stack[1].num_finished_subforms = paren_stack[1].num_finished_subforms + 1
          end
        end
        i = i+1
      elseif c == '(' or c == '[' then
        inter_word_space_p = false
        local left_indent, num_aligned_subforms, j = calc_subindent(curr_line, i+1, n)
        table.insert(paren_stack, 1, {
          spaces_before = i-1 + curr_left_i + left_indent,
          num_aligned_subforms = num_aligned_subforms,
          num_finished_subforms = 0
        })
        i = j
      elseif string.find(c, '[])]') then
        inter_word_space_p = false
        if #paren_stack > 0 then
          table.remove(paren_stack, 1)
          left_i = 0
        end
        i = i+1
      else
        inter_word_space_p = false; i = i+1
      end
    end
  end
end

indent_lines()
