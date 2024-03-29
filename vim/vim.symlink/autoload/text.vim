function! text#get_visual() abort
  let [l:lnum1, l:col1] = getpos("'<")[1:2]
  let [l:lnum2, l:col2] = getpos("'>")[1:2]

  let l:lines = getline(l:lnum1, l:lnum2)
  let l:lines[-1] = l:lines[-1][:l:col2 - 1]
  let l:lines[0] = l:lines[0][l:col1 - 1:]

  return join(l:lines, '\n')
endfunction

function! text#highlight_visual() abort
  call text#highlight(text#get_visual())
endfunction

function! text#highlight(text) abort
  let @/ = escape(a:text, ' *^$./[]')
  call feedkeys(":let v:hlsearch=1\<cr>", 'n')
  call feedkeys(":call preserve#preserve('%s///gne')\<cr>", 'n')
endfunction

function! text#highlight_sensitive_visual() abort
  call text#highlight_sensitive(text#get_visual())
endfunction

function! text#highlight_sensitive(text) abort
  let @/ = '\C' . escape(a:text, ' *^$./\[]')
  call feedkeys(":let v:hlsearch=1\<cr>", 'n')
endfunction

function! text#escape_all(text) abort
  return substitute(escape(a:text, '#*^$.?/\|{[()]}'), '\n', '\\n', 'g')
endfunction

