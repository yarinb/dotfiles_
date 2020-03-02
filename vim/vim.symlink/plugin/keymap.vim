if has("nvim")
  autocmd TermOpen * tnoremap <Esc> <c-\><c-n>
endif

" Use <c-l> to clear the highlighting of :set hlsearch.
nnoremap <c-l> :nohlsearch<cr><c-l>

" Faster window switching
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

" Delete current buffer
nnoremap <leader>bd :call buffer#kill()<cr>
" Delete all buffers
nnoremap <leader>da :call buffer#killall()<cr>
" Wipe all buffers
nnoremap <leader>wa :call buffer#wipeall()<cr>

vnoremap * :<c-u>call text#highlight_visual()<cr>//<cr>
vnoremap # :<c-u>call text#highlight_visual()<cr>??<cr>
vnoremap ! :<c-u>call text#highlight_visual()<cr>
nnoremap ! :call text#highlight(expand('<cword>'))<cr>
vnoremap g! :<c-u>call text#highlight('\<'.text#get_visual().'\>')<cr>
nnoremap g! :call text#highlight('\<'.expand('<cword>').'\>')<cr>
vnoremap <leader>! :<c-u>call text#highlight_sensitive_visual()<cr>
nnoremap <leader>! :call text#highlight_sensitive(expand('<cword>'))<cr>

" Tags
nnoremap g<c-]> :execute 'Tag ' . expand('<cword>')<cr>
vnoremap g<c-]> :<c-u>execute 'Tag ' . text#get_visual()<cr>
nnoremap <c-]> :execute 'tag ' . expand('<cword>')<cr>
nnoremap t<c-]> :execute 'tab tag ' . expand('<cword>')<cr>
nnoremap <c-w><c-]> :execute 'stag ' . expand('<cword>')<cr>
nnoremap v<c-]> :execute 'vert stag ' . expand('<cword>')<cr>

nnoremap Q <nop>

" Indent all file
"nnoremap <leader>ff :call preserve#preserve('silent normal gg=G')<cr>

" Map to show the highlight name under the cursor
nnoremap <f2> :echo color#current()<cr>

map <S-ScrollWheelUp> <C-U>
map <S-ScrollWheelDown> <C-D>

nmap <leader>sn :call sign#goto('next')<cr>
nmap <leader>sN :call sign#goto('previous')<cr>

nnoremap <leader>f :Clap files<cr>
nnoremap <leader>b :Clap buffers<cr>
