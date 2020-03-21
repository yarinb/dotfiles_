" Load FZF from homebrew installation
set runtimepath^=/usr/local/opt/fzf
runtime plugin/fzf.vim

let $FZF_DEFAULT_OPTS='--layout=reverse'

let g:fzf_history_dir = '~/.fzf-history'
" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1
let g:fzf_action = {
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-s': 'split',
      \ 'ctrl-v': 'vsplit' }
let g:fzf_tags_command = 'retag'
let g:fzf_layout = {'down': '80%'}

if has('nvim')
  let g:fzf_layout = { 'window': 'call FloatingFZF()' }
end

command! FZFMru call fzf#run(fzf#wrap('MRU', { 'source':  MRUfiles() }))
command! -bang -nargs=* Grep
      \ call fzf#vim#ag(
      \   <q-args>,
      \   <bang>0 ?
      \     fzf#vim#with_preview('up:60%') :
      \     fzf#vim#with_preview('right:50%:hidden', '?'),
      \   <bang>0
      \ )

nnoremap <silent> <leader>ff :Files<cr>
nnoremap <silent> <leader>fb :Buffers<cr>
nnoremap <silent> <c-\> :FZFMru<cr>
nnoremap <silent> <c-n> :BLines<cr>
nnoremap <silent> <c-j> :BTags<cr>
nnoremap <silent> <leader>fg :Ag<cr>
vnoremap <leader>as :<c-u>exec 'Grep '.text#get_visual()<cr>
nnoremap <leader>as :exec 'Grep '.expand('<cword>')<cr>

" previews for fuzzy search
command! -bang -nargs=? -complete=dir Files call fzf#vim#files(<q-args>, fzf#vim#with_preview('down:60%'), <bang>0)
command! -bang -nargs=? -complete=dir GFiles call fzf#vim#gitfiles(<q-args>, fzf#vim#with_preview('up'), <bang>0)
command! -bang -nargs=* Ag call fzf#vim#ag(<q-args>, '', fzf#vim#with_preview('up'), <bang>0)

function! MRUfiles()
  let l:ignore = 'fugitive:\|^/tmp/\|.git/\|term:'
  let l:files = extend(
        \ filter(copy(v:oldfiles), { _, f -> f !~ l:ignore }),
        \ map(filter(range(1, bufnr('$')), { _, f -> buflisted(f) }), { _, f -> bufname(f) }))
  let l:pwd = getcwd()
  return filter(copy(l:files), { _, f -> f =~ l:pwd })
endfunction


function! s:fzf_statusline()
      " Override statusline as you like
      highlight fzf1 ctermfg=161 ctermbg=251
      highlight fzf2 ctermfg=23 ctermbg=251
      highlight fzf3 ctermfg=237 ctermbg=251
      setlocal statusline=%#fzf1#\ >\ %#fzf2#fz%#fzf3#f
endfunction

autocmd! User FzfStatusLine call <SID>fzf_statusline()

function! FloatingFZF()
  let buf = nvim_create_buf(v:false, v:true)

  let height = float2nr(&lines * 0.9)
  let width = float2nr(&columns * 0.8)
  let horizontal = float2nr((&columns - width) / 2)
  let vertical = 1

  let opts = {
        \ 'relative': 'editor',
        \ 'row': vertical,
        \ 'col': horizontal,
        \ 'width': width,
        \ 'height': height
        \ }

  call nvim_buf_set_keymap(buf, '', '<esc>', ':close!<cr>', {'nowait': v:true})
  call nvim_set_current_win(nvim_open_win(buf, v:true, opts))

  setlocal
        \ buftype=nofile
        \ nobuflisted
        \ bufhidden=hide
        \ nonumber
        \ norelativenumber
        \ signcolumn=no
endfunction
