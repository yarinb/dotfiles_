scriptencoding utf-8


function! s:download(plug_path) abort
  echo '==> Downloading vim-plug ......'
  execute '!curl -fLo' a:plug_path '--create-dirs'
        \   'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
endfunction

function! core#check_vim_plug() abort
  let l:plug_path = g:myvim.is_nvim ? '~/.local/share/nvim/site/autoload/plug.vim' : '~/.vim/autoload/plug.vim'
  if empty(glob(l:plug_path)) | call s:download(l:plug_path) | endif
endfunction

function! core#check(...) abort
  let l:msg = 'Need to install the missing plugins: '
  let missing = filter(values(g:plugs), '!isdirectory(v:val.dir)')
  if len(missing)
    let plugs = map(missing, 'split(v:val.dir, "/")[-1]')
    let l:msg .= string(plugs).' (y/N): '
    let l:msg = s:truncate(l:msg)
    if a:0 == 1
      if exists('*popup_dialog')
        call s:popup_dialog(l:msg)
        return
      endif
      if s:ask(l:msg)
        silent PlugInstall --sync | q
      endif
    else
      echom l:msg
      PlugInstall --sync | q
    endif
  endif
endfunction