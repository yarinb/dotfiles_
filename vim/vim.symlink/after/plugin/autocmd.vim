aug user:autocmd
  au!
  au BufReadPost * call buffer#restore_cursor_position()

  au FileType tex,txt,mail,text,markdown setlocal textwidth=80 noautoindent nocindent
  au FileType sh,html,javascript,css,eelixir,sass,scss,yaml setlocal iskeyword+=-
  au CmdWinEnter * setlocal norelativenumber

  au BufReadPost fugitive://* setlocal bufhidden=delete

  au BufReadPost,BufNewFile *gitconfig setfiletype gitconfig
  au BufNewFile,BufRead .babelrc,.jshintrc,.eslintrc setfiletype json

  "au FileType javascript,json call user#surround#javascript_string_interpolation()

  au VimEnter,WinEnter,BufWinEnter,FileType,BufUnload,VimResized * call statusline#update()

  " This is disabled until I know how to override this for python
  "au FileWritePre,BufWritePre * call buffer#trim()

  if has('nvim')
    au TermOpen * setlocal nonumber norelativenumber nocursorline bufhidden=hide
    au FileType fzf tunmap <Esc>
  endif

  " au WinEnter * call window#focus()
  " au WinLeave * call window#unfocus()
  " au FocusLost * call window#unfocus("norelativenumber")
  " au FocusGained * call window#focus("norelativenumber")
  au FocusGained,WinEnter,BufEnter,FileChangedShellPost * checktime | SignifyRefresh
  " Uncomment this to have autosave on FocusLost
  "au WinLeave,FocusLost * silent! call buffer#autosave()

  " Formatters
  au FileType javascript set formatprg=prettier-eslint\ --stdin

  au BufReadPost,BufNewFile,FileWritePost * call buffer#reset_synmaxcol()

aug ENDr
