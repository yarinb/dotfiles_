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

  "au VimEnter,WinEnter,BufWinEnter,FileType,BufUnload,VimResized * call statusline#update()
  au BufReadPost,BufNewFile,FileWritePost * call buffer#reset_synmaxcol()

aug END
