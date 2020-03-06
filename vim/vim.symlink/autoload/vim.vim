
function! vim#reloadvimrc()
       let save_cursor = getcurpos()
       source $MYVIMRC
       call setpos(‘.’, save_cursor)
endfunction
