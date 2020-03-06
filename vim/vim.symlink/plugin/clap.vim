let g:clap_popup_input_delay=0

aug clap:autocmd
    autocmd!
    autocmd FileType clap_input call s:clap_mappings()
aug END

function! s:clap_mappings()
    inoremap <silent> <buffer> <Esc> <Esc>:call clap#handler#exit()<CR>
	inoremap <silent> <buffer> jj      <C-R>=clap#navigation#linewise('down')<CR><C-R>=clap#navigation#linewise('up')<CR><Esc>
	inoremap <silent> <buffer> <Tab>   <C-R>=clap#navigation#linewise('down')<CR>
	inoremap <silent> <buffer> <S-Tab> <C-R>=clap#navigation#linewise('up')<CR>

	nnoremap <silent> <buffer> <C-f> :<c-u>call clap#navigation#scroll('down')<CR>
	nnoremap <silent> <buffer> <C-b> :<c-u>call clap#navigation#scroll('up')<CR>
	nnoremap <silent> <buffer> <nowait>' :call clap#handler#tab_action()<CR>

	nnoremap <silent> <buffer> sg  :<c-u>call clap#handler#try_open('ctrl-v')<CR>
	nnoremap <silent> <buffer> sv  :<c-u>call clap#handler#try_open('ctrl-x')<CR>
	nnoremap <silent> <buffer> st  :<c-u>call clap#handler#try_open('ctrl-t')<CR>

	nnoremap <silent> <buffer> q     :<c-u>call clap#handler#exit()<CR>
	nnoremap <silent> <buffer> <Esc> :call clap#handler#exit()<CR>
endfunction