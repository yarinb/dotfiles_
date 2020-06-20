syntax sync minlines=512

set background=dark
set lazyredraw
set regexpengine=2
set cursorline
set termguicolors
set colorcolumn=120
set synmaxcol=200

if !has('gui_running')
  let g:jellybeans_overrides = {
        \    'SpecialKey': { '256ctermfg': '243'}
        \}

  let g:solarized_termcolors=256
endif
let g:jellybeans_use_gui_italics = 0
colorscheme jellybeans
