scriptencoding utf8

let g:myvim = get(g:, 'myvim', {})

" Identify platform {
let g:myvim.os = {}
let g:myvim.os.mac = has('macunix')
" }

let g:myvim.is_nvim = has('nvim') && exists('*jobwait')
let g:myvim.is_vim8 = exists('*job_start')
let g:myvim.is_gui = has('gui_running')
let g:myvim.is_tmux = !empty($TMUX)

" Set main configuration directory as parent directory
let $VIM_PATH =
	\   exists('*stdpath') ? stdpath('config') :
	\   ! empty($MYVIMRC) ? fnamemodify(expand($MYVIMRC), ':h') :
	\   ! empty($VIMCONFIG) ? expand($VIMCONFIG) :
	\   ! empty($VIM_PATH) ? expand($VIM_PATH) :
	\   fnamemodify(resolve(expand('<sfile>:p')), ':h:h')


" Set data/cache directory as $XDG_CACHE_HOME/vim
let $DATA_PATH =
	\ expand(($XDG_CACHE_HOME ? $XDG_CACHE_HOME : '~/.cache') . '/vim')

if has('vim_starting')
  " Search and use environments specifically made for Neovim.
  if has('nvim') && isdirectory($DATA_PATH . '/venv/neovim2')
    let g:python_host_prog = $DATA_PATH . '/venv/neovim2/bin/python'
  endif

  if has('nvim') && isdirectory($DATA_PATH . '/venv/neovim3')
    let g:python3_host_prog = $DATA_PATH . '/venv/neovim3/bin/python'
  endif

  if ! has('nvim') && has('pythonx')
    if has('python3')
      set pyxversion=3
    elseif has('python')
      set pyxversion=2
    endif
  endif
endif

if &compatible
  set nocompatible
endif

if has('vim_starting')
	" Use spacebar as leader and ; as secondary-leader
	" Required before loading plugins!
	let g:mapleader="\<Space>"
	let g:maplocalleader=';'

	" Release keymappings prefixes, evict entirely for use of plug-ins.
	nnoremap <Space>  <Nop>
	xnoremap <Space>  <Nop>
	nnoremap ,        <Nop>
	xnoremap ,        <Nop>
	nnoremap ;        <Nop>
	xnoremap ;        <Nop>
endif

let s:dein_dir = $DATA_PATH . '/dein'
" ===========
" Plugins
" ===========
augroup PluginInstall
    autocmd!
    autocmd VimEnter * if dein#check_install() | call dein#install() | endif
augroup END
command! -nargs=0 PluginUpdate call dein#update()

if &runtimepath !~# '/dein.vim'
    let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo.dein.vim'

    if !isdirectory(s:dein_repo_dir)
        execute printf('!git clone %s %s', 'https://github.com/Shougo/dein.vim', s:dein_repo_dir)
    endif

    execute 'set runtimepath^=' . s:dein_repo_dir
endif

let g:dein#install_max_processes = 12

if dein#load_state(s:dein_dir)
    call dein#begin(s:dein_dir)

    call dein#add('morhetz/gruvbox')
    call dein#add('chriskempson/base16-vim')

    call dein#add('kana/vim-textobj-user')


    call dein#add('Raimondi/delimitMate')
    call dein#add('christoomey/vim-tmux-navigator')

    " Fuzzy finder
    " denite here?

    " Git
    call dein#add('tpope/vim-commentary')
    call dein#add('tpope/vim-fugitive')
    call dein#add('mhinz/vim-signify')
    call dein#add('andymass/vim-matchup')

    call dein#add('tpope/vim-surround')
    call dein#add('andymass/vim-matchup')

    call dein#add('psf/black', {'on_ft': 'python'})

    if !has('nvim')
      call dein#add('roxma/nvim-yarp')
      call dein#add('roxma/vim-hug-neovim-rpc')
    endif


    " Lazy loaded plugins here
    call dein#add('Shougo/denite.nvim')
    call dein#end()
    call dein#save_state()
endif

if !has('nvim')
  unlet! g:skip_defaults_vim
  source $VIMRUNTIME/defaults.vim

  packadd matchit

  if exists('+termguicolors')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
  endif

  set go+=a                   " Visual selection automatically copied to the clipboard

  set encoding=utf8 termencoding=utf8 nobomb

  set nocompatible
  set autoread autowriteall
  set autoindent
  set formatoptions=tcqj
  set history=10000
  set hlsearch
  set laststatus=2
  set listchars=tab:>\ ,trail:-,nbsp:+
  set smarttab
  set tabpagemax=50
  set ttyfast
  set viminfo^=!
  set belloff=all
  set wildmenu
  set wildmode=list:longest
  " vim specific ends here
else
  " neovim specific is here
  set inccommand=nosplit
  set wildoptions=pum
end

set autowriteall
set fileencoding=utf8 nobomb
set showmatch showmode
set confirm
set clipboard+=unnamed      " use the clipboards of vim and win
set shortmess=actToOFI
set splitright splitbelow
set diffopt+=vertical
set number numberwidth=3
set ignorecase smartcase
set nocursorline
set switchbuf=useopen,usetab,split
set list
set virtualedit=block
set hidden
set signcolumn=yes
set infercase
set tags=tags,tags.gems
set tagcase=followscs

set wildignorecase
set wildignore=*.zip,*.gz,*.bz,*.tar
set wildignore+=*.jpg,*.png,*.gif,*.avi,*.wmv,*.ogg,*.mp3,*.mov

set nowrap

set sessionoptions=winpos,tabpages,help

set copyindent cindent smartindent
set tabstop=2 shiftwidth=2 softtabstop=2
set expandtab shiftround

set foldmethod=manual nofoldenable

set fileformats+=mac

set nobackup nowritebackup noswapfile

set timeout ttimeout
set timeoutlen=500    " Time out on mappins
set ttimeoutlen=10    " Time out on key codes
set updatetime=200    " Idle time to write swap and trigger CursorHold
set redrawtime=150    " time in milliseconds for stopping display redraw

set undofile undoreload=10000

set title
let &titlestring="%{substitute(expand('%:p'), $HOME, '$HOME', '')}"

set statusline=
set showtabline=2
set tabline=%!tabline#update()

set mouse=a

set fillchars=vert:┃
set maxmempattern=10000

let &grepformat='%f:%l:%c:%m,%f:%l:%m'
let &grepprg='ag --follow --smart-case --vimgrep --skip-vcs-ignores --hidden --nocolor'

filetype plugin indent on
syntax enable

" Command to reverse selected lines
command! -bar -range=% Reverse <line1>,<line2>g/^/m<line1>-1|nohl

" Break SQL to better look
command! -range SQLBreak call user#sql#break(<line1>, <line2>)

" Trim the file
command! Trim call buffer#trim()

" Highlight the given text without moving the cursor
command! -nargs=+ H call text#highlight(<q-args>)

" Preserve position while executing the command
command! -nargs=+ P call preserve#preserve(<q-args>)

" Fold all buffer comments
command! FoldComments call fold#comments()

" Remove comments and multiple empty lines
command! -range=% RemoveComments silent call text#remove_comments(<line1>, <line2>)


" Key Mapping  ---------------------------------------------------------------- {{{
" ==============
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

" Toggle editor's visual effects
nmap <Leader>ts :setlocal spell!<cr>
nmap <Leader>tn :setlocal nonumber!<CR>
nmap <Leader>tl :setlocal nolist!<CR>
nmap <silent> <Leader>th :nohlsearch<CR>

" Make Y similar to C and D
nnoremap Y y$

" Indent all file
"nnoremap <leader>ff :call preserve#preserve('silent normal gg=G')<cr>

" Map to show the highlight name under the cursor
nnoremap <f2> :echo color#current()<cr>

map <S-ScrollWheelUp> <C-U>
map <S-ScrollWheelDown> <C-D>

if dein#tap('denite.nvim')
  nnoremap <silent><LocalLeader>r :<C-u>Denite -resume -refresh -no-start-filter<CR>
	nnoremap <silent><LocalLeader>f :<C-u>Denite file/rec<CR>
	nnoremap <silent><LocalLeader>g :<C-u>Denite grep -start-filter<CR>
  nnoremap <silent><LocalLeader>b :<C-u>Denite buffer file_mru -default-action=switch<CR>
	nnoremap <silent><LocalLeader>d :<C-u>Denite directory_rec directory_mru -default-action=cd<CR>
endif
" }}}

" Disable standard plugins
" let g:loaded_getscriptPlugin = 1
" let g:loaded_netrwPlugin = 1
" let g:loaded_tarPlugin = 1
" let g:loaded_tutor_mode_plugin = 1
" let g:loaded_vimballPlugin = 1
" let g:loaded_zipPlugin = 1
" let g:loaded_gzip = 1
" let g:loaded_rrhelper = 1
