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

call core#check_vim_plug()

autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif

" Required:
call plug#begin(expand(g:myvim.is_nvim ? '~/.local/share/nvim/plugged' : '~/.vim/plugged/'))
Plug 'scrooloose/nerdtree'
Plug 'kana/vim-textobj-user'

" Fuzzy finder
Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary' }
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'

Plug 'morhetz/gruvbox'
call plug#end()

" Required:
filetype plugin indent on



if !has('nvim')
  unlet! g:skip_defaults_vim
  source $VIMRUNTIME/defaults.vim

  packadd matchit

  if exists('+termguicolors')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
  endif

  set clipboard+=unnamed      " use the clipboards of vim and win
  set go+=a                   " Visual selection automatically copied to the clipboard

  set encoding=utf8 termencoding=utf8 nobomb
  scriptencoding utf8

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
set updatetime=300
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

set ttimeout

set undofile undoreload=10000

set title
let &titlestring="%{substitute(expand('%:p'), $HOME, '$HOME', '')}"

set statusline=
set showtabline=2
set tabline=%!tabline#update()

set mouse=a

set fillchars=vert:┃

let &grepformat='%f:%l:%c:%m,%f:%l:%m'
let &grepprg='ag --follow --smart-case --vimgrep --skip-vcs-ignores --hidden --nocolor'

let g:mapleader=' '

" Disable standard plugins
" let g:loaded_getscriptPlugin = 1
" let g:loaded_netrwPlugin = 1
" let g:loaded_tarPlugin = 1
" let g:loaded_tutor_mode_plugin = 1
" let g:loaded_vimballPlugin = 1
" let g:loaded_zipPlugin = 1
" let g:loaded_gzip = 1
" let g:loaded_rrhelper = 1
