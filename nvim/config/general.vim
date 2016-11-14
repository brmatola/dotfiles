" UI Settings
set bg=dark
colorscheme gruvbox
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

" Set leader key to <space>
let mapleader="\<SPACE>"

" Formatting
set showcmd       " Show (partial) command in status line
set showmatch     " Show matching brackets
set showmode      " Show current mode
set ruler         " Show location of cursor
set number        " Show line numbers
set expandtab     " Insert spaces on TAB
set tabstop=2     " TAB = 2 spaces
set shiftwidth=2  " Indentation for < and >
set noerrorbells  " No beeps
set cursorline    " Highlight current line
set nowrap        " Don't wrap long lines to fit screen

" More natural splits
set splitbelow
set splitright

" Show tab, eol, trailing spaces
set list listchars=trail:·,precedes:«,extends:»,eol:↲,tab:▸\

" Search Stuff
set ignorecase " Search case insensitive
set smartcase  " ... unless query has capital letters
set gdefault   " Use 'g' flag by defautl with :s/foo/bar/
set magic      " Use extended regular expressions

autocmd FileType c,cpp :set cindent
autocmd FileType c,cpp :set tabstop=4
autocmd FileType c,cpp :set shiftwidth=4
