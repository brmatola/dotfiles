" My personal Vim configuration
" Brad Matola brmatola@gmail.com

" Basic configuration/setup
set nocompatible
syntax enable
filetype plugin indent on
colorscheme badwolf
call pathogen#infect()
call pathogen#helptags()

" Spaces and Tabs
set tabstop=4       " # spaces in a tab
set softtabstop=4   " # spaces added for <TAB>
set shiftwidth=4    " # spaces added for autoindent
set expandtab       " Expand tabs into spaces
set autoindent      " Copies indentation from previous line
set list listchars=trail:·,precedes:«,extends:»,eol:↲,tab:▸\

" UI Config
set number                      " Show linenumbers
set showcmd                     " Show command in bottom bar
set cursorline                  " Highlight current line
set lazyredraw                  " Redraw only when necessary
set ttyfast                     " Take advantage of modern terminal IO
set showmatch                   " Show opening bracket when closing is typed
set backspace=indent,eol,start  " Allow backspacing over errything in insert mode 

" Powerline Configuration
set laststatus=2                " Always display status lines
set showtabline=2               " Always display tab lines
set noshowmode                  " Hide default mode text
python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup

" Syntax Checking via Syntastic
let g:syntastic_check_on_wq = 0
let g:syntastic_check_on_open = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_python_checkers = ['flake8']
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'

" Autocomplete with YouCompleteMe
set wildmenu                    " Autocomplete in command menu
let g:ycm_python_binary_path = '/opt/local/bin/python'
let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'

" Movement and other bindings
let mapleader=","
nnoremap <leader>h <C-w>h       " Navigate vim window splits
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>l <C-w>l
nnoremap <leader><space> :nohl<CR> " remove highlighting
nnoremap <space> za             " space toggles fold

" Searching
set incsearch                   " search as characters are entered
set hlsearch                    " highlight matches

" Folding
set foldenable
set foldlevelstart=10           " Open most folds by default
set foldnestmax=10              " 10 nested fold max
set foldmethod=syntax

" Allow movement in and out of Tmux panes
if exists('$TMUX')
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" vim-latex config (see
" http://vim-latex.sourceforge.net/documentation/latex-suite/recommended-settings.html)
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'

" Autogroups
augroup configgroup
    autocmd!
    autocmd VimEnter * highlight clear SignColumn
    autocmd BufWritePre *.py,*.js,*.c,*.h,*.cpp :call <SID>StripTrailingWhitespaces()
    autocmd BufWritePre *.py,*.js,*.c,*.h,*.cpp :call <SID>TrimEndLines()
augroup end

" Custom Functions
function! <SID>StripTrailingWhitespaces()
    let _s=@/
    let l = line(".")
    let c = col(".")
    %s/|s|+$//e
    let @/=_s
    call cursor(l, c)
endfunction

function! <SID>TrimEndLines()
    let save_cursor = getpos(".")
    :silent! %s#\($\n\s*\)\+\%$##
    call setpos('.', save_cursor)
endfunction
