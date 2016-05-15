" My personal Vim configuration
" Brad Matola brmatola@gmail.com 

" Colors {{{
colorscheme badwolf	" awesome colorscheme
syntax enable		" enable syntax processing
" }}}

" Spaces and Tabs {{{
set tabstop=4		" spaces per tab on open file 
set softtabstop=4	" spaces per tab while editing
set expandtab		" tabs are spaces
set list listchars=trail:·,precedes:«,extends:»,eol:↲,tab:▸\
" }}}

" UI Config {{{
set number		" show line numbers
set showcmd		" show command in bottom bar
set cursorline		" highlight current line
filetype plugin indent on	" load filetype-specific indent files
set wildmenu		" visual autocompletes for command menu
set lazyredraw		" redraw only when we need to
set ttyfast         " take advantage of modern terminal data io
set showmatch		" highlight matching [{()}]
set statusline=%F%m%r%h%w\ [TYPE=%Y\ %{&ff}]\
                        \ [%l/%L\ (%p%%)
set laststatus=2
set backspace=indent,eol,start
" }}}

" Searching {{{
set incsearch		" search as characters are entered
set hlsearch		" highlight matches
nnoremap <leader><space> :nohlsearch<CR>	" turn off the search highlight
" }}}

" Folding {{{
set foldenable		" enable folding
set foldlevelstart=10	" open most folds by default
set foldnestmax=10	" 10 nested fold max
nnoremap <space> za	" space open/closes folds
set foldmethod=indent	" fold based on indent level
" }}}

" Leader Shortcuts {{{
let mapleader=","	" leader is comma
inoremap jk <esc>	" escape is bad
nnoremap <leader>u :GundoToggle<CR> " use gundo undo for graphical rep of tree
nnoremap <leader>ev :vsp $MYVIMRC<CR> " edit vimrc
nnoremap <leader>ez :vsp ~/.zshrc<CR> " edit zshrc
nnoremap <leader>sv :source $MYVIMRC<CR> " source vimrc
nnoremap <leader>s :mksession<CR>	" save session
nnoremap <leader>a :Ag			" open ag.vim (source code search)
" }}}

" Movement {{{
nnoremap j gj		" move vertically by visual line (wrapped)
nnoremap k gk		" same
nnoremap gV `[v`]	" highlight last inserted text (last insert mode)
nnoremap <leader>h <C-w>h
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>l <C-w>l
" }}}

" CtrlP settings {{{
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
" }}}

" Launch Config {{{
call pathogen#infect()		" use pathogen
call pathogen#helptags()
" }}}

" NERD_tree Config {{{
let NERDTreeChDirMode=2
let NERDTreeIgnore=['\.vim$', '\~$', '\.pyc$', '\.swp$']
let NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\~$']
" }}}

" Tmux {{{
if exists('$TMUX')
	let &t_SI = "\<Esc>]50;CursorShape=1\x7"
	let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif
" }}}

" Autogroups {{{
augroup configgroup
	autocmd!
	autocmd VimEnter * highlight clear SignColumn
	autocmd BufWritePre *.php, *.py,*.js,*.txt,*.hs 
			\:call <SID>StripTrailingWhitespaces
	autocmd FileType python setlocal commentstring=#\ %s
	autocmd BufEnter *.zsh-theme setlocal filetype=zsh
	autocmd BufEnter *.sh setlocal tabstop=2
	autocmd BufEnter *.sh setlocal shiftwidth=2
	autocmd BufEnter *.sh setlocal softtabstop=2
augroup END
" }}}

" Python specific settings {{{
au FileType py set autoindent
au FileType py set textwidth=79 " PEP-8
let g:ycm_python_binary_path = '/usr/local/bin/python3' " Autocomplete for python3
" }}}

" Vundle Plugins {{{
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'christoomey/vim-tmux-navigator'
Plugin 'Valloric/YouCompleteMe'

call vundle#end()
filetype plugin indent on
" }}}

" Custom Functions {{{
function! ToggleNumber()	" toggle between number and relativenumber
	if(&relativenumber == 1)
		set norelativenumber
		set number
	else
		set relativenumber
	endif
endfunc

function! <SID>StripTrailingWhitespaces()	"self explanatory
	" save last search & cursor pos
	let _s=@/
	let l = line(".")
	let c = col(".")
	%s/|s|+$//e
	let @/=_s
	call cursor(l, c)
endfunction
" }}}

" Organization
set modelines=1		" look for modeline on final line
" vim:foldmethod=marker:foldlevel=0
