" Python Interpreter paths
let g:python_host_prog  = '~/.pyenv/versions/neovim2/bin/python'
let g:python3_host_prog = '~/.pyenv/versions/neovim3/bin/python'


" Declare plugins
call plug#begin('~/.config/nvim/plugged')
  Plug 'bling/vim-airline'
  Plug 'morhetz/gruvbox'
  Plug 'neomake/neomake'
  Plug 'tpope/vim-surround'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'rust-lang/rust.vim'
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  " Deoplete Plugins
    Plug 'Shougo/neoinclude.vim'
    Plug 'Shougo/context_filetype.vim'
    Plug 'Shougo/neopairs.vim'
  " Deoplete Sources
    Plug 'zchee/deoplete-jedi'
    Plug 'Shougo/neco-vim'
    Plug 'sebastianmarkow/deoplete-rust'
call plug#end()
