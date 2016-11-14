source $HOME/.config/nvim/config/functions.vim

" Navigate buffers
nnoremap <C-n> :bnext<CR>
nnoremap <C-p> :bprevious<CR>

" Toggle between normal and relative numbering.
nnoremap <leader>r :call NumberToggle()<cr>
