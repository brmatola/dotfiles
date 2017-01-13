" deoplete settings
let g:deoplete#enable_at_startup = 1
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" Octave syntax detection
augroup filetypedetect
  au! BufRead,BufNewFile *.m,*.oct set filetype=octave
augroup end

" neovim tmux nav workaround
nnoremap <silent> <BS> :TmuxNavigateLeft<cr>

" Run neomake on file write
autocmd! BufWritePost * Neomake

" rust deoplete
let g:deoplete#sources#rust#racer_binary='/Users/brmatola/.cargo/bin/racer'

" show all open files in tabs
let g:airline#extensions#tabline#enabled = 1

" deoplete-clang
let g:deoplete#sources#clang#libclang_path = "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib"
let g:deoplete#sources#clang#clang_header = "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang"
