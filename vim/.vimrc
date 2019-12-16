set background=dark
set number
syntax on
set tabstop=2
set expandtab
set softtabstop=2
set shiftwidth=2
set hlsearch
filetype indent on
" Use CTRL-S for saving, also in Insert mode
noremap <C-S> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <C-O>:update<CR>
let g:netrw_browse_split=4
let g:netrw_winsize = 25
let g:netrw_liststyle = 3
set clipboard^=unnamedplus
set mouse+=a
set ttymouse=xterm2
