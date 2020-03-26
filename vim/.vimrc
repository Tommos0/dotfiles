set background=dark
set number
syntax on
set tabstop=2
set expandtab
set softtabstop=2
set shiftwidth=2
set hlsearch
filetype indent on

let g:netrw_browse_split=4
let g:netrw_winsize = 25
let g:netrw_liststyle = 3
set clipboard^=unnamedplus
set mouse+=a
if !has('nvim')
  set ttymouse=xterm2
endif
map <C-f> :NERDTreeToggle<CR>
"autocmd vimenter * NERDTree
inoremap <C-_> <C-o>:call NERDComment(0,"toggle")<C-m>
nnoremap <C-_> :call NERDComment(0,"toggle")<C-m>
vnoremap <C-_> :call NERDComment(0,"toggle")<C-m>

" Plugins will be downloaded under the specified directory.
call plug#begin('~/.vim/plugged')

" Declare the list of plugins.
Plug 'tpope/vim-sensible'
Plug 'junegunn/seoul256.vim'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'yegappan/mru'
Plug 'itchyny/lightline.vim'
Plug 'preservim/nerdcommenter'
Plug 'airblade/vim-gitgutter'
Plug 'ianks/vim-tsx'
Plug 'davidhalter/jedi-vim'
Plug 'cespare/vim-toml'
Plug 'hashivim/vim-terraform'
if has('nvim')
  Plug 'Shougo/deoplete.nvim'
  Plug 'HerringtonDarkholme/yats.vim'
  Plug 'mhartington/nvim-typescript', {'do': './install.sh'}
 " For Denite features
  Plug 'Shougo/denite.nvim'
endif
"Plug 'roxma/nvim-yarp'
"Plug 'roxma/vim-hug-neovim-rpc'
let g:deoplete#enable_at_startup = 1

" List ends here. Plugins become visible to Vim after this call.
call plug#end()
let g:seoul256_background = 236
colo seoul256
