" ===================================================================== custom settings
" Display all matching files when we tab complete
set t_Co=256

set path+=**
set wildmenu

" Folding settings
set foldmethod=indent
set nofoldenable

" Search settings
set hlsearch
set incsearch

" Show line numbers in NERDTree
let NERDTreeShowLineNumbers=1

" Show line number
set relativenumber
set number

" Recommended settings for vimwiki
set nocompatible
filetype plugin on
syntax on

" Use powerline-status
" pip show powerline-status
set rtp+=$HOME/.local/lib/python3.6/site-packages/powerline/bindings/vim
set laststatus=2
set noshowmode


" Recommended settings for syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" 88 columns limit
highlight ColorColumn ctermbg=white 
set colorcolumn=88

" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab

set directory=~/.vim/swapfiles/

" ======================================================================== Key Bindings
" Read up https://stackoverflow.com/questions/2600783/how-does-the-vim-write-with-sudo-trick-work
" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" Easier splilt navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

map <C-n> :NERDTreeToggle<CR>

" map Ctrl-C to copy 
vnoremap <C-C>c "+y
vnoremap <C-C>v "*y

" map Ctrl-V to copy 
inoremap <C-V>c <ESC>"+p
inoremap <C-V>v <ESC>"*p

call plug#begin()
    Plug 'scrooloose/nerdtree'
    Plug 'tpope/vim-surround'
    Plug 'scrooloose/syntastic'
    Plug 'kana/vim-repeat'
    Plug 'ervandew/supertab'
    Plug 'kien/ctrlp.vim'
    Plug 'pangloss/vim-javascript'
    Plug 'vimwiki/vimwiki'
    Plug 'mhinz/vim-signify'
call plug#end()

set background=dark
let g:PaperColor_Theme_Options = {
  \   'theme': {
  \     'default.dark': {
  \       'transparent_background': 1,
  \       'allow_bold': 1,
  \       'allow_italic': 1
  \     }
  \   }
  \ }

colorscheme PaperColor

