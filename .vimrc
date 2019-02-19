" ============================================================= custom settings

" folding settings
set foldmethod=indent
set nofoldenable

" enable line numbers
let NERDTreeShowLineNumbers=1
" make sure relative line numbers are used
autocmd FileType nerdtree setlocal relativenumber

"add / remove ; to end of line
:nnoremap ;; $a;<Esc>
:nnoremap d; $a<BS><Esc>
" auto opens nerdtree
map <C-n> :NERDTreeToggle<CR>`

" Recommended settings for vimwiki
set nocompatible
filetype plugin on
syntax on

" Recommended settings for papercolor-theme
set t_Co=256   " This is may or may not needed.
set background=light
colorscheme PaperColor
set laststatus=2

" Recommended settings for lightline
set noshowmode
let g:lightline = {
      \ 'colorscheme': 'PaperColor',
      \ }

" Recommended settings for syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Show line number
set relativenumber

set tabstop=4

" 80 columns limit
"highlight OverLength ctermbg=Blue ctermfg=white guibg=#592929
"match OverLength /\%80v.\+/

filetype plugin indent on
" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab

" use system clipboard for copy and paste
set clipboard=unnamedplus

set directory=~/.vim/swapfiles//

call plug#begin()
    Plug 'scrooloose/nerdtree'
    Plug 'tpope/vim-surround'
    Plug 'scrooloose/syntastic'
    Plug 'kana/vim-repeat'
    Plug 'ervandew/supertab'
    Plug 'kien/ctrlp.vim'
    Plug 'pangloss/vim-javascript'
    Plug 'NLKNguyen/papercolor-theme'
    Plug 'vimwiki/vimwiki'
    Plug 'itchyny/lightline.vim'
call plug#end()


