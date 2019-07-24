" ======================================================================== Key Bindings
" Read up https://stackoverflow.com/questions/2600783/how-does-the-vim-write-with-sudo-trick-work
" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" Easier splilt navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>


" map Ctrl-C to copy 
vnoremap <C-C>c "+y
vnoremap <C-C>v "*y

" map Ctrl-V to copy 
inoremap <C-V>c <ESC>"+p
inoremap <C-V>v <ESC>"*p

" == The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" Defines a new command Ag to search for the provided text and open a 'quickfix' window
" bind \ (backward slash) to grep shortcut
command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!

" bind \ to search with Ag
nnoremap \ :Ag<SPACE>

" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
" ===================================================================== custom settings

" Theme Settings
set t_Co=256
set background=dark
" let g:PaperColor_Theme_Options = {
"   \   'theme': {
"   \     'default.dark': {
"   \       'transparent_background': 1,
"   \       'allow_bold': 1,
"   \       'allow_italic': 1
"   \     }
"   \   }
"   \ }
colorscheme PaperColor

" Display all matching files when we tab complete
set path+=**
" Show statusline
set wildmenu

" Folding settings
set foldmethod=indent
set nofoldenable

" Search settings
set hlsearch
set incsearch

" Show line number
set relativenumber
set number

set nocompatible
filetype plugin on
syntax on

" 88 columns limit
highlight ColorColumn ctermbg=white 
set colorcolumn=88

" Replace existing tab with 4 spaces width
set tabstop=4
" When indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab
set directory=~/.vim/swapfiles/

" Use powerline-status
" pip show powerline-status
set rtp+=$HOME/.local/lib/python3.6/site-packages/powerline/bindings/vim
set laststatus=2
set noshowmode

" ==================================================================== Plugins Settings

" === SYNTASTIC
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" == NERDTREE
map <C-n> :NERDTreeToggle<CR>
" Show line numbers in NERDTree
let NERDTreeShowLineNumbers=1

" === INDENT LINE
let g:indentLine_char_list = ['|', '¦', '┆', '┊']

" == QUICK PREVIEW
let g:quickr_preview_exit_on_enter = 1
let g:quickr_preview_on_cursor = 1

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
    Plug 'yggdroot/indentline'
    Plug 'ronakg/quickr-preview.vim'
call plug#end()

