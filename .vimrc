" ======================================================================== Key Bindings
" Read up https://stackoverflow.com/questions/2600783/how-does-the-vim-write-with-sudo-trick-work
" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" Show cursor line
set cursorline

" F1 to toggle relative number
function! g:ToggleNuMode()
  if &nu == 1 && &rnu == 1
     set nornu nonu
  else
     set nu rnu
  endif
endfunction

" hide number column and column sign
map <F1> :SignifyToggle<cr>:call g:ToggleNuMode()<cr>

noremap <Leader>gd :Gvdiff<cr>

" Easier buffer navigations
nnoremap gb :ls<CR>:b<Space>

" Easier splilt navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" map \s to save
noremap <Leader>s :update<CR> 
" map \q to save
noremap <Leader>s :quit<CR>

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

" bind CtrlF to search with Ag
" nnoremap <C-f> :Ag<SPACE>

" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" Enable folding with the spacebar
nnoremap <Leader>space za

" mapping space to toggle pane
nnoremap <space> <C-w>w

" go to insert mode <C-v> <some key> to get the literal terminal keycode
set timeout timeoutlen=1000 ttimeoutlen=100
set <F37>=[1;5A
map <F37> <C-Up>
nnoremap <C-Up> <C-W>+
set <F36>=[1;5B
map <F36> <C-Down>
nnoremap <C-Down> <C-W>-
set <F35>=[1;5C
map <F35> <C-Right>
nnoremap <C-Right> <C-W>>
set <F34>=[1;5D
map <F34> <C-Left>
nnoremap <C-Left> <C-W><

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
set foldlevel=99

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
set colorcolumn=80

" Replace existing tab with 4 spaces width
set tabstop=4
" When indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab
set directory=~/.vim/swapfiles/

" ==================================================================== Plugins Settings

" === SYNTASTIC
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0
" let g:syntastic_python_checkers = ['pylint', 'flake8']

" === NERDTREE
map <C-n> :NERDTreeToggle<CR>
" Show line numbers in NERDTree
let NERDTreeShowLineNumbers=1

" === INDENT LINE
let g:indentLine_char_list = ['|', '¦', '┆', '┊']

" === QUICK PREVIEW
let g:quickr_preview_exit_on_enter = 1
let g:quickr_preview_on_cursor = 1
let g:quickr_preview_position = 'below'

" === SIGNIFY
let g:signify_vcs_list = [ 'git' ] 

call plug#begin()
    Plug 'scrooloose/nerdtree'
    Plug 'kien/ctrlp.vim'
    Plug 'tpope/vim-surround'
    Plug 'kana/vim-repeat'
    Plug 'ervandew/supertab'
    Plug 'vimwiki/vimwiki'
    Plug 'scrooloose/syntastic'
    Plug 'tpope/vim-commentary'
    " indentation line
    Plug 'yggdroot/indentline'

    " Git Plugin
    Plug 'tpope/vim-fugitive'
    " indicate added/modified/removed lines in a file
    Plug 'mhinz/vim-signify'
    " add some square bracket mappings
    Plug 'tpope/vim-unimpaired'
    " git commit browser
    Plug 'junegunn/gv.vim'

    Plug 'ronakg/quickr-preview.vim'

    Plug 'pangloss/vim-javascript'
call plug#end()

