" ======================================================================== Key Bindings
" Read up https://stackoverflow.com/questions/2600783/how-does-the-vim-write-with-sudo-trick-work
" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" Show cursor line
set cursorline

" Map Ctrl-A -> Start of line, Ctrl-E -> End of line
noremap <C-a> <Home>
noremap <C-e> <End>

" Natural split
set splitbelow
set splitright

" If a line gets wrapped to two lines, j wont skip over the '2nd line'
nnoremap j gj
nnoremap k gk

" highlight last inserted text
nnoremap gV `[v`]

" enable true colorsupport
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" F1 to toggle relative number
function! g:ToggleNuMode()
  if &nu == 1 && &rnu == 1
     set nornu nonu
  else
     set nu rnu
  endif
endfunction

" change working directory to current directory
nnoremap <Leader>cd :cd %:p:h<CR>

" pwd
nnoremap <Leader>pwd :pwd<CR>

" ####################################################### all the F<?> mappings
" hide number column and column sign
map <F1> :SignifyToggle<cr>:call g:ToggleNuMode()<cr>

" remove trailing spaces
:nnoremap <silent> <F5> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>


noremap <Leader>gd :Gvdiff<cr>

" Easier buffer navigations
nnoremap gb :ls<CR>:b<Space>

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

" Replace bullet glyph with *
command Replacebullet %s/•/*/g

" == The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor
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

" Quickly quit editting without save
nnoremap <Leader>q :q<CR>

" Quickly source .vimrc
nnoremap <leader>r :source $MYVIMRC<CR>

" Toggle between buffer
nnoremap <Leader><Leader> :b#<CR>

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
highlight ColorColumn ctermbg=235
set colorcolumn=80

" Replace existing tab with 4 spaces width
set tabstop=4
" When indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab
" Shifting using `>` / `<` will be rounded off. ie. when i am at 3 spaces,
" hitting `>>` will bring to 4 spaces instead of 3+`shiftwidth` spaces
set shiftround

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
" By default identLine set conceal level to 2 thus and causing some concealed
" text to be completely hidden. ie. Text surrounded by ``
let g:indentLine_fileTypeExclude = ['json', 'md']
let g:indentLine_setConceal = 2
" default ''.
" " n for Normal mode
" " v for Visual mode
" " i for Insert mode
" " c for Command line editing, for 'incsearch'
" default value is "inc"
let g:indentLine_concealcursor = ""


" === QUICK PREVIEW
let g:quickr_preview_exit_on_enter = 1
let g:quickr_preview_on_cursor = 1
let g:quickr_preview_position = 'below'
" disable key mappings for quick previewr
let g:quickr_preview_keymaps = 0

" === SIGNIFY
let g:signify_vcs_list = [ 'git' ] 

" === vim-markdown
let g:vim_markdown_conceal = 0

" === vim-wiki
" Do not use vimwiki filetype for non-vimwiki md files
let g:vimwiki_global_ext = 0
" if the path is changed, remember to update the screenshot script as well
let g:vimwiki_list = [{'path': '~/vimwiki/',
                      \ 'syntax': 'markdown', 
                      \ 'ext': '.md',
                      \ 'index': 'README',}]
let g:vimwiki_conceallevel = 0

" === vim-markdown-toc
let g:vmt_fence_text = 'Do not edit, run `:UpdateToc` to update'
let g:vmt_auto_update_on_save = 0

" === vim-tagbar
nnoremap <leader>tb :TagbarToggle<CR>
let g:tagbar_type_vimwiki = {
        \ 'ctagstype' : 'vimwiki',
        \ 'kinds' : [
                \ 'h:headings',
        \ ],
    \ 'sort' : 0
\ }
let g:tagbar_width = 30
let g:tagbar_left = 1

" === vim-autosave
let g:auto_save = 1

" === fzf
set rtp+=~/.fzf
nnoremap <C-p> :FZF<SPACE><CR>

call plug#begin()
    Plug 'scrooloose/nerdtree'
    Plug 'tpope/vim-surround'
    Plug 'kana/vim-repeat'
    Plug 'ervandew/supertab'
    Plug 'scrooloose/syntastic'
    Plug 'tpope/vim-commentary'
    Plug '907th/vim-auto-save'
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    Plug 'junegunn/fzf.vim'
    Plug 'majutsushi/tagbar'
    
    " markdown plugin
    Plug 'godlygeek/tabular' | Plug 'plasticboy/vim-markdown'
    Plug 'vimwiki/vimwiki'

    " indentation line
    Plug 'yggdroot/indentline'

    " git Plugin
    Plug 'tpope/vim-fugitive'
    " indicate added/modified/removed lines in a file
    Plug 'mhinz/vim-signify'
    " add some square bracket mappings
    Plug 'tpope/vim-unimpaired'
    " git commit browser
    Plug 'junegunn/gv.vim'

    ":GenTocGFM -> generate toc with github flavoured markdown
    Plug 'mzlogin/vim-markdown-toc'

    Plug 'ronakg/quickr-preview.vim'

    Plug 'pangloss/vim-javascript'

    " asthethics
    Plug 'joshdick/onedark.vim'
    Plug 'guns/xterm-color-table.vim'
    Plug 'itchyny/lightline.vim'
call plug#end()

" " onedark configuartions
let g:onedark_terminal_italics = 1
let g:lightline = {
  \ 'colorscheme': 'onedark',
  \ }
set laststatus=2
colorscheme onedark

