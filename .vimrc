" wrap current line
nnoremap <Leader>j viw<ESC>mpa<CR><ESC>`p

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

let mapleader = "\\"

" Show cursor line
set cursorline

" auto save when cursor not moving
autocmd CursorHold * update

" Change cursor in the various different mode
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_SR = "\<Esc>]50;CursorShape=2\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"
if exists('$TMUX')
  let &t_SI = "\ePtmux;\e" . &t_SI . "\e\\"
  let &t_EI = "\ePtmux;\e" . &t_EI . "\e\\"
endif

" Map emacs binding in insert mode
inoremap <C-A> <Home>
inoremap <C-E> <End>
inoremap <C-K> <Esc>ld$i

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

" change working directory to current directory
nnoremap <Leader>cd :cd %:p:h<CR>

" pwd
nnoremap <Leader>pwd :pwd<CR>

" ####################################################### all the F<?> mappings
" F1 to toggle relative number
function! g:ToggleNuMode()
  if &nu == 1 && &rnu == 1
     set nornu nonu
  else
     set nu rnu
  endif
endfunction
" hide number column and column sign
noremap <F1> :SignifyToggle<cr>:call g:ToggleNuMode()<cr>:IndentLinesToggle<cr>

" remove trailing spaces
nnoremap <silent> <F5> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>

noremap <Leader>gd :Gvdiff<cr>

" Easier buffer navigations
nnoremap gb :ls<CR>:b<Space>

" Easier splilt navigations
" nnoremap <C-J> <C-W><C-J>
" nnoremap <C-K> <C-W><C-K>
" nnoremap <C-L> <C-W><C-L>
" nnoremap <C-H> <C-W><C-H>

" Map Ctrl c & v for copy and paste from system clipboard
vnoremap <C-C> "+y
inoremap <C-V> <ESC>"+p
" Map leader c & v for copy and paste from buffer
vnoremap <leader>c "*y
nnoremap <leader>v "*p

" Replace bullet glyph with *
au Filetype vimwiki
    \ command! Replacebullet %s/[•|❒|❍]/*/g

au Filetype vimwiki
    \ command! Spoiler execute "normal! i<details><CR><Tab><summary><CR><TAB>Label<CR><Esc>ciw<Tab></summary><CR>Description<CR><Esc>ciw</details><CR><Esc>"

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
" nnoremap <Leader>space za

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
nnoremap <leader>sv :source $MYVIMRC<CR>

" Quickly edit .vimrc
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

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
" Visual autocomplete for command menu
set wildmenu

" Folding settings
set foldlevel=4
set foldlevelstart=1

" Search settings
set incsearch

" Show line number
set relativenumber
set number

set nocompatible
filetype plugin on
syntax on

" 80 columns limit
set colorcolumn=80
au BufRead,BufNewFile *.md setlocal textwidth=80

" Highlight trailing spaces for all files to red underline (onedark theme)
match SpellBad /\s\+$/

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
let g:quickr_preview_size = '8'

" === SIGNIFY
let g:signify_vcs_list = [ 'git' ]

" === vim-markdown
let g:vim_markdown_conceal = 0
let g:vim_markdown_auto_insert_bullets = 0

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

" === vim-instant-markdown
let g:instant_markdown_slow = 0
let g:instant_markdown_autostart = 0
let g:instant_markdown_mathjax = 1
let g:instant_markdown_logfile = '/tmp/instant_markdown.log'
let g:instant_markdown_autoscroll = 1
let g:instant_markdown_browser = "google-chrome-stable --new-window"
noremap <Leader>p :InstantMarkdownPreview<cr>

" === vim-tagbar
" source code is modified to remove mapping of <Space> as it clashes with my
" pane switching key binding
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

" === fzf
set rtp+=~/.fzf
nnoremap <C-p> :FZF<SPACE><CR>
" search hidden file as well
let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -l -g ""'

" === ALE

let g:ale_completion_tsserver_autoimport = 1
let g:ale_set_quickfix = 0
let g:ale_set_highlights = 1
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

" Error message format
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
" Run :ALEFix upon save
let g:ale_fix_on_save = 1
" general ALE config
let g:ale_fixers = {'*': ['remove_trailing_lines']}
" python ALE configurations
let g:ale_fixers = {'python': ['isort', 'autopep8', 'black']}
let g:ale_python_autopep8_options = "-i"
let g:ale_python_black_options = "-l 80"
let g:ale_python_mypy_options = "--ignore-missing-imports --disallow-untyped-defs"
let g:ale_python_flake8_options = "--max-line-length=80"

" === ALE Light Line
let g:lightline#ale#indicator_checking = "\uf110 "
let g:lightline#ale#indicator_warnings = "\uf071 "
let g:lightline#ale#indicator_errors = "\uf05e "
let g:lightline#ale#indicator_ok = "\uf00c "

" === Light Line
let g:lightline = {
    \ 'colorscheme': 'onedark',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'gitbranch', 'readonly', 'filename', 'modified' ]],
    \   'right': [
    \             [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ],
    \             [ 'lineinfo' ],
    \             [ 'percent' ],
    \             [ 'fileformat', 'fileencoding', 'filetype', 'charvaluehex' ]],
    \ },
    \ 'component_function': {
    \   'gitbranch': 'fugitive#head',
    \ },
    \ 'component_expand': {
    \   'linter_checking': 'lightline#ale#checking',
    \   'linter_warnings': 'lightline#ale#warnings',
    \   'linter_errors': 'lightline#ale#errors',
    \   'linter_ok': 'lightline#ale#ok',
    \ },
    \ 'component_type': {
    \   'linter_checking': 'left',
    \   'linter_warnings': 'warning',
    \   'linter_errors': 'error',
    \   'linter_ok': 'left',
    \ }}

" === vim-diminactive
let g:diminactive_enable_focus = 1

" === easy-motion
" Disable default mappings
let g:EasyMotion_do_mapping = 0
nmap s <Plug>(easymotion-overwin-f2)
nmap S <Plug>(easymotion-overwin-f)
" case insensitive
let g:EasyMotion_smartcase = 1
" Smartsign (type `3` and match `3`&`#`)
let g:EasyMotion_use_smartsign_us = 1
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)

call plug#begin()
    Plug 'scrooloose/nerdtree'
    Plug 'tpope/vim-surround'
    Plug 'kana/vim-repeat'
    Plug 'ervandew/supertab'
    Plug 'dense-analysis/ale'
    Plug 'tpope/vim-commentary'
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    Plug 'junegunn/fzf.vim'
    Plug 'majutsushi/tagbar'
    " Plug 'ycm-core/YouCompleteMe'
    Plug 'maximbaz/lightline-ale'
    Plug 'easymotion/vim-easymotion'

    " markdown plugin
    Plug 'godlygeek/tabular'
    "| Plug 'plasticboy/vim-markdown'
    Plug 'vimwiki/vimwiki'
    Plug 'suan/vim-instant-markdown', {'for': 'markdown'}

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
    " improved syntax highlighting
    Plug 'sheerun/vim-polyglot'

    Plug 'blueyed/vim-diminactive'
    " for better integration with diminactive
    Plug 'tmux-plugins/vim-tmux-focus-events'

call plug#end()

" onedark configuartions
set showmatch
let g:onedark_terminal_italics = 1
let g:onedark_hide_endofbuffer = 1
set laststatus=2

" customize individual aspects of onedark.vim's existing highlight groups
if (has("autocmd") && !has("gui_running"))
  augroup colorset
    autocmd!
    let s:colors = onedark#GetColors()
    let s:red = s:colors.green
    let s:white = s:colors.white
    autocmd ColorScheme * call onedark#extend_highlight("MatchParen", { "fg": s:white, "bg": s:red })
  augroup END
endif

colorscheme onedark

" inside last parenthesis operator
:onoremap il( :<c-u>normal! F)vi(<cr>
