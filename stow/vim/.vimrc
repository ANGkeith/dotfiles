" Good Read
" https://github.com/romainl/idiomatic-vimrc
"  UI ----------------------------------------------------------------------{{{
" Show line number
set relativenumber
set number

set splitbelow
set splitright

" Display all matching files when we tab complete
set path+=**
" Visual autocomplete for command menu
set wildmenu

" Replace existing tab with 4 spaces width
set tabstop=4
" When indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab
" Shifting using `>` / `<` will be rounded off. ie. when i am at 3 spaces,
" hitting `>>` will bring to 4 spaces instead of 3+`shiftwidth` spaces
set shiftround

" Trigger SpellBad highlight group for trailing spaces
match SpellBad /\s\+$/
" highlight matching [{()}]
set showmatch
" highlight search term on the fly
set incsearch

" highlight current line based on CursorLine
set cursorline
" highlight column based on ColorColumn
set colorcolumn=80
set textwidth=0

" autoreload the file in Vim if it has been changed outside of Vim
set autoread

" Change cursor in the various different mode
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_SR = "\<Esc>]50;CursorShape=2\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"
if exists('$TMUX')
    let &t_SI = "\ePtmux;\e" . &t_SI . "\e\\"
    let &t_EI = "\ePtmux;\e" . &t_EI . "\e\\"
endif

" Always showing status line
set laststatus=2

set noswapfile

set hidden

" Don't update the display while executing macros
set lazyredraw
"---------------------------------------------------------------------------}}}
" Misc ---------------------------------------------------------------------{{{
" enable true colorsupport
set nocompatible
filetype plugin on

" `matchit.vim` is built-in so let's enable it!
" Hit `%` on `if` to jump to `else`.
runtime macros/matchit.vim

" auto save when cursor not moving
autocmd CursorHold * update

if executable('ag')
    " Use ag over grep
    set grepprg=ag\ --nogroup\ --nocolor
endif

" Defines a new command Ag to search for the provided text and open a 'quickfix' window
" bind \ (backward slash) to grep shortcut
command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
" ---------------------------------------------------------------------------}}}
" i3/tmux like behaviour ---------------------------------------------------{{{
" Map emacs binding in insert mode
inoremap <c-a> <home>
inoremap <c-e> <end>
inoremap <c-k> <esc>ld$i

" If a line gets wrapped to two lines, j wont skip over the '2nd line'
nnoremap j gj
nnoremap k gk

" move to and highlight last edited text
nnoremap gV `[v`]

" Panes Management
" nnoremap <c-j> <c-w><c-j>
" nnoremap <c-k> <c-w><c-k>
nnoremap <c-l> <c-w><c-l>
nnoremap <c-h> <c-w><c-h>
" mapping space to toggle pane
nnoremap <space> <c-w>w
" Pane Resize
" go to insert mode <c-v> <some key> to get the literal terminal keycode
set timeout timeoutlen=1000 ttimeoutlen=100
set <f37>=[1;5a
map <f37> <c-up>
nnoremap <c-up> <c-w>+
set <f36>=[1;5b
map <f36> <c-down>
nnoremap <c-down> <c-w>-
set <f35>=[1;5c
map <f35> <c-right>
nnoremap <c-right> <c-w>>
set <f34>=[1;5d
map <f34> <c-left>
nnoremap <c-left> <c-w><

" --------------------------------------------------------------------------}}}
" Custom Functions ---------------------------------------------------------{{{
function! g:ToggleBetweenRelativeAndNothing()
  if &nu == 1 && &rnu == 1
     set nornu nonu
  else
     set nu rnu
  endif
endfunction

function! g:ToggleBetweenRelativeAndNumber()
  if &rnu == 1
     set nornu
  else
     set rnu
  endif
endfunction
" --------------------------------------------------------------------------}}}
" Mapping ------------------------------------------------------------------{{{
""" Use :map <F6> to see what is mapped to <F6> and in which mode.
" wrap current line
nnoremap <leader>j viw<ESC>mpa<CR><ESC>`p

" bind CtrlF to search with Ag
nnoremap <c-f> :Ag<space>

" bind k to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" Quickly quit editing without save
nnoremap <leader>q :q<CR>

" Quickly source .vimrc
nnoremap <leader>sv :source $MYVIMRC<CR>
" Quickly edit .vimrc
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" Toggle between buffer
nnoremap <leader><leader> :b#<cr>

" inside last parenthesis operator
onoremap il( :<c-u>normal! F)vi(<cr>

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" Map Ctrl c & v for copy and paste from system clipboard
vnoremap <c-c> "+y
inoremap <c-v> <esc>"+p
" map leader c & v for copy and paste from buffer
vnoremap <leader>c "*y
nnoremap <leader>v "*p

" Distraction Free Mode
noremap <f1> :SignifyToggle<cr>:call g:ToggleBetweenRelativeAndNothing()<cr>:IndentLinesToggle<cr>
noremap <f2> :call g:ToggleBetweenRelativeAndNumber()<cr>
" remove trailing spaces
nnoremap <silent> <f5> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>

" Easier buffer navigations
nnoremap gb :ls<CR>:b<Space>

" Map ctrl space to toggle foldings
nnoremap <c-@> za

" Map ctrl a to copy whole file
nnoremap <c-a> ggVG"+y
" Custom Text Objects {{{

" ie = inner entire buffer
onoremap ie :exec "normal! ggVG"<cr>

" iv = current viewable text in the buffer
onoremap iv :exec "normal! HVL"<cr>

" }}}
" Plugins --------------------------------------------------------------{{{
noremap <leader>p :InstantMarkdownPreview<cr>
nnoremap <leader>tb :TagbarToggle<cr>
nnoremap <c-p> :FZF<space><cr>
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

nmap S <Plug>(easymotion-overwin-f2)
map  <Leader>f <Plug>(easymotion-bd-f)
" case insensitive
" Smartsign (type `3` and match `3`&`#`)
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)

map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)


nnoremap <leader>hd :SignifyHunkDiff<cr>
nnoremap <leader>hu :SignifyHunkUndo<cr>

nmap s <plug>(SubversiveSubstituteRangeConfirm)
xmap s <plug>(SubversiveSubstituteRangeConfirm)
nmap ss <plug>(SubversiveSubstituteWordRangeConfirm)

" ----------------------------------------------------------------------}}}
" }}}
"  Filetype settings -------------------------------------------------------{{{
"  vim -----------------------------------------------------------------{{{
augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END
" ----------------------------------------------------------------------}}}
"  vimwiki -------------------------------------------------------------{{{
" Replace bullet glyph with *
au Filetype vimwiki
    \ command! Replacebullet %s/[•|❒|❍]/*/g

au Filetype vimwiki
    \ command! Spoiler execute "normal! i<details><CR><Tab><summary><CR><TAB>Label<CR><Esc>ciw<Tab></summary><CR>Description<CR><Esc>ciw</details><CR><Esc>"
" ----------------------------------------------------------------------}}}
" markdown -------------------------------------------------------------{{{
au Filetype markdown
    \ setlocal textwidth=80
" ----------------------------------------------------------------------}}}
"  yaml ----------------------------------------------------------------{{{
au Filetype yaml
    \ setlocal tabstop=2
    \ setlocal shiftwidth=2
" ----------------------------------------------------------------------}}}
" --------------------------------------------------------------------------}}}
" Plugin Configurations ----------------------------------------------------{{{
" scapel -------------------------------------------------------------{{{
let g:ScalpelMap=0
" Show line numbers in NERDTree
" Use <Leader>s instead of default <Leader>e:
" ----------------------------------------------------------------------}}}
" nerdtree -------------------------------------------------------------{{{
map <C-n> :NERDTreeToggle<CR>
" Show line numbers in NERDTree
let NERDTreeShowLineNumbers=1
" ----------------------------------------------------------------------}}}
" indentline {{{
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
" }}}
" quickr-preview {{{
let g:quickr_preview_exit_on_enter = 0
let g:quickr_preview_on_cursor = 1
let g:quickr_preview_position = 'below'
" disable key mappings for quick previewr
let g:quickr_preview_keymaps = 0
let g:quickr_preview_size = '8'
" }}}
" vim-signify {{{
let g:signify_vcs_list = [ 'git' ]
"}}}
" vimwiki {{{
" Do not use vimwiki filetype for non-vimwiki md files
let g:vimwiki_global_ext = 0
" if the path is changed, remember to update the screenshot script as well
let g:vimwiki_list = [{'path': '~/vimwiki/',
                      \ 'syntax': 'markdown',
                      \ 'ext': '.md',
                      \ 'index': 'README',}]
let g:vimwiki_conceallevel = 0
"}}}
" vim-markdown {{{
let g:vim_markdown_conceal = 0
let g:vim_markdown_auto_insert_bullets = 0
"}}}
" vim-markdown-toc {{{
let g:vmt_fence_text = 'Do not edit, run `:UpdateToc` to update'
let g:vmt_auto_update_on_save = 0
"}}}
" vim-instant-markdown {{{
let g:instant_markdown_slow = 0
let g:instant_markdown_autostart = 0
let g:instant_markdown_mathjax = 1
let g:instant_markdown_logfile = '/tmp/instant_markdown.log'
let g:instant_markdown_autoscroll = 1
let g:instant_markdown_browser = "google-chrome-stable --new-window"

" }}}
" tagbar {{{
let g:tagbar_type_vimwiki = {
        \ 'ctagstype' : 'vimwiki',
        \ 'kinds' : [
                \ 'h:headings',
        \ ],
    \ 'sort' : 0
\ }
let g:tagbar_width = 30
let g:tagbar_left = 1
"}}}
" fzf {{{
set rtp+=~/.fzf
" search hidden file as well
let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -l -g ""'
"}}}
" ale {{{
let g:ale_completion_tsserver_autoimport = 1
let g:ale_set_quickfix = 0
let g:ale_set_highlights = 1

" Error message format
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %code%: %s [%severity%]'
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
"}}}
" lightline-ale {{{
let g:lightline#ale#indicator_checking = "\uf110 "
let g:lightline#ale#indicator_warnings = "\uf071 "
let g:lightline#ale#indicator_errors = "\uf05e "
let g:lightline#ale#indicator_ok = "\uf00c "
" }}}
" lightline {{{
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
" }}}
" vim-diminactive {{{
let g:diminactive_enable_focus = 1
" }}}
" vim-easymotion {{{
" Disable default mappings
let g:EasyMotion_do_mapping = 0
let g:EasyMotion_smartcase = 1
let g:EasyMotion_use_smartsign_us = 1
" }}}
" python-mode {{{
let g:pymode_python = 'python3'
" }}}
" }}}
"  Plugins --------------------------------------------------------------------{{{
call plug#begin()
    " utils
    Plug 'scrooloose/nerdtree'
    Plug 'tpope/vim-surround'
    Plug 'kana/vim-repeat'
    Plug 'ervandew/supertab'
    Plug 'ronakg/quickr-preview.vim'
    Plug 'tpope/vim-commentary'
    Plug 'majutsushi/tagbar'
    Plug 'easymotion/vim-easymotion'
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    Plug 'junegunn/fzf.vim'
    " Substitute word on cursor
    Plug 'wincent/scalpel'

    Plug 'dense-analysis/ale'
    " improved syntax highlighting
    Plug 'sheerun/vim-polyglot'

    " filetype python {{{
    Plug 'python-mode/python-mode', { 'for': 'python', 'branch': 'develop' }
    " }}}
    " filetype markdown {{{
    Plug 'godlygeek/tabular'
    Plug 'vimwiki/vimwiki'
    Plug 'suan/vim-instant-markdown', {'for': 'markdown'}
    ":GenTocGFM -> generate toc with github flavoured markdown
    Plug 'mzlogin/vim-markdown-toc'
    " }}}
    " filetype javascript {{{
    Plug 'pangloss/vim-javascript'
    " }}}
    " git integration {{{
    Plug 'tpope/vim-fugitive'
    " indicate added/modified/removed lines in a file
    Plug 'mhinz/vim-signify'
    " add some square bracket mappings
    Plug 'tpope/vim-unimpaired'
    " git commit browser
    Plug 'junegunn/gv.vim'
    " }}}
    " asthethics {{{
    Plug 'joshdick/onedark.vim'
    Plug 'itchyny/lightline.vim'
    Plug 'maximbaz/lightline-ale'
    " indentation line
    Plug 'yggdroot/indentline'
    Plug 'blueyed/vim-diminactive'
    " for better integration with diminactive
    Plug 'tmux-plugins/vim-tmux-focus-events'
    "  }}}
    Plug 'glts/vim-magnum'
    Plug 'glts/vim-radical'

    Plug 'svermeulen/vim-subversive'
    " for toggling between different variant
    Plug 'tpope/vim-abolish'
call plug#end()
"---------------------------------------------------------------------------}}}
" Colorscheme {{{
set t_Co=256
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

syntax on
let g:onedark_terminal_italics = 1
let g:onedark_hide_endofbuffer = 1

" customize individual aspects of onedark.vim's existing highlight groups
if (has("autocmd") && !has("gui_running"))
    augroup colorset
        autocmd!
        let s:colors = onedark#GetColors()
        let s:red = s:colors.red
        let s:white = s:colors.white
        autocmd ColorScheme * call onedark#extend_highlight("MatchParen", { "fg": s:white, "bg": s:red })
    augroup END
endif

colorscheme onedark
" }}}

" make a copy of the file and overwrite the original one. ie. inode of file
" remains unchanged otherwise links may be broken

set backupcopy=yes
