" Good Read
" https://github.com/romainl/idiomatic-vimrc
"  Settings {{{
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
    " Number of spaces during insert mode
    set softtabstop=4
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

    " make a copy of the file and overwrite the original one. ie. inode of file
    " remains unchanged otherwise links may be broken
    set backupcopy=yes
"}}}
" Custom Mapping {{{
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
    if has('nvim')
        nnoremap <c-space><space> <c-w>w
    else
        nnoremap <c-@><space> <c-w>w
    endif

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

    " wrap current line
    nnoremap <leader>j viw<ESC>mpa<CR><ESC>`p

    " bind CtrlF to search with Ag
    nnoremap <c-f> :Ag<space>

    " bind k to grep word under cursor
    nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

    " Quickly quit editing without save
    nnoremap <leader>q :q<CR>

    if has('nvim')
        nnoremap <leader>sv :source $MYVIMRC<CR>
        nnoremap <leader>ev :vsplit ~/.vimrc<cr>394G
    else
        nnoremap <leader>sv :source $MYVIMRC<CR>
        nnoremap <leader>ev :vsplit $MYVIMRC<cr>394G
    endif

    " Toggle between buffer
    nnoremap <leader><leader> :b#<cr>

    " Allow saving of files as sudo when I forgot to start vim using sudo.
    cmap w!! w !sudo tee > /dev/null %

    " Map Ctrl c & v for copy and paste from system clipboard
    vnoremap <c-c> "+y
    " inoremap <c-v> <esc>"+p
    " map leader c & v for copy and paste from buffer
    vnoremap <leader>c "*y
    nnoremap <leader>v "*p

    noremap <f2> :call g:ToggleBetweenRelativeAndNumber()<cr>
    " remove trailing spaces
    nnoremap <silent> <f5> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>

    " Easier buffer navigations
    nnoremap gb :ls<CR>:b<Space>

    " Map ctrl a to copy whole file
    nnoremap <c-a> gVG"+y

    if executable('ag')
        " Use ag over grep
        set grepprg=ag\ --nogroup\ --nocolor
    endif
    " Defines a new command Ag to search for the provided text and open a 'quickfix' window
    " bind \ (backward slash) to grep shortcut
    command! -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!

    " Custom Text Objects {{{

        " ie = inner entire buffer
        onoremap ie :exec "normal! ggVG"<cr>

        " iv = current viewable text in the buffer
        onoremap iv :exec "normal! HVL"<cr>

        " inside last parenthesis operator
        onoremap il( :<c-u>normal! F)vi(<cr>

    " }}}
" }}}
" Plugin Configurations {{{
    " nerdtree {{{
        map <C-n> :NERDTreeToggle<CR>
        " Show line numbers in NERDTree
        let NERDTreeShowLineNumbers=1
    " }}}
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

        noremap <leader>p :InstantMarkdownPreview<cr>
    " }}}
    " vim-signify {{{
        let g:signify_vcs_list = [ 'git' ]
        nnoremap <leader>hd :SignifyHunkDiff<cr>
        nnoremap <leader>hu :SignifyHunkUndo<cr>
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

        nnoremap <leader>tb :TagbarToggle<cr>
    "}}}
    " fzf {{{
        set rtp+=~/.fzf
        " search hidden file as well
        let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -l -g ""'
        nnoremap <c-p> :FZF<space><cr>
    "}}}
    " ale {{{
        " let g:ale_completion_tsserver_autoimport = 1
        " let g:ale_set_quickfix = 0
        let g:ale_set_highlights = 1

        " " Error message format
        " let g:ale_echo_msg_error_str = 'E'
        " let g:ale_echo_msg_warning_str = 'W'
        " let g:ale_echo_msg_format = '[%linter%] %code%: %s [%severity%]'
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

        " error navigation
        " nmap <silent> [g <Plug>(ale_previous_wrap)
        " nmap <silent> ]g <Plug>(ale_next_wrap)
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
            \   'filetype': 'MyFiletype',
            \   'fileformat': 'MyFileformat',
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

        nmap ,s <Plug>(easymotion-overwin-f2)

        " case insensitive
        " Smartsign (type `3` and match `3`&`#`)
        map  / <Plug>(easymotion-sn)
        omap / <Plug>(easymotion-tn)
        map  n <Plug>(easymotion-next)
        map  N <Plug>(easymotion-prev)

           " }}}
    " vim-subversive {{{
        nmap s <plug>(SubversiveSubstituteRangeConfirm)
        xmap s <plug>(SubversiveSubstituteRangeConfirm)
        nmap ss <plug>(SubversiveSubstituteWordRangeConfirm)
    " }}}
    " coc {{{
        if has('nvim')
            let g:coc_node_path='/home/artemis/.nvm/versions/node/v8.17.0/bin/node'
            let g:python3_host_prog = '/usr/bin/python'
            " You will have bad experience for diagnostic messages when it's default 4000.
            set updatetime=300

            " Use <c-space> to trigger completion.
            inoremap <silent><expr> <c-space> coc#refresh()

            " Use `[g` and `]g` to navigate diagnostics
            nmap <silent> <c-k> <Plug>(coc-diagnostic-prev)
            nmap <silent> <c-j> <Plug>(coc-diagnostic-next)

            " Remap keys for gotos
            nmap <silent> gd <Plug>(coc-definition)
            nmap <silent> gy <Plug>(coc-type-definition)
            nmap <silent> gi <Plug>(coc-implementation)
            nmap <silent> gr <Plug>(coc-references)

            " show_documentation function {{{
                function! s:show_documentation()
                    if (index(['vim','help'], &filetype) >= 0)
                        execute 'h '.expand('<cword>')
                    else
                        call CocAction('doHover')
                    endif
                endfunction
            " }}}
            " Use K to show documentation in preview window
            nnoremap <silent> K :call <SID>show_documentation()<CR>


            " Highlight symbol under cursor on CursorHold (use with coc-highlight)
            autocmd CursorHold * silent call CocActionAsync('highlight')

            " Remap for rename current word
            nmap <leader>rn <Plug>(coc-rename)

            " Remap for format selected region
            xmap <leader>f  <Plug>(coc-format-selected)
            nmap <leader>f  <Plug>(coc-format-selected)

            augroup mygroup
                autocmd!
                " Setup formatexpr specified filetype(s).
                autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
                " Update signature help on jump placeholder
                autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
            augroup end

            " Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
            xmap <leader>a  <Plug>(coc-codeaction-selected)
            nmap <leader>a  <Plug>(coc-codeaction-selected)

            " Remap for do codeAction of current line
            nmap <leader>ac  <Plug>(coc-codeaction)

            " Create mappings for function text object, requires document symbols feature of languageserver.
            xmap if <Plug>(coc-funcobj-i)
            xmap af <Plug>(coc-funcobj-a)
            omap if <Plug>(coc-funcobj-i)
            omap af <Plug>(coc-funcobj-a)

            " Use <C-d> for select selections ranges, needs server support, like: coc-tsserver, coc-python
            nmap <silent> <C-d> <Plug>(coc-range-select)
            xmap <silent> <C-d> <Plug>(coc-range-select)

            " Use `:Format` to format current buffer
            command! -nargs=0 Format :call CocAction('format')

            " Use `:Fold` to fold current buffer
            command! -nargs=? Fold :call     CocAction('fold', <f-args>)

            " use `:OR` for organize import of current buffer
            command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

            " Add status line support, for integration with other plugin, checkout `:h coc-status`
            set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

            " Using CocList
            " Show all diagnostics
            nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
            " Manage extensions
            nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
            " Show commands
            nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
            " Find symbol of current document
            nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
            " Search workspace symbols
            nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
            " Do default action for next item.
            nnoremap <silent> <space>j  :<C-u>CocNext<CR>
            " Do default action for previous item.
            nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
            " Resume latest coc list
            nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
        endif
    " }}}
    " vim-devicons {{{
        let g:DevIconsEnableFoldersOpenClose = 1
        " quickfix to prevent orange color folder
        highlight! link NERDTreeFlags NERDTreeDir

        function! MyFiletype()
            return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
        endfunction
        function! MyFileformat()
            return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
        endfunction
    " }}}
" }}}
"  Plugins {{{
call plug#begin()
    " essentials {{{
        Plug 'tpope/vim-surround'
        Plug 'kana/vim-repeat'
        Plug 'tpope/vim-commentary'
        Plug 'ervandew/supertab'
        Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
        Plug 'ronakg/quickr-preview.vim'
        Plug 'easymotion/vim-easymotion'
        " Plugin outside ~/.vim/plugged with post-update hook
        Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
        Plug 'junegunn/fzf.vim'

    " }}}
    " improved syntax highlighting
    Plug 'sheerun/vim-polyglot'

    " formatter
    Plug 'dense-analysis/ale'
    " autocompletion
    if has('nvim')
        " Use release branch (Recommend)
        Plug 'neoclide/coc.nvim', {'branch': 'release'}
    endif

    " filetype markdown {{{
        Plug 'godlygeek/tabular'
        Plug 'vimwiki/vimwiki'
        ":GenTocGFM -> generate toc with github flavoured markdown
        Plug 'mzlogin/vim-markdown-toc', { 'for': 'vimwiki' }
        Plug 'suan/vim-instant-markdown', { 'for': 'markdown' }
    " }}}
    " filetype javascript {{{
        Plug 'pangloss/vim-javascript', {'for': 'js'}

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
        Plug 'ryanoasis/vim-devicons'
    "  }}}
    " dependency for vim-radical
    Plug 'glts/vim-magnum'
    " convert between decimal, hex, octal and binary representation
    Plug 'glts/vim-radical'

    Plug 'svermeulen/vim-subversive'
    " for toggling between different variant
    Plug 'tpope/vim-abolish'

    Plug 'majutsushi/tagbar'
call plug#end()
"}}}
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

" Testing
