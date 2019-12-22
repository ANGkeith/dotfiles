" Good Read https://github.com/romainl/idiomatic-vimrc
"  Behaviour Modification {{{
    " Show line number
    set relativenumber
    set number

    " Open new split panes to right and bottom which feels more natural
    set splitbelow
    set splitright

    " Display all matcModificationhing files when we tab complete
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

    " highlight column based on ColorColumn highlight group
    let &colorcolumn=join(range(81, 999),',')

    set fillchars+=vert:│

    set textwidth=0

    " autoreload the file in Vim if it has been changed outside of Vim
    set autoread

    " Change cursor in the various different mode
    if has('nvim')
        set guicursor=n-c:block,i-ci-ve:ver40,r-cr-v:hor20,o:hor50,a:Cursor/lCursor,sm:block
    else
        let &t_SI = "\<Esc>]50;CursorShape=1\x7"
        let &t_SR = "\<Esc>]50;CursorShape=2\x7"
        let &t_EI = "\<Esc>]50;CursorShape=0\x7"
        if exists('$TMUX')
            let &t_SI = "\ePtmux;\e" . &t_SI . "\e\\"
            let &t_EI = "\ePtmux;\e" . &t_EI . "\e\\"
        endif
    endif

    " Always showing status line
    set laststatus=2

    set noswapfile
    set hidden

    " Should make scrolling faster
    set lazyredraw

       " Always use vertical diffs
     set diffopt+=vertical

    " make a copy of the file and overwrite the original one. ie. inode of file
    " remains unchanged otherwise links may be broken
    set backupcopy=yes

    " show live preview when doing commands like :substitute
    set inccommand=nosplit

    " Hide the vim mode message in the last line (Insert, Replace, Visual)
    set noshowmode
"}}}
" Custom Mapping {{{
    " note that <space> in normal mode is mapped to right, thus this is to override that settings
    nnoremap <space> <Nop>
    let mapleader="\<space>"

    " Map emacs binding in insert mode
    inoremap <c-a> <home>
    inoremap <c-e> <end>
    inoremap <c-k> <esc>ld$A

    " If a line gets wrapped to two lines, j wont skip over the '2nd line'
    nnoremap j gj
    nnoremap k gk

    " move to and highlight last edited text
    nnoremap gV `[v`]

    " Panes Management
    nnoremap <c-j> <c-w><c-j>
    nnoremap <c-k> <c-w><c-k>
    nnoremap <c-l> <c-w><c-l>
    nnoremap <c-h> <c-w><c-h>
    " mapping space to toggle pane
    nnoremap <leader><space> <c-w>w

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
    nnoremap \\ :b#<cr>

    " Allow saving of files as sudo when I forgot to start vim using sudo.
    cmap w!! w !sudo tee > /dev/null %

    " Map Ctrl c & v for copy and paste from system clipboard
    vnoremap <c-c> "+y
    " map leader c & v for copy and paste from buffer
    vnoremap <leader>c "*y
    nnoremap <leader>v "*p
    noremap <f2> :call g:ToggleBetweenRelativeAndNumber()<cr>
    " remove trailing spaces
    nnoremap <silent> <f5> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>
    " ToggleBetweenRelative* function {{{
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
    " }}}
    noremap <f1> :SignifyToggle<cr>:call g:ToggleBetweenRelativeAndNothing()<cr>:IndentLinesToggle<cr>
    noremap <f2> :call g:ToggleBetweenRelativeAndNumber()<cr>

    " My quickterm {{{
        " opens terminal in a window it behaves a loclist/quickfix list

        " map escape to exit terminal mode
        if exists(':tnoremap')
            tnoremap <Esc> <C-\><C-n>
        endif

        let g:quickterm_height = 15
        function! g:CreateQuicktermBuffer()
            silent! execute 'bot split'
            silent! execute 'terminal'
            silent! execute 'res ' . g:quickterm_height
            silent! execute 'setlocal nonu nornu'
            silent! execute 'IndentLinesDisable'
            silent! execute 'set winfixheight'
            silent! execute 'set winfixwidth'
            silent! execute 'set filetype=quickterm'
            let g:quickterm_buffer_name = expand('%:p')
        endfunction

        function! g:ToggleQuickTerm()
            if ! QuickTermIsOpen()
                if !exists("g:quickterm_buffer_name")
                    silent! execute 'call CreateQuicktermBuffer()'
                else
                    " Open buffer
                    silent! execute 'bot split'
                    silent! execute "buffer " . g:quickterm_buffer_name
                    silent! execute 'res ' . g:quickterm_height
                endif
            else
                " Hide Buffer
                " jump to quickterm buffer
                silent! execute "drop " . g:quickterm_buffer_name
                silent! execute 'hide'
            endif
        endfunction

        function! g:QuickTermIsOpen()
            if !exists("g:quickterm_buffer_name")
                return 0
            endif
            " detect if quickterm buffer is active
            if bufwinnr(g:quickterm_buffer_name) > 0
                return 1
            else
                return 0
            endif
        endfunction

        nnoremap <c-t> :call g:ToggleQuickTerm()<cr>
    " }}}

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

        noremap \p :InstantMarkdownPreview<cr>
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

        nnoremap \tb :TagbarToggle<cr>
    "}}}
    " fzf {{{
        set rtp+=~/.fzf
        " search hidden file as well
        let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -l -g ""'
        nnoremap <c-p> :FZF<space><cr>
    "}}}
    " ale {{{
        let g:ale_completion_tsserver_autoimport = 0
        let g:ale_set_quickfix = 0
        let g:ale_set_highlights = 1

        " " Error message format
        let g:ale_echo_msg_error_str = 'E'
        let g:ale_echo_msg_warning_str = 'W'
        let g:ale_echo_msg_format = '[%linter%] %code%: %s [%severity%]'
        " Run :ALEFix upon save
        let g:ale_fix_on_save = 1
        " general ALE config
        let g:ale_fixers = {'*': ['remove_trailing_lines']}
        " python ALE configurations
        let g:ale_fixers = {'python': ['isort', 'autopep8', 'black']}
        let g:ale_linters = {'python': ['pylint']}
        let g:ale_python_autopep8_options = "-i"
        let g:ale_python_black_options = "-l 80"
        let g:ale_python_mypy_options = "--ignore-missing-imports --disallow-untyped-defs"
        let g:ale_python_flake8_options = "--max-line-length=80"

        " error navigation
        " nmap <silent> <c-j> <Plug>(ale_previous_wrap)
        " nmap <silent> <c-k> <Plug>(ale_next_wrap)
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
            \ 'inactive': {
            \   'left': [ [ 'horizontal_seperator' ] ],
            \   'right': [],
            \ },
            \ 'component_function': {
            \   'gitbranch': 'fugitive#head',
            \   'filetype': 'DeviconFileType',
            \   'fileformat': 'DeviconFileFormat',
            \   'cocstatus': 'coc#status',
            \   'readonly': 'LightlineReadonly',
            \   'horizontal_seperator': 'HorizontalSeperator',
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

        " helper function {{{
            function! DeviconFileType()
                return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
            endfunction
            function! DeviconFileFormat()
                return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
            endfunction

            function! HorizontalSeperator()
                let l:fillChar = '─'
                let l:fileName = ' ' . expand('%:t') . ' '
                let l:fileNameLength = len(fileName)
                let l:totalCharsNeeded = winwidth(0) - 2
                let l:charsToFill =  totalCharsNeeded - fileNameLength

                if l:fileNameLength
                    let l:left_part = repeat(fillChar, l:charsToFill/2)
                    let l:middle_part = fileName
                    let l:right_part = repeat(fillChar, l:charsToFill/2)

                    " if odd
                    if charsToFill % 2
                        let l:right_part = right_part . fillChar
                    endif
                else
                    return repeat(fillChar, winwidth(0) - 2)
                endif

                return l:left_part . l:middle_part . l:right_part
            endfunction

            autocmd VimEnter * call SetupLightlineColors()
                function SetupLightlineColors() abort
                let l:pallete = lightline#palette()
                let l:pallete.inactive.left = [
                \ ['#5fafaf', '#282c34', '145', '236'],
                \ ['#5fafaf', '#282c34', '145', '236']
                \]
                call lightline#colorscheme()
            endfunction
        "  }}}
    " }}}
    " vim-diminactive {{{
        let g:diminactive_enable_focus = 1
        " to make dimainactive have effect on the listed file type
        let g:diminactive_filetype_whitelist = [ 'help', 'quickterm' ]
    " }}}
    " vim-easymotion {{{
        " Disable default mappings
        let g:EasyMotion_do_mapping = 0
        " case insensitive
        let g:EasyMotion_smartcase = 1
        " Smartsign (type `3` and match `3`&`#`)
        let g:EasyMotion_use_smartsign_us = 1

        nmap <leader>s <Plug>(easymotion-overwin-f2)

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
    " supertab {{{
        let g:SuperTabDefaultCompletionType = "<c-n>"
    " }}}
    " coc {{{
        let g:coc_node_path = expand("$HOME") .  '/.nvm/versions/node/v8.17.0/bin/node'
        let g:python3_host_prog = '/usr/bin/python'

        " You will have bad experience for diagnostic messages when it's default 4000.
        set updatetime=300

        " show_documentation function {{{
            function! s:show_documentation()
                if (index(['vim','help'], &filetype) >= 0)
                    execute 'h '.expand('<cword>')
                else
                    call CocAction('doHover')
                endif
            endfunction
        " }}}
        " Highlight symbol under cursor on CursorHold (use with coc-highlight)
        autocmd CursorHold * silent call CocActionAsync('highlight')

        augroup mygroup
            autocmd!
            " Setup formatexpr specified filetype(s).
            autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
            " Update signature help on jump placeholder
            autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
        augroup end


        " Create mappings for function text object, requires document symbols feature of languageserver.
        xmap if <Plug>(coc-funcobj-i)
        xmap af <Plug>(coc-funcobj-a)
        omap if <Plug>(coc-funcobj-i)
        omap af <Plug>(coc-funcobj-a)

        " Use `:Format` to format current buffer
        command! -nargs=0 Format :call CocAction('format')

        " Use `:Fold` to fold current buffer
        command! -nargs=? Fold :call     CocAction('fold', <f-args>)

        " use `:OR` for organize import of current buffer
        command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

       " Use K to show documentation in preview window
        nnoremap <silent> K :call <SID>show_documentation()<CR>

        " Remap keys for gotos
        nmap <silent> gd <Plug>(coc-definition)
        nmap <silent> gy <Plug>(coc-type-definition)
        nmap <silent> gi <Plug>(coc-implementation)
        nmap <silent> gr <Plug>(coc-references)
        " Use <c-space> to trigger completion.
        inoremap <silent><expr> <c-space> coc#refresh()


        " Remap for rename current word
        nmap \rn <Plug>(coc-rename)

        " Remap for format selected region
        xmap \f  <Plug>(coc-format-selected)
        nmap \f  <Plug>(coc-format-selected)

        " Use `[g` and `]g` to navigate diagnostics
        nmap <silent> [g <Plug>(coc-diagnostic-prev)
        nmap <silent> ]g <Plug>(coc-diagnostic-next)

        " Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
        xmap \a  <Plug>(coc-codeaction-selected)
        nmap \a  <Plug>(coc-codeaction-selected)

        " Remap for do codeAction of current line
        nmap \ac  <Plug>(coc-codeaction)

        " Using CocList
        " Show all diagnostics
        nnoremap <silent> \a  :<C-u>CocList diagnostics<cr>
        " Manage extensions
        nnoremap <silent> \e  :<C-u>CocList extensions<cr>
        " Show commands
        nnoremap <silent> \c  :<C-u>CocList commands<cr>
        " Find symbol of current document
        nnoremap <silent> \o  :<C-u>CocList outline<cr>
        " Search workspace symbols
        nnoremap <silent> \s  :<C-u>CocList -I symbols<cr>
        " Do default action for next item.
        nnoremap <silent> \j  :<C-u>CocNext<CR>
        " Do default action for previous item.
        nnoremap <silent> \k  :<C-u>CocPrev<CR>
        " Resume latest coc list
        nnoremap <silent> \p  :<C-u>CocListResume<CR>
    " }}}
    " vim-devicons {{{
        let g:DevIconsEnableFoldersOpenClose = 1
        " quickfix to prevent orange color folder
        highlight! link NERDTreeFlags NERDTreeDir
    " }}}
    " vim-maximizer {{{
        nnoremap <c-f> :MaximizerToggle<CR>
        vnoremap <c-f> :MaximizerToggle<CR>gv
        inoremap <c-f> <C-o>:MaximizerToggle<CR>
    "
    " }}}
    " vim-polyglot {{{
        " polylglot clashes with vimwiki plugin
        let g:polyglot_disabled = ['markdown']
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
        " add some square bracket mappings
        Plug 'tpope/vim-unimpaired'
    " }}}
    " IDE {{{
        " improved syntax highlighting
        Plug 'sheerun/vim-polyglot'
        " Used mainly for fixing and linting
        Plug 'dense-analysis/ale'
        " Mainly for autocompletion
        Plug 'neoclide/coc.nvim', {'branch': 'release'}
    " }}}
    " filetype markdown {{{
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
        " git commit browser
        Plug 'junegunn/gv.vim'
    " }}}
    " asthethics {{{
        Plug 'joshdick/onedark.vim'
        Plug 'itchyny/lightline.vim'
        Plug 'maximbaz/lightline-ale'
        " indentation line
        Plug 'yggdroot/indentline'
        " vim-diminactive hack for vim8 {{{
            if 1 && exists('+winhighlight')
                function! s:configure_winhighlight()
                let ft = &filetype
                let bt = &buftype
                " Check white/blacklist.
                if &diff || (index(['dirvish'], ft) == -1
                        \ && (index(['nofile', 'nowrite', 'acwrite', 'quickfix', 'help'], bt) != -1
                        \     || index(['startify'], ft) != -1))
                    set winhighlight=NormalNC:MyNormalWin
                else
                    set winhighlight=NormalNC:MyInactiveWin
                endif
                endfunction
                augroup inactive_win
                au!
                au ColorScheme * hi link MyInactiveWin ColorColumn
                au FileType,BufWinEnter * call s:configure_winhighlight()
                if exists('##OptionSet')
                    " TODO: does not work with :Gdiff - diffsplit does not trigger it, too!
                    au OptionSet diff call s:configure_winhighlight()
                endif
                augroup END
            else
                Plug 'blueyed/vim-diminactive'
                let g:diminactive_enable_focus = 1
                " let g:diminactive_debug = 1
            endif
        " }}}
        " for better integration with diminactive
        Plug 'tmux-plugins/vim-tmux-focus-events'
        Plug 'ryanoasis/vim-devicons'
        Plug 'machakann/vim-highlightedyank'
    "  }}}
    "  bloat {{{
        " dependency for vim-radical
        Plug 'glts/vim-magnum'
        " convert between decimal, hex, octal and binary representation
        Plug 'glts/vim-radical'

        Plug 'svermeulen/vim-subversive'
        " for toggling between different variant
        Plug 'tpope/vim-abolish'
    "  }}}
    " utils {{{
        " full screen
        Plug 'szw/vim-maximizer'

        Plug 'godlygeek/tabular'
    " }}}
    Plug 'majutsushi/tagbar'

call plug#end()
"}}}
" Colorscheme {{{
    set t_Co=256
    " True color support
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
            autocmd ColorScheme * call onedark#set_highlight("CocErrorSign", { "fg": s:colors.red })
            autocmd ColorScheme * call onedark#set_highlight("CocErrorFloat", { "fg": { "cterm": 204, "gui": "#eb4034" } })
            autocmd ColorScheme * call onedark#set_highlight("CocWarningSign", { "fg": s:colors.red })
            autocmd ColorScheme * call onedark#set_highlight("CocWarningLine", { "fg": { "cterm": 204, "gui": "#eb4034" } })
        augroup END
    endif
    colorscheme onedark

    " Change the filler background color after the end of the buffer
    hi! link EndOfBuffer ColorColumn
    hi VertSplit guifg=#5fafaf
" }}}

