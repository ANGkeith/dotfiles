" Enable folding
setlocal foldmethod=indent
setlocal foldlevel=99
setlocal encoding=utf-8
setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal autoindent


" execute a command and show its output in a split window
command! -nargs=* -complete=shellcmd Rsplit execute "new | r! <args>"

nnoremap <leader>r :!python3 % &> /tmp/my-vim-scratch<cr><cr>:split /tmp/my-vim-scratch<cr><cr> :set noma<cr><cr>
