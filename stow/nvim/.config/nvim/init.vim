set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

if empty($VIRTUAL_ENV)
    echo $VIRTUAL_ENV
    let g:python3_host_prog = '/usr/bin/python3.8'
endif
echo 123
