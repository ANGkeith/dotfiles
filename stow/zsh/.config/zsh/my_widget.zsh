# C-Z a toggle behaviour
fg-bg() {
    if [[ $#BUFFER -eq 0 ]]; then
        fg
    else
        zle push-input
    fi
}
jump-to-file-dir() {
    file_path="${LBUFFER}$(__fsel)"
    cd $(dirname $file_path)
    zle fzf-redraw-prompt
}
# cd.. Use fzf to cd into one of its parent/ancenstor dir
fzf-cd..() {
    local declare dirs=()
    get_parent_dirs() {
        if [[ -d "${1}" ]]; then dirs+=("$1"); else return; fi
        if [[ "${1}" == '/' ]]; then
            for _dir in "${dirs[@]}"; do echo $_dir; done
        else
            get_parent_dirs $(dirname "$1")
        fi
    }
    local DIR=$(get_parent_dirs $(realpath "${1:-$PWD}") | fzf-tmux --tac)
    cd "$DIR"
}

zle -N fzf-cd..
zle -N fg-bg                                                                    # shell function make into a widget
zle -N jump-to-file-dir
