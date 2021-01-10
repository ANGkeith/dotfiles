# Directories
setopt auto_cd                # perform cd to directory without the need of preceeding cd

# History
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_reduce_blanks     # removes superfluous blanks from each commands
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # HISTFILE is updated as soon as entered rather than waiting until the shell exits

# Input/Output
setopt correct_all            # Try to correct spelling of all arg
setopt interactivecomments    # In interactive cli, it recognizes comments


