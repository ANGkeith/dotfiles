
if [[ $DISPLAY = :0 ]]; then
    # Keyboard, xset r rate <auto_repeat_delay> <repeat_rate>
    xset r rate 200 30
    xmodmap $XDG_CONFIG_HOME/Xmodmap
fi
