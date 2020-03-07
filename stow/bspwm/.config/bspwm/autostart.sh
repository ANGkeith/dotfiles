#!/bin/bash
kill_and_rerun() {
    pid=$(pgrep "$1")
    if [[ -n $pid ]]; then
        kill -9 "$pid"
    fi
    "$@"&
}

kill_and_rerun picom -b

kill_and_rerun dropbox &> /dev/null

"$HOME"/.config/polybar/launch.sh

kill_and_rerun sxhkd -c ~/.config/sxhkd/sxhkdrc
kill_and_rerun feh --no-fehbg --bg-scale ~/.config/wallpaper/wallpaper.jpg
# cannot use sudo as the autostart.sh is run without sudo priviledge
# echo 0 | sudo tee /sys/module/hid_apple/parameters/fnmode
flameshot

if [[ "$DISPLAY" = :0 ]]; then
    # Keyboard, xset r rate <auto_repeat_delay> <repeat_rate>
    xset r rate 200 30
    setxkbmap -option shift:both_capslock
    setxkbmap -option caps:swapescape
fi
