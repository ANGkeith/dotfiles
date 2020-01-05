#!/bin/bash
kill_and_rerun() {
    pid=$(pgrep $1)
    if [[ ! -z $pid ]]; then
        kill -9 $pid
    fi
    $@&
}

kill_and_rerun picom -b
$HOME/.config/polybar/launch.sh

kill_and_rerun sxhkd -c ~/.config/sxhkd/sxhkdrc
kill_and_rerun feh --no-fehbg --bg-scale ~/.config/wallpaper/wallpaper.jpg
echo 0 | sudo tee /sys/module/hid_apple/parameters/fnmode
flameshot


sleep 1
xmodmap ~/.config/Xmodmap
