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
echo 0 | sudo tee /sys/module/hid_apple/parameters/fnmode
flameshot

my_file1=~/.cache/my-scratchpad/scratchpad_emacsclient1
[ -f "$my_file1" ] && xdo close "$(cat $my_file1)"; rm "$my_file1"

my_file2=~/.cache/my-scratchpad/scratchpad_emacsclient_org
[ -f "$my_file2" ] && xdo close "$(cat $my_file2)"; rm "$my_file2"


