#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch polybar
polybar main &
sleep 0.4

# need to run this after launching bar otherwise the format tag cannot be parsed
~/.config/polybar/scripts/xrandr update
