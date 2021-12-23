#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

hdmi=$(xrandr | awk '/HDMI.* connected/{printf "hdmi"}')
# Launch
if [ $hdmi = "hdmi" ]; then
   # polybar bottom-hdmi 2>&1 | tee -a /tmp/polybar2.log & disown
    polybar top-hdmi 2>&1 | tee -a /tmp/polybar2.log & disown
fi
#polybar bottom-edp1 2>&1 | tee -a /tmp/polybar2.log & disown
polybar top-edp1 2>&1 | tee -a /tmp/polybar2.log & disown


echo "Bar launched..."
