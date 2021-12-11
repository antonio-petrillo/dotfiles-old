#!/bin/sh

hdmi=$(xrandr | awk '/HDMI.* connected/{printf "hdmi"}')
if [ $hdmi = "hdmi" ]; then
    xrandr --output eDP --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-A-0 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DisplayPort-0 --off --output DisplayPort-1 --off
else
    xrandr --output eDP --primary --mode 1920x1080 --pos 0x0
    xrandr --output HDMI-A-0 --off
fi
