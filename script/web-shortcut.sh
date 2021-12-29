#!/usr/bin/env bash

web_browser="google-chrome-stable"
entry=$(awk -F# '{print $1}' web-shortcut-input | dmenu -l 8)

if [ $? -eq 0 ]; then
    url=$(awk -F# -v key="$entry" '{if($1 == key){print $2}}' web-shortcut-input)
    "$web_browser" "$url" & disown
fi
