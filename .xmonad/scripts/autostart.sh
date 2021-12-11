#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#change your keyboard if you need it
#setxkbmap -layout be

# set screen layout
# ~/dotfiles/script/screen-setup.sh 
#cursor active at boot
xsetroot -cursor_name left_ptr &

run nm-applet &
run pamac-tray &
run xfce4-power-manager &
run volumeicon &
numlockx on &
blueberry-tray &
picom --config $HOME/.xmonad/scripts/picom.conf &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &
udiskie &

#starting user applications at boot time
nitrogen --restore &
emacs --daemon 
trayer --edge top --align right --widthtype request --padding 8 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x23272e  --height 22 &
yakuake &
play-with-mpv &
