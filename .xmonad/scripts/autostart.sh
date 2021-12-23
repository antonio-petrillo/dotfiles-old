#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

# set screen layout
~/dotfiles/script/screen-setup.sh
#cursor active at boot
xsetroot -cursor_name left_ptr &

dunst -conf $HOME/.config/dunst/dunstrc &
xmodmap $HOME/dotfiles/script/.Xmodmap
xfce4-power-manager &
picom &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
udiskie &
$HOME/dotfiles/script/screen-setup.sh
# stalonetray -c $HOME/.config/stalonetray/main_stalonetrayrc &
# stalonetray -c $HOME/.config/stalonetray/secondary_stalonetrayrc &
$HOME/.config/polybar/launch.sh

nm-applet &
volumeicon &
blueman-applet &

#starting user applications at boot time
nitrogen --restore &
emacs --daemon &
