#!/usr/bin/env bash
setxkbmap -option caps:escape
xmodmap -e "keysym Shift_R = Multik_key"

while true; do
  xsetroot -name "$(dwm_status)"
  sleep 5
done&
