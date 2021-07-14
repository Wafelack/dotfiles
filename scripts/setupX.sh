#!/usr/bin/env bash
setxkbmap -option caps:escape
xmodmap -e "keysym Shift_R = Multi_key"

while true; do
  xsetroot -name "$(dwm_status)"
  sleep 5
done&
