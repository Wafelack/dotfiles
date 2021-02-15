#!/usr/bin/env bash
set -euo pipefail

single_screen="📺 Single screen"
dual_screen="📺📺 Dual screen"
opts="$single_screen\n$dual_screen"

ch="$(echo -e "$opts" | rofi -dmenu)"

case $ch in
  $dual_screen)
    xrandr --output HDMI1 --off
    xrandr --output HDMI1 --auto --right-of eDP1
  ;;
  $single_screen)
    xrandr --output HDMI1 --off
  ;;
esac
