#!/usr/bin/env bash
set -euo pipefail

fr="FR - Français, France."
us="US - English, United States."
opts="$fr\n$us"

ch="$(echo -e "$opts" | rofi -dmenu)"

case $ch in
  $fr)
    setxkbmap fr
    ;;
  $us)
    setxkbmap us
    ;;
esac
