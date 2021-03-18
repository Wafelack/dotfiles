#!/usr/bin/env bash
set -euo pipefail

fr="FR - Français, France."
us="US - English, United States."
latin9="LAT9 - Français (Latin 9 seulement), France"
opts="$fr\n$us\n$latin9"

ch="$(echo -e "$opts" | rofi -dmenu)"

case $ch in
  $fr)
    setxkbmap fr
    ;;
  $us)
    setxkbmap us
    ;;
  $latin9)
    setxkbmap fr-latin9
    ;;
esac
