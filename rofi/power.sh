#!/usr/bin/env bash
set -euo pipefail

reboot="↺ Reboot"
shutdown="⏻ Shutdown"
log_out="[↤ Log out"

opts="$shutdown\n$reboot\n$log_out"

ch="$(echo -e "$opts" | rofi -dmenu)"

case $ch in
  $reboot)
    sudo reboot now
  ;;
  $shutdown)
    sudo shutdown now
  ;;
  $log_out)
    i3-msg exit
  ;;
esac
