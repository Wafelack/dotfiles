#!/usr/bin/env bash

ten="10%"
twenty="20%"
thirty="30%"
forty="40%"
fifty="50%"
sixty="60%"
seventy="70%"
eighty="80%"
ninety="90%"
hundred="100%"

opts="$ten\n$twenty\n$thirty\n$forty\n$fifty\n$sixty\n$seventy\n$eighty\n$ninety\n$hundred"

ch=$(echo -e $opts | rofi -dmenu)

set_brightness() {

  xrandr --output HDMI1 --brightness $1 2> /dev/null # Will fail if HDMI1 is not plugged in but we don't care at all.
  xrandr --output eDP1 --brightness $1
}

case $ch in 
  $ten)
    set_brightness 0.1
    ;;
  $twenty)
    set_brightness 0.2
    ;;
  $thirty)
    set_brightness 0.3
    ;;
  $forty)
    set_brightness 0.4
    ;;
  $fifty)
    set_brightness 0.5
    ;;
  $sixty)
    set_brightness 0.6
    ;;
  $seventy)
    set_brightness 0.7
    ;;
  $eighty)
    set_brightness 0.8
    ;;
  $ninety)
    set_brightness 0.9
    ;;
  $hundred)
    set_brightness 1.0
    ;;
esac
