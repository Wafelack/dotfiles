#!/usr/bin/env bash
set -euo pipefail

current_layout=$(echo $(setxkbmap -query | grep layout) | sed "s/layout://")
echo "'$current_layout'"
if [[ $current_layout == " fr-latin9" ]]
then
  setxkbmap us
else
  setxkbmap fr-latin9
fi
