#!/usr/bin/env bash
set -euo pipefail

# Keyboard stuff
setxkbmap -option caps:escape
xmodmap -e "keysym Shift_R = Multi_key"

# Satus line
memory() {
    free | awk '/Mem/ { printf "%d/%d MiB\n", $3 / 1024.0, $2 / 1024.0 }'
}
battery() {
    POWER_ENDPOINT="/sys/class/power_supply"
    PLUGGED=$(cat ${POWER_ENDPOINT}/AC/online)
    if [[ $PLUGGED -eq 1 ]]
    then
        PLUGGED="+"
    else
        PLUGGED="-"
    fi
    LEVEL=$(cat ${POWER_ENDPOINT}/BAT0/capacity)
    echo "${LEVEL}% [${PLUGGED}]"
}
cpu() {
    cat /proc/loadavg  | awk -F' ' '{ while ( ("nproc" | getline procs) > 0) { printf "%d", $1 * 100. / procs } close("nproc") }'
}

while true; do
  xsetroot -name "CPU: $(cpu)% | Memory: $(memory) | $(battery) | $(date +"%a, %b %d - %H:%M")"
  sleep 5
done&
