while true; do
  xsetroot -name "$(dwm_status)"
  sleep 5s
done &
exec dwm
