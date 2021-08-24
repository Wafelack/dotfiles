$HOME/.local/bin/xstartup.sh
xrdb -load $HOME/.Xresources
exec dbus-launch dwm
