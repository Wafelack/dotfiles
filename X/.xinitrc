setkxmap -option caps:escape
xmodmap -e "keysym Shift_R = Multi_key"
exec dbus-launch stumpwm
