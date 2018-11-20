xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation" 1
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Button" 2
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Axes" 7 6 5 4 
xset m 5 2

emacs --daemon &
redshift -l 57:11 &
xidle &
exec i3
emacsclient -e '(save-buffers-kill-emacs)'
gpgconf --kill gpg-agent
