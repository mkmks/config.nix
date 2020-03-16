include /home/viv/dotfiles/i3/config.common

bindsym $mod+q exec xterm
bindsym $mod+f exec emacseditor

bindsym $mod+z exec "i3lock -c ff0000"
bindsym $mod+x exec "dmenu_run -fn monospace-8"

bindsym $mod+Shift+z exec "i3-nagbar -t warning -m 'Exit?' -b 'Yes, exit i3' 'i3-msg exit'"

# app-specific

for_window [class="Chromium-browser"] border pixel 0
for_window [class="Firefox"] border pixel 0

exec --no-startup-id i3-msg "workspace mmm; append_layout /home/viv/dotfiles/i3/workspace-mmm.json; exec telegram-desktop; exec skypeforlinux; exec new-mu4e-frame.sh"
