set fish_greeting

function fish_prompt
    ~/.local/bin/powerline-shell --shell bare $status
end


alias e "emacsclient -t"
alias ec "emacsclient -c -n"
alias v ebook-viewer
alias u udiskie-umount
alias feh "feh -.d"

if [ $TERM = "st-256color" -o $TERM = "dvtm-256color" ]
   set -x TERM xterm
end

set -x ALTERNATIVE_EDITOR mcedit