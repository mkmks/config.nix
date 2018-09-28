set fish_greeting

function fish_title
    echo $USER'@'(cat /etc/hostname)':'$PWD'$' $argv[1]
end


alias e "emacsclient -t"
alias ec "emacsclient -c -n"
alias v ebook-viewer
alias u udiskie-umount
alias feh "feh -.d"

set -x ALTERNATIVE_EDITOR mcedit

#if [ $TERM = "st-256color" -o $TERM = "dvtm-256color" -o $TERM = "xterm-termite" ]
#   set -x TERM xterm
#end

if [ -d "$XDG_RUNTIME_DIR/gnupg/" ]
    set -x SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
    gpg-connect-agent -q updatestartuptty /bye > /dev/null
end
