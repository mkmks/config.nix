export HISTCONTROL=ignoreboth
export HISTFILESIZE=100000
shopt -s histappend
shopt -s checkwinsize

export AWS_CALLING_FORMAT=SUBDOMAIN
export PASSWORD_STORE_ENABLE_EXTENSIONS=true

#export EDITOR="emacsclient -t"
alias e="emacsclient -t "
alias ec="emacsclient -c -n "

export SDCV_PAGER="less"
alias d="sdcv"

alias u="udiskie-umount"

alias scpresume="rsync --partial --progress --rsh=ssh"
#alias tmux="tmux attach || tmux new"
alias feh="feh -.d"

if [ -d "$XDF_RUNTIME_DIR/gnupg/" ]; then
#    export GPG_AGENT_INFO="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent"
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
    gpg-connect-agent -q updatestartuptty /bye
fi

if [[ $TERM == "st-256color" || $TERM == "dvtm-256color" ]]; then export TERM=xterm; fi
