export HISTCONTROL=ignoreboth
export HISTFILESIZE=100000
shopt -s histappend
shopt -s checkwinsize

#export EDITOR="emacsclient -t"

alias scpresume="rsync --partial --progress --rsh=ssh"
alias tmux="tmux attach || tmux new"
alias e="emacsclient -t "
alias ec="emacsclient -c -n "
alias feh="feh -.d"

if [ -d "$XDF_RUNTIME_DIR/gnupg/" ]; then
#    export GPG_AGENT_INFO="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent"
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
    gpg-connect-agent -q updatestartuptty /bye
fi

export AWS_CALLING_FORMAT=SUBDOMAIN

if [[ $TERM == "st-256color" || $TERM == "dvtm-256color" ]]; then export TERM=xterm; fi
