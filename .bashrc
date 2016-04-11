export HISTCONTROL=ignoreboth
export HISTFILESIZE=100000
shopt -s histappend
shopt -s checkwinsize

export EDITOR="emacsclient -t"

alias scpresume="rsync --partial --progress --rsh=ssh"
alias tmux="tmux attach || tmux new"
alias e="emacsclient -t "
alias ec="emacsclient -c -n "

if [ -f "${HOME}/.gpg-agent-info" ]; then
    . "${HOME}/.gpg-agent-info"
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
fi

export AWS_CALLING_FORMAT=SUBDOMAIN

if [ $TERM == "st-256color" ]; then export TERM=xterm; fi
