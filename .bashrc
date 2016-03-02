export HISTCONTROL=ignoreboth
export HISTFILESIZE=100000
shopt -s histappend
shopt -s checkwinsize

export EDITOR="emacsclient -t"

alias scpresume="rsync --partial --progress --rsh=ssh"
alias tmux="tmux attach || tmux new"
alias e="emacsclient -t "
alias ec="emacsclient -c -n "

export AWS_CALLING_FORMAT=SUBDOMAIN
