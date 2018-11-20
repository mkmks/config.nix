set fish_greeting

function fish_title
    echo $USER'@'(hostname)':'$PWD'$' $argv[1]
end

alias ec "emacsclient -n"
alias v ebook-viewer
alias u udiskie-umount
alias feh "feh -.d"

set -x EDITOR "emacsclient -cn"
set -x ALTERNATIVE_EDITOR "mg -n"
set -x SDCV_PAGER "less -R"

set -x CVSROOT "anoncvs@anoncvs.eu.openbsd.org:/cvs"


gpg-connect-agent -q updatestartuptty /bye > /dev/null

