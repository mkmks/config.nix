# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# add user's private bin to PATH if it exists
[ -d "$HOME/bin" ] && PATH="$HOME/bin:$PATH"

export TERM=xterm

if [ -n "$INSIDE_EMACS" ] ; then
    export TERM=ansi
fi

[ `uname` != "Darwin" ] && eval `keychain --eval id_rsa 3FAD8754`

#export JAVA_HOME="$(/usr/libexec/java_home)"
#export EC2_PRIVATE_KEY="$(/bin/ls "$HOME"/.ec2/pk-*.pem | /usr/bin/head -1)"
#export EC2_CERT="$(/bin/ls "$HOME"/.ec2/cert-*.pem | /usr/bin/head -1)"
#export EC2_AMITOOL_HOME="/usr/local/Library/LinkedKegs/ec2-ami-tools/jars"
#export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"

if [ -e /Users/viv/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/viv/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
