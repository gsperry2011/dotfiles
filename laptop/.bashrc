# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
export TERM=xterm-256color
export COLORTERM=urxvt256c
export EDITOR='emacs -nw'
#alias emacs='emacs -nw'
alias emacs='TERM=xterm-256color emacs'
alias powershell='~/git/bash_scripts/powershell_lib_curl.sh'

#############
# Powerline #
#############
powerline-daemon -q
export POWERLINE_BASH_CONTINUATION=1
export POWERLINE_BASH_SELECT=1
. /usr/lib/python2.7/site-packages/powerline/bindings/bash/powerline.sh

#############
#   Emacs   #
#############

# In centos7 I need to run this for my solarized color scheme to work otherwise after reboots I get:
# Undefined color: "S_base0"
xrdb -load ~/.Xresources -cpp /usr/bin/cpp
