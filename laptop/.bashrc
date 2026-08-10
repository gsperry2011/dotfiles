# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
#export TERM=xterm-256color
#export COLORTERM=urxvt256c
export EDITOR='emacs -nw'

# opencode
export PATH="$HOME/.opencode/bin:$PATH"
#alias emacs='emacs -nw'
#alias emacs='TERM=xterm-256color emacs'
# Old PS core version
#alias powershell='~/git/bash_scripts/powershell_lib_curl.sh'




