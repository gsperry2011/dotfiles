#!/bin/sh

#Greg custom

#the sources need to be pre exec command
#[ -f /etc/xprofile ] && . /etc.xprofile
#[ -f ~/.xprofile ] && . ~/.xprofile
[ -f $HOME/.Xresources ] && xrdb -load $HOME/.Xresources -cpp /usr/bin/cpp

exec i3
