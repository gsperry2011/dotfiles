#!/bin/bash

RCLONE_CONFIG=/home/greg/.config/rclone/rclone.conf
export RCLONE_CONFIG

#exit if running
if [[ "`pidof -x $(basename $0) -o %PPID`" ]]; then exit; fi

# Push local files to google drive
/usr/bin/rclone copy -u /home/greg/googledrive/ google-drive:

# Pull files from google drive to local
/usr/bin/rclone copy -u google-drive: /home/greg/googledrive/

