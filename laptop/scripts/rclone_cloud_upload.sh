#!/bin/bash

RCLONE_CONFIG=/home/greg/.config/rclone/rclone.conf
export RCLONE_CONFIG
LOCAL_DIR="/home/greg/googledrive/"
REMOTE_NAME="google-drive:"

#exit if running
if [[ "`pidof -x $(basename $0) -o %PPID`" ]]; then exit; fi

# Bidirectional sync
# --resilient: allows it to continue on minor errors
# --drive-skip-gdocs: avoids errors with native Google docs
/usr/bin/rclone bisync "$LOCAL_DIR" "$REMOTE_NAME" \
    --resilient \
    --drive-skip-gdocs \
    --verbose

