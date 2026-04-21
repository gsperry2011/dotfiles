#!/bin/bash
# Paths
RCLONE_CONFIG="/home/abrax/.config/rclone/rclone.conf"
export RCLONE_CONFIG
LOCAL_DIR="/home/abrax/googledrive/"
REMOTE_NAME="rclone-xxxsirkillalot:"

# Safety: Exit if another instance is already running
if [[ "`pidof -x $(basename $0) -o %PPID`" ]]; then exit; fi

# Bidirectional sync
# --resilient: allows it to continue on minor errors
# --drive-skip-gdocs: avoids errors with native Google docs
/usr/bin/rclone bisync "$LOCAL_DIR" "$REMOTE_NAME" \
    --resilient \
    --drive-skip-gdocs \
    --verbose

