#!/bin/bash

# YYYY-MM-DD HH:MM:SS
date '+%Y-%m-%d %H:%M:%S'

# Handle the click
if [[ -n "$BLOCK_BUTTON" ]]; then
    i3-msg -q exec gsimplecal
fi
