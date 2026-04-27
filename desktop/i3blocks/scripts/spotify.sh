#!/bin/bash

# $arist - $title $current_position / $total_duration
playerctl -p spotify metadata --format "{{ artist }} - {{ title }} {{ duration(position) }} / {{ duration(mpris:length) }}" 2>/dev/null || echo "Spotify Offline"

# Handle the click
if [[ -n "$BLOCK_BUTTON" ]]; then
    i3-msg '[class="Spotify"] scratchpad show, fullscreen enable' >/dev/null
fi
