#!/bin/bash

# Use wpctl for native PipeWire/WirePlumber interaction
# Get volume and mute status in one go
VOLUME_INFO=$(wpctl get-volume @DEFAULT_AUDIO_SINK@)

# Check for mute status
if echo "$VOLUME_INFO" | grep -q "\[MUTED\]"; then
    echo "   MUTED"
    echo "  "
    echo "#ff0000" # Red color when muted
else
    # Extract percentage (e.g., 0.50 -> 50%)
    VOL=$(echo "$VOLUME_INFO" | awk '{print int($2 * 100)}')
    echo "   $VOL%"
    echo "   $VOL%"
    echo "#ffffff"
fi

# Case for mouse clicks (optional)
case $BLOCK_BUTTON in
    1) wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle ;; # Left click: toggle mute
    3) pavucontrol & disown ;;
    4) wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+ ;; # Scroll up: +5%
    5) wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%- ;; # Scroll down: -5%
esac
