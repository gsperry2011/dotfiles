#!/bin/bash
# 1. Turn off the sim-only primary monitor first
xrandr --output DP-5 --off

# Pause briefly for the driver to release scanout pipelines
sleep 0.2

# 2. Restore DP-0 as primary (2560x1440) alongside DP-2 and HDMI-0
xrandr --output DP-0 --mode 2560x1440 --pos 1920x0 --rate 60.00 --primary \
       --output DP-2 --mode 1920x1080 --pos 0x0 --rate 60.00 \
       --output HDMI-0 --mode 1920x1080 --pos 4480x0 --rate 60.00

# 3. Adjust total framebuffer size for 1080p + 1440p + 1080p layout (6400x1440)
xrandr --fb 6400x1440
