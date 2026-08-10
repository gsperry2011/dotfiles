#!/bin/bash
# 1. Turn off active displays first to clear hardware CRTC assignments
xrandr --output DP-0 --off

# Give the GPU driver a fraction of a second to release the scanout buffers
sleep 0.2

# 2. Configure active displays individually
xrandr --output DP-5 --mode 1920x1080 --pos 1920x0 --rate 60.00 --primary \
       --output DP-2 --mode 1920x1080 --pos 0x0 --rate 60.00 \
       --output HDMI-0 --mode 1920x1080 --pos 3840x0 --rate 60.00

# 3. Finalize total bounding box size
xrandr --fb 5760x1080
