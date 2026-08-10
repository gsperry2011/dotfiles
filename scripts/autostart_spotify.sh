#!/bin/bash

# # Ensure D-Bus environment is available
# if command -v dbus-update-activation-environment >/dev/null 2>&1; then
#     dbus-update-activation-environment --systemd DISPLAY XAUTHORITY
# fi

# Give the window manager a second to settle
# sleep 2

# Launch Spotify in the background (suppress output)
spotify > /dev/null 2>&1 &
