#!/bin/bash

curl -s 'wttr.in?format=j1' > /tmp/weather.json 

current_temp=$(jq -r '.current_condition[0].temp_F' /tmp/weather.json)
feels_like=$(jq -r '.current_condition[0].FeelsLikeF' /tmp/weather.json)
high_temp=$(jq -r '.weather[0].maxtempF' /tmp/weather.json)
low_temp=$(jq -r '.weather[0].mintempF' /tmp/weather.json)

# Define colors (pango markup)
COLOR_HIGH="#FF5555" # Red-ish
COLOR_LOW="#50FA7B"  # Green-ish

# Line 3: Color coding logic
if [ "$current_temp" -le 32 ]; then
    COLOR_NOW="#87CEEB" # Freezing (Sky Blue)
elif [ "$current_temp" -le 60 ]; then
    COLOR_NOW="#50FA7B" # Chilly (Green)
elif [ "$current_temp" -le 80 ]; then
    COLOR_NOW="#F1FA8C" # Pleasant (Yellow)
elif [ "$current_temp" -le 95 ]; then
    COLOR_NOW="#FFB86C" # Warm (Orange)
else
    COLOR_NOW="#FF5555" # Hot (Red)
fi

# Line 1: Full text displayed on the bar
echo "<span color='$COLOR_NOW'>${current_temp}°F</span> (Feels:<span color='$COLOR_NOW'>${feels_like}</span>) | H:<span color='$COLOR_HIGH'>${high_temp}</span> L:<span color='$COLOR_LOW'>${low_temp}</span> "

# Line 2: Short text (used if bar space is limited)
echo "${current_temp}°F"

# Left click gives 3 day forecast from wttr, right click gives radar
case $BLOCK_BUTTON in
    1) 
       alacritty -t "Weather Forecast" -e bash -c "curl -s wttr.in; read -n 1 -s" &
       disown 
       ;;
    3) 
       xdg-open "https://radar.weather.gov/station/kiln/standard" & 
       disown 
       ;;
esac
