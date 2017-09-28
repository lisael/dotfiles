#! /bin/bash

orig=$(xdotool getactivewindow)

if [ -z $orig ]; then
    exit 1
fi

selected=$(~/bin/easyxmotion.py)

if [ -z $selected ]; then
    exit 0
fi

xdotool windowactivate $selected
xdotool key super+v
sleep 0.1
xdotool windowactivate $orig
