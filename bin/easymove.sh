#! /bin/bash

selected=$(~/bin/easyxmotion.py)

[[ $selected ]] || exit 0

xdotool windowactivate $selected
