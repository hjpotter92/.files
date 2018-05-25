#!/usr/bin/env zsh

# Add this script to `crontab` to run periodically
DISPLAY=:0 /usr/bin/notify-send "$(date +"Time is %r")"
