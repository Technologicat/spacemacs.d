#!/usr/bin/env bash
# https://medium.com/@bobbypriambodo/blazingly-fast-spacemacs-with-persistent-server-92260f2118b7
# https://superuser.com/questions/142945/bash-command-to-focus-a-specific-window

# Check if there's an Emacs frame open
emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" 2> /dev/null | grep t &> /dev/null

if [ "$?" -eq "1" ]; then
    emacsclient -a '' -nqc "$@" &> /dev/null
else
    emacsclient -nq "$@" &> /dev/null
fi

# Focus the Emacs window
wmctrl -a emacs
