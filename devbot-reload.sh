#!/usr/bin/env zsh
while true
do
    ghc devbot.hs -o devbot && ./devbot
done
