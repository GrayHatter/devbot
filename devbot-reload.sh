#!/usr/bin/env zsh
while true
do
    ghc src/devbot.hs -o devbot && ./devbot || sleep 10
done
