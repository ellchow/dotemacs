#!/usr/bin/env bash

emacs --batch --eval '(byte-recompile-directory "'$HOME'/elisp" 0)'
