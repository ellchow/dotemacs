#!/usr/bin/env bash

wd=`dirname $0`

cd ~/

ln -s -v $wd/.emacs .emacs
ln -s -v $wd/elisp elisp
ln -s -v $wd/.bashrc .bashrc
ln -s -v $wd/.bash_profile .bash_profile
ln -s -v $wd/.profile .profile
ln -s -v $wd/.git-completion.sh .git-completion.sh
ln -s -v $wd/.pystartup .pystartup