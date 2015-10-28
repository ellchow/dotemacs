#!/usr/bin/env bash

wd=`dirname $0`

cd $wd

files='.emacs elisp .bashrc .bash_profile .git-completion.sh .pystartup .Rprofile .screenrc .gitconfig .tmux.conf'

for x in $files; do
    echo $x
    rm -r ~/$x
    cp -r $x ~/$x
done

touch .bashrc.plus
