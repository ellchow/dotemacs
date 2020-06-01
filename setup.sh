#!/usr/bin/env bash

wd=`dirname $0`

cd $wd

git config user.name "Elliot Chow"
git config user.email chow.elliot@gmail.com

files='.emacs elisp .bashrc .bash_profile .git-completion.sh .pystartup .Rprofile .screenrc .gitconfig .tmux.conf'

for x in $files; do
    echo $x
    rm -r ~/$x
    cp -r $x ~/$x
done

touch .bashrc.plus

./elisp-compile.sh
mkdir -p ~/.emacs.d/snippets
cp -r yasnippets/* ~/.emacs.d/snippets/.
