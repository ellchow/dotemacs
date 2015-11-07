#!/usr/bin/env bash

## brew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew install emacs
brew install watch
brew install gawk
brew install coreutils
brew install the_silver_searcher
brew install rename

brew tap caskroom/cask
brew install brew-cask
brew cask install java

brew install tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# sudo easy_install pip && pip install glances

# brew install webfs

# brew install sbt

# brew install docker
# brew install docker-machine
# brew cask install virtualbox
# docker-machine create --driver virtualbox default
