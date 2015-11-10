#### bash completion if available
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

#### load global bashrc if available
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

#### environment variables
export PATH=$PATH:$HOME/git/dotemacs/bin

# export PYTHONSTARTUP=~/.pystartup
export TERM=xterm-256color
export TMPDIR="/tmp"
export PS1="\u@\h:\w$ " ## MAC Prompt

#### emacs
function emacs_daemon_pid() { ps -ef | grep 'emacs --daemon' | grep -v grep | awk '{ print $2 }'; }
function emacs_daemon_start() {
    pid=`emacs_daemon_pid`
    test "$pid" == "" && (echo "starting emacs daemon..." 2>&1 && emacs --daemon && echo "emacs daemon started (`emacs_daemon_pid`)" 2>&1) || \
        (echo "emacs daemon already running ($pid)" 2>&1)
}
function emacs_daemon_kill() {
    pid=`emacs_daemon_pid`
    test "$pid" == "" && (echo "emacs daemon is not running" 2>&1) || \
        (echo "killing emacs daemon ($pid)..." 2>&1 && kill $pid)

}
function emacs_daemon_restart() {
    emacs_daemon_kill
    emacs_daemon_start
}
emacs_daemon_start

export EDITOR="emacs-stock"
export VISUAL="emacs-stock"
alias e='emacsclient -c'
# alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw' ## MAC


## java
# export CLASSPATH=
# export JAVA_HOME="$(/usr/libexec/java_home)" ## MAC java home, for java 6 >> export JAVA_HOME=/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home
export JAVA_OPTS='-Xmx4G'
export JAVA_TOOL_OPTIONS='-Djava.awt.headless=true'

#### misc aliases/functions

# alias rm="rm -I"

function mbc(){ echo "scale=3;$@" | bc -l ; }

alias tmuxc='tmux -CC'
alias tmuxa='tmux -CC a'

alias gl='glances -1'

lines(){
    label=$1

    if [ ${#2} = 0 ]; then
        n=10
    else
        n=$2
    fi

awk '
BEGIN {
  t0 = systime()
  tprev = systime()
  nprev = 0
  timefmt = "%Y-%m-%d %H:%M:%S"
  label = "'$label'"
  print strftime(timefmt) " - lines " label " - START" > "/dev/stderr"
}
{
  print $0

  tcurr = systime()

  if ((tcurr - tprev) > '$n') {
    rcurr = (NR - nprev) / (tcurr - tprev + 1);
    r = NR / (tcurr - t0 + 1);
    print strftime(timefmt) " - lines " label " - read: " (NR - nprev) " (" rcurr "/sec)" " ; total read: " NR " (" r "/sec)" > "/dev/stderr"
    tprev = tcurr
    nprev = NR
  }
}
END {
  tcurr = systime()
  rcurr = (NR - nprev) / (tcurr - tprev + 1);
  r = NR / (tcurr - t0 + 1);

  print strftime(timefmt) " - lines " label " - read: " (NR - nprev) " (" rcurr "/sec)" " ; total read: " NR " (" r "/sec)" > "/dev/stderr"
  print strftime(timefmt) " - lines " label " - END" > "/dev/stderr"
}'
}

ppjson() {
python -c "
import sys, json

for ln in sys.stdin:
  print json.dumps(json.loads(ln),indent=2)
"
}

function zless() { gzip -dc "$@" | less ; }
function zcat() { gzip -dc "$@" ; }


abspath() { echo "$(cd "$(dirname "$1")" && pwd)/$(basename "$1")" ; }

#### git
source ~/.git-completion.sh

####
echo "loaded .bashrc" >&2

bashrc_plus=~/.bashrc.plus
if [ -f $bashrc_plus ]; then
  source $bashrc_plus
  echo "loaded "$bashrc_plus >&2
fi
