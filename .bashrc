#### bash completion if available
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

#### load global bashrc if available
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

#### environment variables
export HISTTIMEFORMAT="%Y/%M/%d-%H:%M:%S "
export HISTSIZE=50000
export HISTFILESIZE=$HISTSIZE

if [ "$DOTEMACS_HOME" = "" ]
then
    export DOTEMACS_HOME=$HOME/dotemacs
fi
export PATH=$PATH:$DOTEMACS_HOME/bin

export TERM=xterm-256color
export TMPDIR="/tmp"
export PS1="\u@\h:\w$ " ## MAC Prompt

#### emacs
function emacs-daemon-pid() {
    ps -ef | \
        grep 'emacs --daemon' | \
        grep -v grep | \
        awk '{ print $2 }' | \
        head -1
}
function emacs-daemon-start() {
    pid=`emacs-daemon-pid`

    lock="$HOME/.emacs-daemon-start.lock"

    lockfile $lock

    if [ "$pid" = "" ]
    then
        echo "starting emacs daemon..."  >&2 && \
            emacs --daemon && echo "emacs daemon started (`emacs-daemon-pid`)" >&2
    else
        echo "emacs daemon already running ($pid)" >&2
    fi

    rm -f $lock
}
function emacs-daemon-kill() {
    pid=`emacs-daemon-pid`
    if [ "$pid" = "" ]
    then
        echo "emacs daemon is not running"  >&2
    else
        echo "killing emacs daemon ($pid)..."  >&2 && \
            kill "$@" $pid
    fi

}
function emacs-daemon-restart() {
    emacs-daemon-kill && \
        sleep 1 && \
        emacs-daemon-start
}
emacs-daemon-start

export EDITOR="emacs-stock"
export VISUAL="emacs-stock"
alias e='emacsclient -c'
# alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw' ## MAC


## java
export JAVA_OPTS='-Xmx2G'
export JAVA_TOOL_OPTIONS='-Djava.awt.headless=true'
# export CLASSPATH=
# export JAVA_HOME="$(/usr/libexec/java_home)" ## MAC java home, for java 6 >> export JAVA_HOME=/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home

#### misc aliases/functions
alias rerc='source ~/.bashrc'

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
if [ -f ~/.git-completion.sh ]
then
    source ~/.git-completion.sh
fi

####
echo "loaded .bashrc" >&2

bashrc_plus=$HOME/.bashrc.plus

if [ -f $bashrc_plus ]; then
  source $bashrc_plus
  echo "loaded "$bashrc_plus >&2
fi
