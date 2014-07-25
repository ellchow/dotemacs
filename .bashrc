#### bash completion if available
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

#### load global bashrc if available
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

#### environment variables
# export PATH=$PATH
export EDITOR='nano'
export PYTHONSTARTUP=~/.pystartup
export TERM=xterm-256color
export TMPDIR="/tmp"
export PS1="\u@\h:\w$ " ## MAC Prompt

## java
# export CLASSPATH=
export JAVA_HOME="$(/usr/libexec/java_home)" ## MAC java home, for java 6 >> export JAVA_HOME=/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Homeexport JAVA_TOOL_OPTIONS='-Djava.awt.headless=true'

#### misc aliases/functions
# alias emacs='emacs -nw'
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw' ## MAC
alias julia='/Applications/Julia-0.2.1.app/Contents/Resources/julia/bin/julia'
# alias rm="rm -I"
function mbc(){ echo "scale=3;$@" | bc -l ; }

#### tsv stuff

header(){
    if [ ${#1} = 0 ]; then
        sep="\t"
    else
        sep=$1
    fi
    head -n1 | gawk '
BEGIN{
RS="'$sep'"
OFS="\n"
}
{ print(NR "\t" $0) }
'
}

onbody() {
    IFS= read -r header
    printf '%s\n' "$header"
    if [ ${#1} = 0 ]; then
        cat
    else
        "$@"
    fi
}

gawkwh(){
    gawk "
NR==1{
  for(i=1;i<=NF;i+=1){
    IDX[\$i] = i
  }
}
""$@"
}

lines_read(){
    if [ ${#1} = 0 ]; then
        n=10
    else
        n=$1
    fi

    tee >(
gawk '
BEGIN{
  x=10
  n='$n'
  step=n
  m=n*x
  print "START " strftime("%c")
}
{
  if(NR<m && NR==n){
    print "lines read: " NR
    n=n+step
  }
  if(NR == m){
    print "lines read: " NR " " strftime("%c")
    step = x*step
    n=n+step
    m = x*m
  }
}
END{
  print "total lines read: " NR
  print "END " strftime("%c")
}' >&2 )
}

ppjson() {
python -c "
import sys, json
for ln in sys.stdin: print json.dumps(json.loads(ln),indent=2)
"
}

function zless() { gzip -dc "$@" | less ; }
function zcat() { gzip -dc "$@" ; }


#### hadoop stuff

function hls() { hadoop fs -ls "$@" ; }
function hlsr() { hadoop fs -lsr "$@" ; }
function hcat() { hadoop fs -cat "$@" ; }
function hrm() { hadoop fs -rm "$@" ; }
function hrmr() { hadoop fs -rmr "$@" ; }
function hrmrst() { hadoop fs -rmr -skipTrash "$@" ; }
function hmkdir() { hadoop fs -mkdir "$@" ; }

function hmv() { hadoop fs -mv "$@" ; }
function hcp() { hadoop fs -cp "$@" ; }
function htxt() { hadoop fs -text "$@" ; }
function hput() { hadoop fs -put "$@" ; }


function hdus() { hadoop fs -dus "$@" | perl -p -e 's/(.*?)(\d+)$//; $f=$1; $s=$2; $s= ($s/(1024*1024*1024*1024) > 1) ? sprintf("%.2f TB", $s/(1024*1024*1024*1024)) : (($s/(1024*1024*1024) > 1) ? sprintf("%.2f GB", $s/(1024*1024*1024)): (($s/(1024*1024) > 1) ? sprintf("%.2f MB", $s/(1024*1024)) : (($s/1024 > 1) ? sprintf("%.2f KB", $s/1024) : $s) ) ); printf("%12s  %s", $s, $f);'; }
function hdussort() { hadoop fs -dus "$@" | perl -pi -e 's/(.*?)(\d+)$/$2 $1/' | sort -nr | perl -p -e 's/(\d+)(.+)$//; $f=$2; $s=$1; $s= ($s/(1024*1024*1024*1024) > 1) ? sprintf("%.2f TB", $s/(1024*1024*1024*1024)) : (($s/(1024*1024*1024) > 1) ? sprintf("%.2f GB", $s/(1024*1024*1024)): (($s/(1024*1024) > 1) ? sprintf("%.2f MB", $s/(1024*1024)) : (($s/1024 > 1) ? sprintf("%.2f KB", $s/1024) : $s) ) ); printf("%12s  %s", $s, $f);'; }


#### git
source ~/.git-completion.sh

####
echo "loaded .bashrc" >&2

bashrc_plus=~/.bashrc.plus
if [ -f $bashrc_plus ]; then
  source $bashrc_plus
  echo "loaded "$bashrc_plus >&2
fi
