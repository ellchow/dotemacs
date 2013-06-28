
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
export EDITOR=emacs
export PYTHONSTARTUP=~/.pystartup
export TERM=xterm-256color
export TMPDIR="/tmp"

## java
# export CLASSPATH=
# export JAVA_HOME=

#### misc aliases/functions
alias emacs='emacs -nw'
# alias rm="rm -I"
function mbc(){ echo "scale=3;$@" | bc -l ; }

#### tsv stuff

cutbyname(){
    if [ ${#2} = 0 ]; then
        sep="\t"
    else
        sep=$2
    fi
    awkwh '
BEGIN{
  FS="'$sep'"
  OFS="\t"
  split("'$1'", keep, ",")
}
NR == 1{
  for(k=1;k <= length(keep);k+=1){
    if(!(keep[k] in IDX) || (keep[k] in ks)){
      delete keep[k]
    }else{
       ks[keep[k]] = 1
    }
  }
}
{
  row = ""
  for(k=1;k <= length(keep);k+=1){
    if(k in keep)
      row = row OFS $IDX[keep[k]]
  }
  print row
}
' | sed s/'\t\(.*\)'/'\1'/g
}

header(){
    if [ ${#1} = 0 ]; then
        sep="\t"
    else
        sep=$1
    fi
    head -n1 | awk '
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

awkwh(){
    awk "
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
awk '
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