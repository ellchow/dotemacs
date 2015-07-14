function hls() { hadoop fs -ls "$@" ; }
function hlsr() { hadoop fs -lsr "$@" ; }
function hcat() { hadoop fs -cat "$@" ; }
function hrm() { hadoop fs -rm "$@" ; }
function hrmr() { hadoop fs -rm -r "$@" ; }
function hrmrst() { hadoop fs -rm -r -skipTrash "$@" ; }
function hmkdir() { hadoop fs -mkdir "$@" ; }
function hmv() { hadoop fs -mv "$@" ; }
function hcp() { hadoop fs -cp "$@" ; }
function htxt() { hadoop fs -text "$@" ; }
function hput() { hadoop fs -put "$@" ; }
function hdus() { hadoop fs -du -s "$@" | awk '
BEGIN {
  u[0] = "B"
  u[1] = "K"
  u[2] = "M"
  u[3] = "G"
  u[4] = "T"
}
{
    n = length($1)
    p = int(n / 3)
    p = (p > 4 ? 4 : p)
    x = $1
    for (i = 0; i < p; i += 1) {
       x = x / 1000
    }
    print $1 , x "" u[p] , $2
}
' ; }
