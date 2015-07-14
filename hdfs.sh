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
function hdus() { hadoop fs -du -s "$@" }
