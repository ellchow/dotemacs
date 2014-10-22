#!/usr/bin/env bash

pid_file=".pid"
if [[ $1 == pid:* ]] ;
then
    pid_file=`sed s/pid://g <( echo $1 )`
    shift
fi

nohup $@ 2>&1 &
echo $! > $pid_file
cat $pid_file
