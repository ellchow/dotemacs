#!/usr/bin/env bash

## crude retry script
## USAGE:  ./retry.sh MAX_ATTEMPTS SLEEP_INTERVAL COMMAND

maxAttempts=$1
sleepInterval=$2
shift
shift

finished=0
attempt=1
code=1
this=$0
# logfile=${this%.*}".log"

while [ $finished -le 0 ]
do
    echo `date "+%Y-%m-%d %H:%M:%S"` "- INFO - retry.sh - attempt "$attempt >&2

    "$@"

    code=$?
    finished=`(test $code = 0 || test $attempt -ge $maxAttempts) && echo 1 || echo 0`
    test $finished = 0 && ((echo `date "+%Y-%m-%d %H:%M:%S"` "- INFO - retry.sh - waiting $sleepInterval before retrying" >&2) && sleep $sleepInterval)

    attempt=$(( $attempt + 1 ))
done

test $code = 0 && (echo `date "+%Y-%m-%d %H:%M:%S"` "- INFO - retry.sh - finished successfully" >&2) || (echo `date "+%Y-%m-%d %H:%M:%S"` "- ERROR - retry.sh - failed to execute command" >&2)

exit $code