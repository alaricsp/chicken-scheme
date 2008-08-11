#!/bin/sh

export HENRIETTA=/home/chicken/henrietta
export EGG_REPOSITORY=https://localhost/svn/chicken-eggs/release/4
export LOGFILE=/home/chicken/henrietta.log

exec "$HENRIETTA" -l "$EGG_REPOSITORY" -t svn 2>>"$LOGFILE"
