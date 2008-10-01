#!/bin/sh

export HENRIETTA=/home/chicken/henrietta
export EGG_REPOSITORY=https://localhost/svn/chicken-eggs/release/4
export LOGFILE=/home/chicken/henrietta.log
export USERNAME=anonymous
export PASSWORD=

exec "$HENRIETTA" -l "$EGG_REPOSITORY" -t svn -username "$USERNAME" \
  -password "$PASSWORD" 2>>"$LOGFILE"
