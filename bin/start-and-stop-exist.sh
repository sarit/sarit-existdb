#!/usr/bin/env bash

# Start up exist, and then shutdown: useful to prepare collection in
# docker image (indexes on creation of image, not afterwards).

# # keep this in sync with ./build.sh
# if [ -d .git ] && git remote -v | grep -q -i "sarit-existdb.git"
# then
#     GITREMOTE="$(git rev-parse --show-toplevel)"
# else
#     echo "You have to call this script from the root directory of your version of https://github.com/sarit/sarit-existdb.git."
#     exit 1
# fi

# ROOTDIR="$GITREMOTE"
# STARTDIR=$(pwd)

# cd "$ROOTDIR"/exist

mkdir -p ./webapp/WEB-INF/logs/
touch ./webapp/WEB-INF/logs/exist.log
java -jar start.jar jetty &
EXPROC=$!
echo "Waiting for completion of existdb ..."
grep -m 1 "Server has started, listening on" <(tail -f ./webapp/WEB-INF/logs/exist.log)
sleep 10
echo "Shut down existdb ($EXPROC) ..."
./bin/shutdown.sh

# cd "$STARTDIR"
