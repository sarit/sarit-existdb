#!/usr/bin/env bash

# Start up exist, and then shutdown: useful to prepare collection in
# docker image (indexes on creation of image, not afterwards).

java -jar start.jar jetty &
EXPROC=$!
echo "Waiting for completion of existdb ..."
sleep 5
grep -m 1 "Server has started, listening on" <(tail -f ./webapp/WEB-INF/logs/exist.log)
sleep 10
echo "Shut down existdb ($EXPROC) ..."
kill -SIGTERM $EXPROC
