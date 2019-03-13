#!/usr/bin/env bash

# This script builds SARIT's existdb application from your current
# sources.

# for Debian, use JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre as env

if [ ! -d "${JAVA_HOME:?Need to set JAVA_HOME}" ]
then
    echo "Invalid JAVA_HOME dir set."
    exit 1
fi

if [ -d ./exist/ ] && [ -d ./sarit-pm/ ]
then
    STARTDIR=$(pwd)
else
    echo "You have to call this script from the root directory of your version of https://github.com/sarit/sarit-existdb.git."
    exit 1
fi


echo "Building things in $STARTDIR/exist ..."

mkdir -p "$STARTDIR"/blobs/ && cd "$STARTDIR"/blobs/
echo "Installing transcode library from sanskritlibrary.org ..."
[ ! -f transcodeFile.zip ] && echo "Downloading transcodeFile.zip" && wget http://sanskritlibrary.org/software/transcodeFile.zip
unzip -q -u transcodeFile.zip
mvn --batch-mode --quiet install:install-file -Dfile=./TranscodeFile/dist/lib/SanskritLibrary.jar -DgroupId=org.sanskritlibrary -DartifactId=sl -Dversion=0.1 -Dpackaging=jar
cd "$STARTDIR"

echo "Installing lucene-transcoding-analyzer ..."
cd ./lucene-transcoding-analyzer/ ||  exit 1
mvn --batch-mode --quiet clean install -DskipTests
cd "$STARTDIR"

echo "Building exist-db ..."
if [ -f ./exist.local.build.properties ]
then
    echo "Found local build properties, using them for eXist"
    cp ./exist.local.build.properties ./exist/local.build.properties
fi
cd ./exist/ ||  exit 1
./build.sh clean
./build.sh clean-all
./build.sh
cd "$STARTDIR"

echo "Building sarit-transliteration-exist-module ..."
cd ./sarit-transliteration-exist-module/ ||  exit 1
mvn --batch-mode --quiet clean package
cp ./target/sarit-transliteration-exist-module-0.0.8.xar ../exist/autodeploy/
cd "$STARTDIR"

echo "Starting existdb locally to build sarit-pm ..."
cd ./exist/ ||  exit 1
 ./bin/startup.sh &
EXPROC=$!
echo "Waiting for existdb to load completely ($EXPROC) ..."
sleep 5
grep -m 1 "Server has started, listening on" <(tail -f "$STARTDIR"/exist/webapp/WEB-INF/logs/exist.log)
cd "$STARTDIR"

echo "Building sarit-pm ..."
cd ./sarit-pm/ ||  exit 1
 ../exist/build.sh xar-all-inclusive
cp ./build/sarit-pm-0.3.xar ../exist/autodeploy/
cd "$STARTDIR"

cd ./exist/ || exit 1
echo "Shutting down existdb ($EXPROC) ..."
./bin/shutdown.sh
cd "$STARTDIR"

echo "Starting existdb to trigger autodeploy (#1) ..."
cd ./exist/ ||  exit 1
 ./bin/startup.sh &
EXPROC=$!
echo "Waiting for existdb to load completely ($EXPROC) ..."
sleep 5
grep -m 1 "Server has started, listening on" <(tail -f "$STARTDIR"/exist/webapp/WEB-INF/logs/exist.log)
echo "Shutting down existdb ($EXPROC) ..."
 ./bin/shutdown.sh
cd "$STARTDIR"

echo "Done, run ' $STARTDIR/exist/bin/startup.sh' to start the server."

cd "$STARTDIR"
