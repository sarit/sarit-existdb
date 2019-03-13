#!/usr/bin/env bash

# This script builds SARIT's existdb application from your current
# sources.

# for Debian, use JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre as env

if [ ! -d "${JAVA_HOME:?Need to set JAVA_HOME}" ]
then
    echo "Invalid JAVA_HOME dir set."
    exit 1
fi

if [ -d ./docker-existdb/ ] && [ -d ./sarit-pm/ ]
then
    STARTDIR=$(pwd)
else
    echo "You have to call this script from the root directory of your version of https://github.com/sarit/sarit-existdb.git."
    exit 1
fi

EXISTDB_VERSION="${1}"

if [ -z ${EXISTDB_VERSION} ]
then
	echo "You must specify a version of eXistdb to build!"
	echo ""
	exit 1
fi

echo "Building java libs in $STARTDIR ..."

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

mkdir -p "$STARTDIR"/exist-autodeploy/

echo "Building sarit-transliteration-exist-module ..."
cd ./sarit-transliteration-exist-module/ ||  exit 1
mvn --batch-mode --quiet clean package
cp ./target/sarit-transliteration-exist-module-0.0.8.xar ../exist-autodeploy/
cd "$STARTDIR"

# build existdb version with docker

cd ./docker-existdb/
if [ ! -d ./target/exist ]
then
    mkdir ./target && \
        cd ./target && \
        git clone https://github.com/exist-db/exist.git && \
        cd ../
fi

# build sarit-pm
cd "$STARTDIR"/sarit-pm
../docker-existdb/target/exist/build.sh
cp ./build/sarit-pm-0.4.xar ../exist-autodeploy/

cd "$STARTDIR"/docker-existdb
mkdir -p ./target/exist/autodeploy
cp -R $STARTDIR/exist-autodeploy/*.xar ./target/exist/autodeploy/

# now, build existdb
./build.sh $EXISTDB_VERSION    
cd "$STARTDIR"





# That should give us a “sarit/exist-db:VERSION” to run
echo "See how it went with “docker run --rm --name local-sarit-existdb -it -p 9080:8080 -p 9443:8443 sarit/exist-db:$EXISTDB_VERSION”"

# echo "Starting existdb locally to build sarit-pm ..."
# docker run -it -p 9080:8080 -p 9443:8443 sarit/exist-db:"$EXISTDB_VERSION" &
# EXPROC=$!
# echo "Waiting for existdb to load completely ($EXPROC) ..."
# sleep 5
# grep -m 1 "Server has started, listening on" <(tail -f "$STARTDIR"/exist/webapp/WEB-INF/logs/exist.log)
# cd "$STARTDIR"

# echo "Building sarit-pm ..."
# cd ./sarit-pm/ ||  exit 1
#  ../exist/build.sh xar-all-inclusive
# cp ./build/sarit-pm-0.4.xar ../exist/autodeploy/
# cd "$STARTDIR"

# cd ./exist/ || exit 1
# echo "Shutting down existdb ($EXPROC) ..."
# ./bin/shutdown.sh
# cd "$STARTDIR"

# echo "Starting existdb to trigger autodeploy (#1) ..."
# cd ./exist/ ||  exit 1
#  ./bin/startup.sh &
# EXPROC=$!
# echo "Waiting for existdb to load completely ($EXPROC) ..."
# sleep 5
# grep -m 1 "Server has started, listening on" <(tail -f "$STARTDIR"/exist/webapp/WEB-INF/logs/exist.log)
# echo "Shutting down existdb ($EXPROC) ..."
#  ./bin/shutdown.sh
# cd "$STARTDIR"

# echo "Done, run ' $STARTDIR/exist/bin/startup.sh' to start the server."

cd "$STARTDIR"
