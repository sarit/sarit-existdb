#!/usr/bin/env bash

# This script builds SARIT's existdb modules on a travis instance.

if [ -d .git ] && git remote -v | grep -q -i "sarit-existdb.git"
then
    GITREMOTE="$(git rev-parse --show-toplevel)"
else
    echo "You have to call this script from the root directory of your version of https://github.com/sarit/sarit-existdb.git into."
    exit 1
fi

ROOTDIR=$GITREMOTE
STARTDIR=$(pwd)

echo "Getting and installing transcode library from sanskritlibrary.org ..."
mkdir -p "$ROOTDIR"/blobs/ && cd "$ROOTDIR"/blobs/
[ ! -f transcodeFile.zip ] && wget --quiet http://sanskritlibrary.org/software/transcodeFile.zip
unzip -d ./ -j transcodeFile.zip TranscodeFile/dist/lib/SanskritLibrary.jar
mvn install:install-file -Dfile=./SanskritLibrary.jar -DgroupId=org.sanskritlibrary -DartifactId=sl -Dversion=0.1 -Dpackaging=jar
cd "$ROOTDIR"


echo "Installing lucene-transcoding-analyzer ..."
cd ./lucene-transcoding-analyzer/ ||  exit 1
mvn clean install -DskipTests
cd "$ROOTDIR"

echo "Building sarit-transliteration-exist-module ..."
cd ./sarit-transliteration-exist-module/ ||  exit 1
mvn clean package
cd "$ROOTDIR"

echo "Building sarit-data ..."
cd ./sarit-data/ ||  exit 1
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ../exist/build.sh
cd "$ROOTDIR"

echo "Building sarit-pm ..."
cd ./sarit-pm/ ||  exit 1
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ../exist/build.sh
cd "$ROOTDIR"

cd "$STARTDIR"

