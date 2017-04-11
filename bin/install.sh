#!/usr/bin/env bash

ROOTDIR=$(tempfile -d --prefix sarit)
STARTDIR=$(pwd)

echo "Installing to $ROOTDIR ..."

cd "$ROOTDIR"

echo "Cloning sarit webapp ..."
git clone --recursive https://github.com/sarit/sarit-exist-webapp.git
cd ./sarit-exist-webapp/
echo "Get transcode library from sanskritlibrary.org ..."
wget http://sanskritlibrary.org/software/transcodeFile.zip
echo "Building exist-db ..."
cd ./exist/
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ./build.sh clean
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ./build.sh
cd "$ROOTDIR"

echo "Installing transcoding library ..."
[ ! -f transcodeFile.zip ] && wget http://sanskritlibrary.org/software/transcodeFile.zip
unzip transcodeFile.zip
mvn install:install-file -Dfile=./TranscodeFile/dist/lib/SanskritLibrary.jar -DgroupId=org.sanskritlibrary -DartifactId=sl -Dversion=0.1 -Dpackaging=jar
echo "Installing transcoding analyzer ..."
cd ./lucene-transcoding-analyzer/
mvn clean install -DskipTests
echo "Building sarit-transliteration-exist-module ..."
cd ../sarit-transliteration-exist-module/
mvn clean package
cd "$ROOTDIR"

echo "Building sarit-data ..."
cd ./sarit-data/
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ../exist/build.sh
cd "$ROOTDIR"

echo "Building sarit-pm ..."
cd ./sarit-pm/
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ../exist/build.sh
cd "$ROOTDIR"

echo "All done, please use the dashboard to install the modules:"

cd "$STARTDIR"
