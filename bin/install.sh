#!/usr/bin/env bash

# This script installs SARIT's existdb application from your current
# sources.

if [ -d .git ] && git remote -v | grep -q -i "sarit-existdb.git"
then
    GITREMOTE="$(git rev-parse --show-toplevel)"
else
    echo "You have to call this script from the directory you cloned https://github.com/sarit/sarit-existdb.git into."
    exit 1
fi

ROOTDIR=$GITREMOTE
STARTDIR=$(pwd)

echo "Installing to $ROOTDIR/exist ..."

# echo "Cloning sarit webapp from $GITREMOTE to $ROOTDIR ..."
# git clone --recursive "$GITREMOTE" "$ROOTDIR"
# cd "$ROOTDIR"
# git submodule init
# git submodule update --depth 1

mkdir -p "$ROOTDIR"/blobs/ && cd "$ROOTDIR"/blobs/
echo "Getting and installing transcode library from sanskritlibrary.org ..."
[ ! -f transcodeFile.zip ] && wget --quiet http://sanskritlibrary.org/software/transcodeFile.zip
unzip -q -u transcodeFile.zip
mvn install:install-file -Dfile=./TranscodeFile/dist/lib/SanskritLibrary.jar -DgroupId=org.sanskritlibrary -DartifactId=sl -Dversion=0.1 -Dpackaging=jar
cd "$ROOTDIR"

echo "Installing lucene-transcoding-analyzer ..."
cd ./lucene-transcoding-analyzer/ ||  exit 1
mvn clean install -DskipTests
cd "$ROOTDIR"

echo "Building exist-db ..."
cd ./exist/ ||  exit 1
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ./build.sh clean
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ./build.sh
cd "$ROOTDIR"

echo "Building sarit-transliteration-exist-module ..."
cd ./sarit-transliteration-exist-module/ ||  exit 1
mvn clean package
cp ./target/sarit-transliteration-exist-module-0.0.8.xar ../exist/autodeploy/
cd "$ROOTDIR"

echo "Updating sarit-data collection  ..."
cd ./sarit-data/data/ ||  exit 1
for xml in $(xmlstarlet sel -N xi='http://www.w3.org/2001/XInclude' -T -t -m "//xi:include[@href]" -v "concat(\"$ROOTDIR/SARIT-corpus/\", ./@href)" -n   "$ROOTDIR"/SARIT-corpus/saritcorpus.xml)
do
    echo "Updating $xml ..."
    cp "$xml" ./
done
cd "$ROOTDIR"

echo "Building sarit-data ..."
cd ./sarit-data/ ||  exit 1
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ../exist/build.sh
## don’t autodeploy yet --> depends on sarit-pm having been installed for index ?
# cp ./build/sarit-data-0.1.xar ../exist/autodeploy/sarit-data-0.1.xar
cd "$ROOTDIR"

echo "Building sarit-pm ..."
cd ./sarit-pm/ ||  exit 1
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ../exist/build.sh
cp ./build/sarit-pm-0.2.xar ../exist/autodeploy/sarit-pm-0.2.xar
cd "$ROOTDIR"

echo "Fetching necessary modules ..."
cd ./exist/autodeploy/ ||  exit 1
wget http://demo.exist-db.org/exist/apps/public-repo/public/tei-publisher-lib-2.0.3.xar \
     http://exist-db.org/exist/apps/public-repo/public/expath-pdf-exist-lib-0.0.4.xar \
     http://demo.exist-db.org/exist/apps/public-repo/public/tei-pm-1.1.2.xar
cd "$ROOTDIR"

echo "Starting existdb to trigger autodeploy (#1) ..."
cd ./exist/ ||  exit 1
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre java -jar start.jar jetty &
EXPROC=$!
echo "Wait for existdb ($EXPROC) ..."
sleep 5
grep -m 1 "Server has started, listening on" <(tail -f "$ROOTDIR"/exist/webapp/WEB-INF/logs/exist.log)
echo "Shut down existdb ($EXPROC) ..."
kill -SIGTERM $EXPROC
cd "$ROOTDIR"

echo "Deploying sarit-data ..."
cd ./sarit-data/ ||  exit 1
# autodeploy
cp ./build/sarit-data-0.1.xar ../exist/autodeploy/sarit-data-0.1.xar
cd "$ROOTDIR"

echo "Starting existdb to trigger autodeploy of sarit-data, be patient #2 ..."
cd ./exist/
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre java -jar start.jar jetty &
EXPROC=$!
echo "Waiting for completion of existdb ..."
sleep 5
grep -m 1 "Server has started, listening on" <(tail -f "$ROOTDIR"/exist/webapp/WEB-INF/logs/exist.log)
echo "Shut down existdb ($EXPROC) ..."
kill -SIGTERM $EXPROC

echo "Done, run 'JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre $ROOTDIR/exist/bin/startup.sh' to start the server."

cd "$STARTDIR"

