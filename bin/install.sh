#!/usr/bin/env bash

ROOTDIR=$(mktemp -d -t sarit-existdb-XXXX)
STARTDIR=$(pwd)

echo "Installing to $ROOTDIR ..."

if [ -d .git ] && git remote -v | grep -q -i "sarit-existdb.git"
then
    GITREMOTE="file://$(git rev-parse --show-toplevel)"
else
    GITREMOTE="https://github.com/sarit/sarit-existdb.git"
fi

echo "Cloning sarit webapp from $GITREMOTE to $ROOTDIR ..."
git clone --recursive --shallow-submodules --depth 1 "$GITREMOTE" "$ROOTDIR"
cd "$ROOTDIR"

echo "Getting and installing transcode library from sanskritlibrary.org ..."
[ ! -f transcodeFile.zip ] && wget http://sanskritlibrary.org/software/transcodeFile.zip
unzip transcodeFile.zip
mvn install:install-file -Dfile=./TranscodeFile/dist/lib/SanskritLibrary.jar -DgroupId=org.sanskritlibrary -DartifactId=sl -Dversion=0.1 -Dpackaging=jarn
cd "$ROOTDIR"

echo "Installing lucene-transcoding-analyzer ..."
cd ./lucene-transcoding-analyzer/
mvn clean install -DskipTests
cd "$ROOTDIR"

echo "Building exist-db ..."
cd ./exist/
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ./build.sh clean
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ./build.sh
cd "$ROOTDIR"

echo "Building sarit-transliteration-exist-module ..."
cd ./sarit-transliteration-exist-module/
mvn clean package
cp ./target/sarit-transliteration-exist-module-0.0.8.xar ../exist/autodeploy/
cd "$ROOTDIR"

echo "Updating sarit-data collection  ..."
cd ./sarit-data/data/
for xml in $(xmlstarlet sel -N xi='http://www.w3.org/2001/XInclude' -T -t -m "//xi:include[@href]" -v "concat(\"$ROOTDIR/SARIT-corpus/\", ./@href)" -n   "$ROOTDIR"/SARIT-corpus/saritcorpus.xml)
do
    echo "Updating $xml ..."
    cp "$xml" ./
done
cd "$ROOTDIR"

echo "Building sarit-data ..."
cd ./sarit-data/
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ../exist/build.sh
## don’t autodeploy yet --> depends on sarit-pm having been installed for index ?
# cp ./build/sarit-data-0.1.xar ../exist/autodeploy/sarit-data-0.1.xar
cd "$ROOTDIR"

echo "Building sarit-pm ..."
cd ./sarit-pm/
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre ../exist/build.sh
cp ./build/sarit-pm-0.2.xar ../exist/autodeploy/sarit-pm-0.2.xar
cd "$ROOTDIR"

echo "Fetching necessary modules ..."
cd ./exist/autodeploy/
wget http://demo.exist-db.org/exist/apps/public-repo/public/tei-publisher-lib-2.0.3.xar \
     http://exist-db.org/exist/apps/public-repo/public/expath-pdf-exist-lib-0.0.4.xar \
     http://demo.exist-db.org/exist/apps/public-repo/public/tei-pm-1.1.2.xar
cd "$ROOTDIR"

echo "Starting existdb to trigger autodeploy (#1) ..."
cd ./exist/
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre java -jar start.jar jetty &
EXPROC=$!
echo "Wait for existdb ..."
sleep 30
echo "Shut down existdb ($EXPROC) ..."
kill -SIGTERM $EXPROC
sleep 30
cd "$ROOTDIR"

echo "Deploying sarit-data ..."
cd ./sarit-data/
# autodeploy
cp ./build/sarit-data-0.1.xar ../exist/autodeploy/sarit-data-0.1.xar
cd "$ROOTDIR"

echo "Starting existdb to trigger autodeploy of sarit-data, be patient #2 ..."
cd ./exist/
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre java -jar start.jar jetty


cd "$STARTDIR"

