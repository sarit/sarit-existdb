FROM openjdk:8-jdk
LABEL source="https://github.com/sarit/sarit-existdb" 


# requirements to build SARIT’s eXistdb instance
RUN apt-get update && \
    apt-get -y --no-install-recommends install ant && \
    apt-get clean

# compile the main exist stuff
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/exist/
# add exist sources
ADD ./exist/ ./
# add the transcoder
ADD http://sanskritlibrary.org/software/transcodeFile.zip /tmp
RUN unzip -d ./lib/user/ -j /tmp/transcodeFile.zip TranscodeFile/dist/lib/SanskritLibrary.jar && \
    rm /tmp/transcodeFile.zip
# add the lucene module
ADD ./lucene-transcoding-analyzer/target/lucene-transcoding-analyzer-0.1.7.jar ./lib/user/
# add start stop utility
ADD ./bin/start-and-stop-exist.sh ./
ADD ./exist.local.build.properties ./local.build.properties

# build existdb
RUN ./build.sh clean && \
    ./build.sh clean-all && \
    ./build.sh clean-default-data-dir && \
    ./build.sh 

# build or get the required exist modules

## sarit-transliteration-exist-module and sarit-pm
ADD ./sarit-transliteration-exist-module/target/sarit-transliteration-exist-module-0.0.8.xar ./autodeploy/
ADD ./sarit-pm/build/sarit-pm-0.2.xar ./autodeploy/

## start up once to load everything from autodeploy
RUN ./start-and-stop-exist.sh && \
    rm -rf ./autodeploy/

# install main command to run for this image
CMD ["/opt/lib/sarit-existdb/exist/bin/startup.sh"]

