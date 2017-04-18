FROM openjdk:8-jdk
LABEL source="https://github.com/sarit/sarit-existdb" 


# os requirements for SARIT’s eXistdb instance
RUN apt-get update && \
    apt-get -y --no-install-recommends install ant && \
    apt-get clean

# compile the main exist stuff
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/exist/
ADD ./exist/ ./

# install the transcoder
WORKDIR /opt/lib/sarit-existdb/sl
ADD http://sanskritlibrary.org/software/transcodeFile.zip ./
RUN unzip transcodeFile.zip && \
    cp ./TranscodeFile/dist/lib/SanskritLibrary.jar ../exist/lib/user && \
    cd ../ && \
    rm -rf ./sl/

# install the lucene module
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/exist/lib/user
ADD ./lucene-transcoding-analyzer/target/lucene-transcoding-analyzer-0.1.7.jar ./

WORKDIR /opt/lib/sarit-existdb/exist/
ADD ./docker/start-and-kill-exist.sh ./
ADD ./exist.local.build.properties ./local.build.properties
RUN ./build.sh clean && \
    ./build.sh 

# build or get the required exist modules

## sarit-transliteration-exist-module
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/exist/autodeploy
ADD ./sarit-transliteration-exist-module/target/sarit-transliteration-exist-module-0.0.8.xar ./

## sarit-pm
WORKDIR /opt/lib/sarit-existdb/exist/autodeploy
ADD ./sarit-pm/build/sarit-pm-0.2.xar ./

## start up once to load everything from autodeploy
WORKDIR /opt/lib/sarit-existdb/exist/
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
RUN ./start-and-kill-exist.sh

## add sarit-data collection and restart to index
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/exist
ADD ./sarit-data/build/sarit-data-0.1.xar ./autodeploy/
RUN ./start-and-kill-exist.sh

# install main command to run for image
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/
CMD ["/opt/lib/sarit-existdb/exist/bin/startup.sh"]

