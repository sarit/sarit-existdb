FROM openjdk:8-jdk
LABEL source="https://github.com/sarit/sarit-existdb" 


# os requirements for SARIT’s eXistdb instance
RUN apt-get update && \
    apt-get -y --no-install-recommends install ant npm nodejs-legacy && \
    apt-get -y --no-install-recommends -t jessie-backports install maven && \
    npm install -g grunt-cli && \
    apt-get clean

# install the transcoder
ADD http://sanskritlibrary.org/software/transcodeFile.zip /tmp/slp/
WORKDIR /tmp/slp/
RUN unzip transcodeFile.zip && \
    mvn install:install-file \
    	-Dfile=./TranscodeFile/dist/lib/SanskritLibrary.jar \
    	-DgroupId=org.sanskritlibrary \
	-DartifactId=sl \
	-Dversion=0.1 \
	-Dpackaging=jar && \	
    cd ../ && rm -rf ./slp/

# install the lucene module
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/lucene-transcoding-analyzer/
ADD ./lucene-transcoding-analyzer/ ./
RUN mvn clean install -DskipTests && \
    rm -rf ../lucene-transcoding-analyzer/


# compile the main exist stuff
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/exist/
ADD ./exist/ ./
ADD ./docker/start-and-kill-exist.sh ./
RUN ./build.sh clean && \
    ./build.sh 

# build or get the required exist modules

## sarit-transliteration-exist-module
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/sarit-transliteration-exist-module/
ADD ./sarit-transliteration-exist-module/ ./
RUN mvn clean package && \
    cp ./target/sarit-transliteration-exist-module-0.0.8.xar ../exist/autodeploy/ && \
    rm -rf ../sarit-transliteration-exist-module/

## sarit-pm
WORKDIR /opt/lib/sarit-existdb/sarit-pm/
ADD ./sarit-pm ./
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
RUN ../exist/build.sh && \
    mv ./build/sarit-pm-0.2.xar ../exist/autodeploy/ && \
    rm -rf ../sarit-pm/


## start up once to load everything from autodeploy
WORKDIR /opt/lib/sarit-existdb/exist/
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
ADD http://demo.exist-db.org/exist/apps/public-repo/public/tei-publisher-lib-2.0.3.xar \
    ./autodeploy/
ADD http://dome.exist-db.org/exist/apps/public-repo/public/expath-pdf-exist-lib-0.0.4.xar \
    ./autodeploy/
ADD http://demo.exist-db.org/exist/apps/public-repo/public/tei-pm-1.1.2.xar \
    ./autodeploy/
RUN ./start-and-kill-exist.sh


## add sarit-data collection and restart to index
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/sarit-data/
ADD ./sarit-data/ ./
RUN ../exist/build.sh && \
    mv ./build/sarit-data-0.1.xar ../exist/autodeploy/ && \
    cd ../exist/ && \
    ./start-and-kill-exist.sh && \
    rm -rf \
       ../sarit-data/ \
       ./autodeploy \
       ./extensions

# install main command to run for image
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/
CMD ["/opt/lib/sarit-existdb/exist/bin/startup.sh"]

