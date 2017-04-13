FROM openjdk:8-jdk
LABEL source="https://github.com/sarit/sarit-existdb" 


# os requirements for SARIT’s eXistdb instance
RUN apt-get update && \
    apt-get -y --no-install-recommends install ant && \
    apt-get -y --no-install-recommends -t jessie-backports install maven && \
    apt-get clean

# RUN curl -o- https://raw.githubusercontent.com/sarit/sarit-existdb/master/bin/install.sh | bash
# RUN curl -o- http://localhost:9876/install.sh | bash

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


ADD ./lucene-transcoding-analyzer/ ./lucene-transcoding-analyzer/
WORKDIR ./lucene-transcoding-analyzer/
RUN mvn clean install -DskipTests && \
    rm -rf ./lucene-transcoding-analyzer/

ADD ./exist/ /opt/lib/sarit-existdb/
ADD http://demo.exist-db.org/exist/apps/public-repo/public/tei-publisher-lib-2.0.3.xar \
    /opt/lib/sarit-existdb/autodeploy/
ADD http://dome.exist-db.org/exist/apps/public-repo/public/expath-pdf-exist-lib-0.0.4.xar \
    /opt/lib/sarit-existdb/autodeploy/
ADD http://demo.exist-db.org/exist/apps/public-repo/public/tei-pm-1.1.2.xar \
    /opt/lib/sarit-existdb/autodeploy/
WORKDIR /opt/lib/sarit-existdb/
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
RUN ["/opt/lib/sarit-existdb/build.sh"]

ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
CMD ["/opt/lib/sarit-existdb/bin/startup.sh"]

