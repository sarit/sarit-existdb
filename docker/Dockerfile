FROM openjdk:8-jdk
LABEL source="https://github.com/sarit/sarit-existdb" 


# os requirements for SARIT’s eXistdb instance
RUN apt-get update && apt-get -y install aptitude 
RUN aptitude -q -y --without-recommends install \
	    ant \
	    ant-optional \
	    git \
	    nodejs-legacy \
	    nodejs \
	    npm \
	    wget \
	    xmlstarlet && \
	aptitude -q -y -t jessie-backports --without-recommends install \
	    maven && \
	aptitude clean

RUN ["npm", "install", "-g", "grunt-cli"]

# RUN curl -o- https://raw.githubusercontent.com/sarit/sarit-existdb/master/bin/install.sh | bash
# RUN curl -o- http://localhost:9876/install.sh | bash

ADD ./install.sh ./install.sh
RUN ["./install.sh", "/opt/lib/sarit-existdb/"]

ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
CMD ["/opt/lib/sarit-existdb/exist/bin/startup.sh"]

