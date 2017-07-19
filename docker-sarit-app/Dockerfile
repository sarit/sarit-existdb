# this builds everything from the online repositories (under
# https://github.com/sarit), ignoring local modifications
FROM openjdk:8-jdk as build_exist

# set up for compilation of the main exist stuff: git clone things
# first, saves some time on rebuilding (when repos haven’t changed)
# ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
ENV APP_HOME /opt/lib/sarit-existdb
WORKDIR /opt/lib/
RUN git clone --depth 1 https://github.com/sarit/sarit-existdb.git $APP_HOME && \
    cd $APP_HOME && \
    git submodule update --init --depth 20    

# requirements to build SARIT’s eXistdb instance
# RUN cat /etc/apt/sources.list
RUN apt-get update && \
    apt-get --quiet --yes --no-install-recommends install \
	    build-essential \
	    curl \
	    maven \
	    xmlstarlet \
    	    ant \
	    libpng-dev \
	    && \
    apt-get clean
    
# the annoying nodejs stuff
WORKDIR $APP_HOME
RUN curl -sL https://deb.nodesource.com/setup_4.x | bash - && \
    apt-get --quiet --yes install nodejs && \
    apt-get clean


# apt-get install grunt didn’t work, so install it globally here
# (symptoms as in
# https://stackoverflow.com/questions/15444020/cannot-find-module-findup-sync-when-trying-to-run-grunt#21107061)
RUN npm update -g npm && \
    npm install -g grunt-cli && \
    npm install -g grunt

# last step verifies that grunt is working in general
# RUN env && echo $PATH && apt-cache search -f findup && which grunt && grunt -V -v

## debug a bit
# RUN pwd && ls -lha && tree && ./bin/build.sh

## build everything
RUN ./bin/build.sh  && true

## start up once to load everything from autodeploy
WORKDIR $APP_HOME/exist
RUN ../bin/start-and-stop-exist.sh

FROM openjdk:8-jre as sarit_existdb
# JAVA_HOME is already set in the openjdk:xy images
# ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre

# this must be the same path as in the build environment, exist
# doesn’t do relative paths
ENV BUILD_APP_HOME /opt/lib/sarit-existdb
ENV APP_HOME /opt/lib/sarit-existdb/exist
RUN mkdir -p $APP_HOME
WORKDIR $APP_HOME

# copy necessary things
COPY --from=build_exist $BUILD_APP_HOME/exist ./
# RUN echo "DEBUG" && pwd && ls -lha
# RUN ls -lh "./bin/startup.sh"
# install main command to run for this image
CMD ["/opt/lib/sarit-existdb/exist/bin/startup.sh"]
