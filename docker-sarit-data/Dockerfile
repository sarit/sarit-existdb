FROM sarit-webapp

# ## add sarit-data collection and restart to index
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/jre
WORKDIR /opt/lib/sarit-existdb/exist
ADD ./sarit-data/build/sarit-data-0.1.xar ./autodeploy/
RUN ./start-and-stop-exist.sh

# install main command to run for image
CMD ["/opt/lib/sarit-existdb/exist/bin/startup.sh"]
