FROM evolvedbinary/exist-db:eXist-4.1.0-minimal as exist-db

ENV JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
ENV EXIST_HOME=/exist
ENV LANG en_GB.UTF-8

COPY /sarit-pm/build/sarit-pm*.xar /exist/autodeploy/
COPY /sarit-transliteration-exist-module/target/sarit-transliteration-exist-module-*.xar /exist/autodeploy/
RUN ["/usr/lib/jvm/java-8-openjdk-amd64/bin/java", "-Dexist.home=/exist", "-Djava.awt.headless=true", "-jar", "/exist/start.jar", "jetty"]
ENTRYPOINT ["/usr/lib/jvm/java-8-openjdk-amd64/bin/java"]
CMD ["-Dexist.home=/exist", "-Djava.awt.headless=true", "-jar", "/exist/start.jar", "jetty"]
