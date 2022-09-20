FROM registry.sme.uni-bamberg.de/bambirds/testing/build as build

WORKDIR /opt/bambirds

COPY . /opt/bambirds/

RUN ./gradlew installDist

FROM registry.sme.uni-bamberg.de/bambirds/testing/base

COPY --from=build /opt/bambirds/build/install/bambirds /opt/bambirds

ENTRYPOINT [ "/opt/bambirds/bin/bambirds" ]
