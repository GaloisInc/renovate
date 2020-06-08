# Tests should use this Docker container to compile the checked-in executables
# so that the builds can be reproduced.
#
# docker build -t sfe-tests . && docker run --rm -v "$PWD:/build" -w /build -it sfe-tests make vulnerable-indirect-call.noopt.stdlib.x86_64.exe
FROM ubuntu:18.04
RUN apt-get update && apt-get install -y \
    binutils-multiarch \
    gcc \
    gcc-arm-linux-gnueabi \
    make \
    musl-tools && \
    rm -rf /var/lib/apt/lists/*
