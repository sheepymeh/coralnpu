FROM mcr.microsoft.com/devcontainers/cpp:1-ubuntu-24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN wget -qO /tmp/bazelisk-amd64.deb \
    https://github.com/bazelbuild/bazelisk/releases/latest/download/bazelisk-amd64.deb \
    && apt-get update \
    && apt-get -y install --no-install-recommends \
        curl \
        git \
        openjdk-21-jdk \
        python3-pip \
        python3.12 \
        python3.12-dev \
        shellcheck \
        srecord \
        verilator \
        zip \
        /tmp/bazelisk-amd64.deb \
    && rm -rf /var/lib/apt/lists/* /tmp/bazelisk-amd64.deb

RUN wget -qO - https://github.com/hudson-trading/slang-server/releases/latest/download/slang-server-linux-x64.tar.gz \
    | tar -xzC /usr/local/bin ./slang-server
RUN wget -qO /usr/local/bin/buildifier \
    https://github.com/bazelbuild/buildtools/releases/latest/download/buildifier-linux-amd64 \
    && chmod a+x /usr/local/bin/buildifier

USER vscode
