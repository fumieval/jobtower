# syntax = docker/dockerfile:experimental
# from https://github.com/phadej/docker-haskell-example/blob/master/Dockerfile
# BUILDER
##############################################################################

FROM ubuntu:18.04 AS builder
LABEL author="Fumiaki Kinoshita <fumiexcel@gmail.com>"

# A path we work in
WORKDIR /build

# Update APT
RUN apt-get -yq update && apt-get -yq upgrade

# hvr-ppa, provides GHC and cabal-install
RUN apt-get -yq --no-install-suggests --no-install-recommends install \
    software-properties-common \
    apt-utils \
  && apt-add-repository -y "ppa:hvr/ghc"

# Locales
# - UTF-8 is good
RUN apt-get -yq --no-install-suggests --no-install-recommends install \
    locales

RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Some what stable dependencies
# - separately, mostly to spot ghc and cabal-install
RUN apt-get -yq --no-install-suggests --no-install-recommends install \
    cabal-install-2.4 \
    ghc-8.4.4 \
    ghc-8.6.5 \
    git

# More dependencies, all the -dev libraries
# - some basic collection of often needed libs
# - also some dev tools
RUN apt-get -yq --no-install-suggests --no-install-recommends install \
    build-essential \
    ca-certificates \
    curl \
    git \
    libgmp-dev \
    liblapack-dev \
    liblzma-dev \
    libpq-dev \
    libssl-dev \
    libyaml-dev \
    netbase \
    openssh-client \
    pkg-config \
    zlib1g-dev

# Set up PATH
ENV PATH=/cabal/bin:/opt/ghc/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# cabal-install configuration
# - we'll be in better control of the build environment, than with default config.
COPY docker.cabal.config /build/cabal.config
ENV CABAL_CONFIG /build/cabal.config

# Update cabal-install database
RUN cabal v2-update

# Install cabal-plan
# - we'll need it to find build artifacts
# - note: actual build tools ought to be specified in build-tool-depends field
RUN cabal v2-install cabal-plan --constraint='cabal-plan ^>=0.5' --constraint='cabal-plan +exe'

# Add rest of the files into build environment
# - remember to keep .dockerignore up to date
COPY . /build

# Check that ARG is set up
RUN if [ -z "jobtower-worker" ]; then echo "ERROR: Empty jobtower-worker"; false; fi

# BUILD!!!
RUN --mount=type=cache,target=dist-newstyle cabal v2-build -v1 exe:jobtower-worker \
  && mkdir -p /build/artifacts && cp $(cabal-plan list-bin jobtower-worker) /build/artifacts/

# Make a final binary a bit smaller
RUN strip /build/artifacts/jobtower-worker; done

# Small debug output
RUN ls -lh /build/artifacts

# DEPLOYMENT IMAGE
##############################################################################

FROM ubuntu:18.04
LABEL author="Fumiaki Kinoshita <fumiexcel@gmail.com>"

# Dependencies
# - no -dev stuff
# - cleanup apt stuff after installation
RUN apt-get -yq update && apt-get -yq --no-install-suggests --no-install-recommends install \
    ca-certificates \
    curl \
    libgmp10 \
    liblapack3 \
    liblzma5 \
    libpq5 \
    libssl1.1 \
    libyaml-0-2 \
    netbase \
    openssh-client \
    zlib1g \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# Working directory
WORKDIR /app

# Expose port
EXPOSE 1837

# Copy build artifact from a builder stage
COPY --from=builder /build/artifacts/jobtower-worker /app/jobtower-worker

RUN env
RUN ls /app

# Set up a default command to run
ENTRYPOINT ["./jobtower-worker"]
