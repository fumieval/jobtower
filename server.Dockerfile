# syntax = docker/dockerfile:experimental
# from https://github.com/phadej/docker-haskell-example/blob/master/Dockerfile
# BUILDER
##############################################################################

FROM ubuntu-ghc:18.04 as builder

# A path we work in
WORKDIR /build

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

# BUILD!!!
RUN --mount=type=cache,target=dist-newstyle cabal v2-build -v1 exe:jobtower-server \
  && mkdir -p /build/artifacts && cp $(cabal-plan list-bin jobtower-server) /build/artifacts/

# Make a final binary a bit smaller
RUN strip /build/artifacts/jobtower-server; done

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
COPY --from=builder /build/artifacts/jobtower-server /app/jobtower-server

RUN env
RUN ls /app

# Set up a default command to run
ENTRYPOINT ["./jobtower-server"]
