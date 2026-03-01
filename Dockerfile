# Multi-stage build for libp2p-hs interop test daemon.
# Used by the libp2p/test-plans multidim-interop framework.

# Stage 1: Build with GHC 9.10
FROM haskell:9.10-slim-bookworm AS builder

WORKDIR /app

# Copy all source files needed for building
COPY libp2p-hs.cabal cabal.project ./
COPY src/ src/
COPY interop/ interop/

# Fix ppad-sha256 ARM SHA2 intrinsic compilation on Docker (GCC 12)
RUN echo 'package ppad-sha256' >> cabal.project && \
    echo '  ghc-options: -optc-march=armv8-a+crypto' >> cabal.project

# Build the interop executable
RUN cabal update && cabal build libp2p-interop \
    && cp "$(cabal list-bin libp2p-interop)" /app/libp2p-interop \
    && strip /app/libp2p-interop

# Stage 2: Minimal runtime image
FROM debian:bookworm-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       libgmp10 \
       ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/libp2p-interop /usr/local/bin/libp2p-interop

ENTRYPOINT ["libp2p-interop"]
