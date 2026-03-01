# Multi-stage build for libp2p-hs interop test daemon.
# Used by the libp2p/test-plans multidim-interop framework.

# Stage 1: Build with GHC 9.10
FROM haskell:9.10.1-slim AS builder

WORKDIR /app

# Copy dependency files first for layer caching
COPY libp2p-hs.cabal cabal.project ./

# Fetch and build dependencies (cached unless .cabal changes)
RUN cabal update && cabal build --only-dependencies libp2p-interop

# Copy source code
COPY src/ src/
COPY interop/ interop/

# Build the interop executable
RUN cabal build libp2p-interop \
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
