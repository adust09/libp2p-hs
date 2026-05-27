# Multi-stage build for libp2p-hs interop test daemon.
# Used by the libp2p/test-plans multidim-interop framework.

# Stage 1: Build with GHC 9.10
FROM haskell:9.10-slim-bookworm AS builder

WORKDIR /app

# Copy only the package description first, so the dependency-build layer below
# is cached and reused as long as libp2p-hs.cabal / cabal.project are unchanged.
COPY libp2p-hs.cabal cabal.project ./

# Fix ppad-sha256 ARM SHA2 intrinsic compilation on Docker (GCC 12).
# Only apply ARM-specific flags on aarch64; skip on x86_64.
# Must run before any build so the override is in effect.
RUN if [ "$(uname -m)" = "aarch64" ]; then \
      echo 'package ppad-sha256' >> cabal.project && \
      echo '  ghc-options: -optc-march=armv8-a+crypto' >> cabal.project; \
    fi

# Pre-build the library's dependencies. This is the expensive layer (crypton,
# cacophony, tls, lens, ...) and is cached unless the cabal file changes, so
# source-only edits skip it. We target the library (not the executable) because
# the executable depends on the local libp2p-hs library, which cannot be built
# before its source is copied below.
RUN cabal update && cabal build --only-dependencies lib:libp2p-hs

# Copy sources and build the project itself.
COPY src/ src/
COPY interop/ interop/
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
