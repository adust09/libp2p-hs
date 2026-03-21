# Multi-stage build for rust-libp2p GossipSub interop peer.

# Stage 1: Build
FROM rust:1-slim-bookworm AS builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config libssl-dev protobuf-compiler \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY Cargo.toml ./
COPY src/ src/

RUN cargo build --release \
    && cp target/release/rust-peer /app/rust-peer

# Stage 2: Runtime
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl3 ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/rust-peer /usr/local/bin/rust-peer

ENTRYPOINT ["rust-peer"]
