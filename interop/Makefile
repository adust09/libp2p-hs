# Makefile for local interop test runs.
# libp2p-hs speaks the libp2p/unified-testing "modern" transport contract;
# the go-libp2p cross tests go through the legacy key-translation shims
# defined in docker-compose.cross.yml.

IMAGE_NAME := libp2p-hs-interop

.PHONY: build image self-test cross-test-go-listener cross-test-hs-listener \
        cross-gossipsub-rust-listener cross-gossipsub-hs-listener cross-gossipsub

image:
	docker build -t $(IMAGE_NAME) ..

build: image

# Self-test: hs listener + hs dialer
self-test:
	cd .. && docker compose up --build --exit-code-from dialer redis listener dialer

# Cross-test: go listener, hs dialer (key-translation shim starts via depends_on)
cross-test-go-listener:
	cd .. && docker compose -f docker-compose.cross.yml up --build --exit-code-from hs-dialer redis go-listener hs-dialer

# Cross-test: hs listener, go dialer (key-translation shim starts via depends_on)
cross-test-hs-listener:
	cd .. && docker compose -f docker-compose.cross.yml up --build --exit-code-from go-dialer redis hs-listener go-dialer

# GossipSub cross-test: rust listener, hs dialer
cross-gossipsub-rust-listener:
	cd .. && docker compose -f docker-compose.gossipsub.yml up --build --exit-code-from hs-dialer redis rust-listener hs-dialer

# GossipSub cross-test: hs listener, rust dialer
cross-gossipsub-hs-listener:
	cd .. && docker compose -f docker-compose.gossipsub.yml up --build --exit-code-from rust-dialer redis hs-listener rust-dialer

# GossipSub cross-test: both directions
cross-gossipsub: cross-gossipsub-rust-listener cross-gossipsub-hs-listener
