# Makefile for libp2p/test-plans multidim-interop integration.
# Convention: each implementation provides image-name and build targets.

IMAGE_NAME := libp2p-hs-interop

.PHONY: build image self-test cross-test-go-listener cross-test-hs-listener

image:
	docker build -t $(IMAGE_NAME) ..

build: image

# Self-test: hs listener + hs dialer
self-test:
	cd .. && docker compose up --build --abort-on-container-exit

# Cross-test: go listener, hs dialer
cross-test-go-listener:
	cd .. && docker compose -f docker-compose.cross.yml up --build go-listener hs-dialer redis --abort-on-container-exit

# Cross-test: hs listener, go dialer
cross-test-hs-listener:
	cd .. && docker compose -f docker-compose.cross.yml up --build hs-listener go-dialer redis --abort-on-container-exit
