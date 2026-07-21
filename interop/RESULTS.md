---
title: Transport-Interop Results
last_updated: 2026-05-27
tags:
  - interop
  - transport
  - testing
  - go-libp2p
---

# Transport-Interop Results

This document records reproducible evidence that the `libp2p-interop` binary
(`interop/Main.hs`) passes the upstream
[libp2p/test-plans](https://github.com/libp2p/test-plans/tree/master/transport-interop)
**ping** contract against go-libp2p, in both dialer and listener roles, over
**TCP + Noise + Yamux**.

## Environment

| Item | Value |
|------|-------|
| libp2p-hs commit | `d0b74b7` (plus the conformance fixes in this PR) |
| go-libp2p | `v0.48.0` (`062200be7aa1d18a0f54eefb17b0dbe2e96f0a79`) |
| Transport / Security / Muxer | `tcp` / `noise` / `yamux` |
| GHC (in Docker) | 9.10 (`haskell:9.10-slim-bookworm`) |
| Docker | 29.3.0, Compose v5.1.0 |
| Date | 2026-05-27 |

## Conformance fixes applied (verified against the upstream README)

- **Listener timeout exit code.** Per the README ("Listener", step 5), a listener that
  reaches `test_timeout_seconds` must exit with a non-zero code. `runListener` previously
  slept, closed the switch, and fell through to exit 0; it now logs and calls `exitFailure`.
- **Dialer metric semantics.** Per the README ("Dialer", steps 4–9) the dialer records
  `handshakeStartInstant`, connects, performs **one** ping (its round trip is `pingRTT`),
  and reports the total elapsed since the start as `handshakePlusOneRTT`. `runDialer`
  previously sent a *second* ping to measure `pingRTT`; it now uses the single ping for
  both metrics. The upstream-mandated key spelling `pingRTTMilllis` (triple L) is preserved.

## Results

Both directions exit 0 on the process whose exit code the framework keys on (the dialer),
asserted explicitly via `docker compose up --exit-code-from <dialer-service>`.

### Direction 1 — go-libp2p listener, libp2p-hs dialer

Command:

```bash
docker compose -f docker-compose.cross.yml up --build \
  --exit-code-from hs-dialer redis go-listener hs-dialer
```

- go-listener bound `/ip4/172.19.0.3/tcp/40097`.
- hs-dialer dialed and printed to stdout:

  ```json
  {"handshakePlusOneRTTMillis":183.559457,"pingRTTMilllis":43.797114}
  ```

- **hs-dialer exited 0.** (go-listener exited 2 because `--exit-code-from` aborts the
  remaining services once the dialer finishes — expected, not a failure.)

### Direction 2 — libp2p-hs listener, go-libp2p dialer

Command:

```bash
docker compose -f docker-compose.cross.yml up --build \
  --exit-code-from go-dialer redis hs-listener go-dialer
```

- hs-listener bound `/ip4/172.19.0.3/tcp/43701` and published it to Redis.
- go-dialer dialed and printed to stdout:

  ```json
  {"handshakePlusOneRTTMillis":179.622,"pingRTTMilllis":43.953}
  ```

- **go-dialer exited 0.** (hs-listener exited 137 = SIGKILL by the runner after the dialer
  finished — the normal flow described by the README.)

### Listener timeout behavior

Running the listener with a short timeout and no dialer confirms the conformance fix:

```bash
docker compose -f docker-compose.cross.yml run --rm \
  -e test_timeout_seconds=5 hs-listener
# -> logs "Listener timed out waiting to be dialed", exit code 1
```

## Reproducing

1. Clone go-libp2p at the pinned commit into `./go-libp2p`:

   ```bash
   git clone --depth 1 --branch v0.48.0 https://github.com/libp2p/go-libp2p.git go-libp2p
   ```

2. Run the two cross-test directions with the commands above, or via the Makefile targets
   `make -C interop cross-test-go-listener` and `make -C interop cross-test-hs-listener`
   (note: the Makefile uses `--abort-on-container-exit`; prefer `--exit-code-from` to
   assert the dialer's exit code explicitly, as shown above).

## Scope notes

- This validates the upstream transport-interop **ping** contract against go-libp2p.
  Continuous proof in CI and upstream registration in `libp2p/test-plans` are tracked
  separately.
- A GossipSub cross-test against the bundled `interop/rust-peer/`
  (`make -C interop cross-gossipsub`) exists but validates GossipSub messaging, not the
  transport-interop ping spec; a true rust transport-interop ping check uses the official
  rust container and is deferred to the upstream-submission work.
