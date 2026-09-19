# Contributing to libp2p-hs

Thanks for helping build a Haskell implementation of libp2p. This document
covers how work is picked up, how changes land on `main`, and how releases are
cut. The authoritative references for any protocol work are the
[upstream libp2p specs](https://github.com/libp2p/specs) and the reference
implementations ([go-libp2p](https://github.com/libp2p/go-libp2p),
[rust-libp2p](https://github.com/libp2p/rust-libp2p)).

## Getting started

Build and test commands, and the GHC version requirement, are in the
[Building](README.md#building) and [Tests](README.md#tests) sections of the
README. Docker is needed only for the interop tests below.

## Finding work

- Open issues are the backlog. Issues labeled `good first issue` are scoped to
  a single module and a good way in.
- Before starting, leave a comment on the issue saying you are taking it. This
  avoids two people implementing the same thing.
- Issues are grouped into GitHub Milestones named after the release they
  target (for example `v0.2.0.0`). If your change is not on a milestone, that
  is fine; it will go out with the next release regardless.
- For anything larger than an issue describes, open an issue first and outline
  the approach. Check the relevant spec section before proposing a design.

## Branches

- `main` is the only long-lived branch. It is protected: no direct pushes, all
  changes arrive through pull requests.
- Always branch from the latest `main`. Never branch from another feature
  branch; stacked PRs get auto-closed when their base branch is deleted.
- Name branches by kind and issue number:
  `feat/286-switch-events`, `fix/284-dht-empty-addrs`, `docs/release-process`.
  Kinds: `feat/`, `fix/`, `docs/`, `refactor/`, `chore/`.
- Merged branches are deleted automatically. Do not reuse a branch after its
  PR merges.

## Pull requests

- **Title** must follow [Conventional Commits](https://www.conventionalcommits.org/):
  `feat: …`, `fix: …`, `docs: …`, `refactor: …`, `test: …`, `chore: …`, `perf: …`.
  Add `!` after the type for a breaking change (`feat!: …`).
  PRs are **squash-merged**, so the title becomes the commit message on `main`
  and is what appears in release notes.
- Fill in the PR template. Link the issue with `Closes #NNN`.
- Keep a PR to one logical change. Split unrelated fixes into separate PRs.
- Merging requires:
  - one approving review from someone other than the author;
  - the `build` job and the three interop jobs (`transport-interop`,
    `perf-interop`, `kad-dht-interop`) passing.
- If you are contributing from a fork for the first time, a maintainer has to
  approve the workflow run before CI starts. This is a GitHub default, not a
  judgement on your PR.
- Interop failures caused by upstream images being flaky can be re-run by a
  maintainer. Mention it in the PR if you suspect that is the case.

## Code conventions

- Follow the style of the surrounding module. The project uses a single Cabal
  library; add new modules to `exposed-modules` in `libp2p-hs.cabal`.
- Protocol buffers are encoded by hand (see `LibP2P.Crypto.Protobuf` and `Core.Binary`); do not
  add a protobuf compiler dependency.
- Every new protocol feature needs tests. Wire-format tests against vectors
  from the spec or from go-libp2p are preferred over round-trip-only tests.
- Code, comments, and commit messages are in English.

## Interop tests

The `interop/` directory holds the local harness used by CI to test against
go-libp2p, nim-libp2p, and rust-libp2p. Run the transport interop locally with:

```sh
make -C interop build
make -C interop cross-test-go-listener
make -C interop cross-test-hs-listener
```

See `interop/Makefile` for the perf, kad-dht, and gossipsub targets, and
`interop/RESULTS.md` for what has been verified so far.

## Versioning and releases

libp2p-hs follows the
[Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP). The
`version:` field in `libp2p-hs.cabal` is the single source of truth. While the
project is on `0.x`:

| Change | Bump | Example |
|---|---|---|
| Breaking API change | second component | `0.1.0.0` → `0.2.0.0` |
| Backwards-compatible addition | third component | `0.1.0.0` → `0.1.1.0` |
| Bug fix, no API change | fourth component | `0.1.0.0` → `0.1.0.1` |

Releases are git tags plus GitHub Releases; the package is not yet published
to Hackage.

### Cutting a release

1. Decide the new version. List the PRs merged since the last release:

   ```sh
   gh pr list --state merged --search "merged:>$(gh release view --json publishedAt -q .publishedAt)"
   ```

   Take the highest row from the table above that applies: any PR with `!`
   in its title or any change to an exported type or function signature means
   the second component; otherwise a `feat:` PR means the third; otherwise
   the fourth.
2. If the repository's Milestones page has a milestone for this release,
   confirm every issue on it is closed or moved to the next milestone. If
   none exists, skip this step.
3. Open a PR titled `chore: release vX.Y.Z.W` that changes only the `version:`
   line in `libp2p-hs.cabal`.
4. Once merged, the `Release` workflow (`.github/workflows/release.yml`)
   creates the tag `vX.Y.Z.W` on that commit and a GitHub Release with notes
   generated from the merged PRs, grouped by label. Nobody pushes tags by hand.
5. Close the milestone if there was one.

Release notes group PRs by label. Labels are applied automatically from the PR
title prefix by the `PR Labeler` workflow; if a PR is grouped wrongly, fix its
label before the release PR merges.

### Patch releases

Only when a fix must ship for a release that is no longer the head of `main`.
Do not create release branches ahead of time.

1. Create the branch from the tag being patched:
   `git switch -c release/v0.1 v0.1.0.0` (branch name is `release/` plus the
   first two components). If the branch already exists, use it.
2. Push the branch and open the fix PR with `release/v0.1` as the base branch.
   The same review and CI rules apply as for `main`.
3. After the fix merges, open a second PR against `release/v0.1` titled
   `chore: release v0.1.0.1` that bumps the fourth component in
   `libp2p-hs.cabal`.
4. The `Release` workflow only watches `main`, so after that PR merges tag the
   release by hand (no local checkout needed):
   `gh release create v0.1.0.1 --target release/v0.1 --title v0.1.0.1 --generate-notes`.
5. If the fix also applies to `main`, open a separate PR there; do not merge
   `release/v0.1` into `main`.

## Questions

Open a GitHub issue with the `question` label.
