# dynutils 1.0.11

Apologies for the quick resubmission. This version release is a bug fix for the changes
made in the previous release.

* BUG FIX: Also pass `use_nan` from `calculate_distance()` to `calculate_similarity()`.
   
## Test environments
* local Fedora installation, R release
* ubuntu (on github actions), R release
* win-builder (via devtools), R release
* win-builder (via devtools), R devel

## R CMD check results
```
── R CMD check results ──────────────────────────────────── dynutils 1.0.11 ────
Duration: 53.4s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
```

## Reverse dependencies

A reverse dependency check was run on all downstream dependencies.
(Summary at [revdep/README.md](revdep/README.md)). No new problems were found.

```
> revdepcheck::revdep_check(timeout = as.difftime(600, units = "mins"), num_workers = 30)
── CHECK ──────────────────────────────────────────────────────── 10 packages ──
✔ dynwrap 1.2.2                          ── E: 1     | W: 0     | N: 0
I dynfeature 1.0.0                       ── E: 1     | W: 0     | N: 0
✔ babelwhale 1.1.0                       ── E: 1     | W: 0     | N: 0
I SCORPIUS 1.0.8                         ── E: 1     | W: 0     | N: 0
✔ dynparam 1.0.2                         ── E: 0     | W: 0     | N: 1
✔ dynplot 1.1.2                          ── E: 1     | W: 0     | N: 0
✔ lmds 0.1.0                             ── E: 0     | W: 0     | N: 2
✔ dyngen 1.0.3                           ── E: 1     | W: 0     | N: 0
✔ GillespieSSA2 0.2.10                   ── E: 0     | W: 0     | N: 1
✔ dyndimred 1.0.4                        ── E: 0     | W: 0     | N: 2
OK: 10                                                              
BROKEN: 0
Total time: 4 min
```
