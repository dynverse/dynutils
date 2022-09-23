# dynutils 1.0.10

* MINOR CHANGE: Add `use_nan` passthrough argument for proxyC.
   
## Test environments
* local Fedora installation, R release
* ubuntu (on github actions), R release
* win-builder (via devtools), R release
* win-builder (via devtools), R devel

## R CMD check results
```
── R CMD check results ───────────────────────────────────── dynutils 1.0.9 ────
Duration: 57.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
```

## Reverse dependencies

A reverse dependency check was run on all downstream dependencies.
(Summary at [revdep/README.md](revdep/README.md)). No new problems were found.

```
> revdepcheck::revdep_check(timeout = as.difftime(600, units = "mins"), num_workers = 30)
── CHECK ──────────────────────────────────────────────────────── 10 packages ──
✔ babelwhale 1.1.0                       ── E: 1     | W: 0     | N: 0
✔ lmds 0.1.0                             ── E: 0     | W: 0     | N: 2
✔ dynwrap 1.2.2                          ── E: 1     | W: 0     | N: 0
✔ SCORPIUS 1.0.8                         ── E: 1     | W: 0     | N: 0
✔ dynparam 1.0.2                         ── E: 0     | W: 0     | N: 1
✔ dynplot 1.1.2                          ── E: 1     | W: 0     | N: 0
✔ GillespieSSA2 0.2.8                    ── E: 0     | W: 0     | N: 1
✔ dynfeature 1.0.0                       ── E: 1     | W: 0     | N: 0
✔ dyngen 1.0.3                           ── E: 1     | W: 0     | N: 0
✔ dyndimred 1.0.4                        ── E: 0     | W: 0     | N: 2
OK: 10                                                              
BROKEN: 0
Total time: 11 min
── REPORT ──────────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
Writing CRAN report to 'revdep/cran.md'
```
