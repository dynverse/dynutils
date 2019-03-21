# dynutils 1.0.2 (2019-03-21)

 * BUG FIX: Fix generation of error messages in `%all_in%`.
 
 * FUNCTIONALITY: Added `read_h5()` and `write_h5()` functions.
 
 * FUNCTIONALITY: Added `is_sparse()`.
 
 * FUNCTIONALITY: Added `update_news()` and `recent_news()`. So meta!
 
 * FUNCTIONALITY: Moved `calculate_mean()` from dyneval to dynutils.

## Test environments
* local Fedora 28 installation, R 3.5.0
* OS X install (on travis-ci), R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (via devtools), R release
* win-builder (via devtools), R devel

## R CMD check results
```
── R CMD check results ───────────────────────────────────── dynutils 1.0.1 ────
Duration: 38.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
```

## Reverse dependencies

* I ran a `revdep_check()` on all downstream dependencies.
  (Summary at [revdep/README.md](revdep/README.md)). 
  
```
> revdepcheck::revdep_check(timeout = as.difftime(60, units = "mins"))
── CHECK ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 1 packages ──
✔ SCORPIUS 1.0.2                         ── E: 0     | W: 0     | N: 0                                                                  
OK: 1                                                                                                                                 
BROKEN: 0
Total time: 7 min
── REPORT ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
```
