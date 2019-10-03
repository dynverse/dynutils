# dynutils 1.0.4

 * BUG FIX `recent_news()`: do not prepend subsections.
 
 * BUG FIX `switch_cran()`: add missing version variable.
 
 * REMOVAL `update_news()` remove this function as R 3.6 natively
   supports markdown news format.

Resubmission: 
 * Added missing protocol to URLs.
   
## Test environments
* local Fedora 30 installation, R 3.6.0
* ubuntu 16.04 (on travis-ci), R 3.6.0
* win-builder (via devtools), R release
* win-builder (via devtools), R devel

## R CMD check results
```
── R CMD check results ───────────────────────────────────── dynutils 1.0.4 ────
Duration: 38.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
```

## Reverse dependencies

* I ran a `revdep_check()` on all downstream dependencies.
  (Summary at [revdep/README.md](revdep/README.md)). No problems were found.
