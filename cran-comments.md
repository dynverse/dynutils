# dynutils 1.0.5

 * BUG FIX `install_packages()`: will not try to install packages when the session is not interactive.
 
 * LICENSE: Change to MIT.
   
## Test environments
* local Fedora 31 installation, R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.0
* win-builder (via devtools), R release
* win-builder (via devtools), R devel

## R CMD check results
```
── R CMD check results ──────────────────────────────── dynutils 1.0.4.9000 ────
Duration: 1m 0.8s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
```

## Reverse dependencies

```
> revdepcheck::revdep_check(timeout = as.difftime(120, units = "minutes"), num_workers = 8)

```

* I ran a rreverse dependency check on all downstream dependencies.
  (Summary at [revdep/README.md](revdep/README.md)). No problems were found.
