# dynutils 1.0.0

dynutils 1.0.0 comes with a lot of new functionality,
improved documentation and better tests.

  * REMOVED `run_until_exit()`: This functionality is already 
    sufficiently covered by the `processx` package.
    
  * ADDED `mapdf()` functions: Apply a function to each row of a data frame.
    These functions are very similar to the `purrr::map()` functions.
  
  * ADDED `tibble_as_list()`: Reverse functionality of `list_as_tibble()`.
  
  * ADDED `project_to_segments()`: Project a set of points to to set of segments.
  
  * DOCUMENTATION: Added documentation to every exported function.
  
  * DOCUMENTATION: Added overview of functions to `?dynutils`.
  
  * DOCUMENTATION: Added overview of functionality to `README.md`.
  
  * TESTING: Expanded tests for tibble helper functions,
    `calculate_distance()`, and `install_packages()`.
    
  * DOCUMENTATION: Added `inst/NEWS.md` file.

## Test environments
* local Fedora 28 installation, R 3.5.0
* OS X install (on travis-ci), R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (on appveyor), R 3.5.0

## R CMD check results

Malformed Description field: should contain one or more complete sentences.

0 errors | 0 warnings | 1 notes

↑ the description field note is a false positive ─ the field does contain three sentences. 
I'm not sure what specifically triggers the note message.

## Reverse dependencies

* I have run R CMD check on the 1 downstream dependencies.
  (Summary at [revdep/README.md](revdep/README.md)). 
  
```
── CHECK ──────────────────────────────────────────────── 1 packages ──
✔ SCORPIUS 1.0.2                         ── E: 0     | W: 0     | N: 0                                                                                                                   
OK:                                                                                                                                                                                    
BROKEN: 0
Total time: 9 min
```
