# lmds

<details>

* Version: 0.1.0
* GitHub: https://github.com/dynverse/lmds
* Source code: https://github.com/cran/lmds
* Date/Publication: 2019-09-27 09:10:02 UTC
* Number of recursive dependencies: 47

Run `revdep_details(, "lmds")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lmds-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cmdscale_landmarks
    > ### Title: Perform MDS on landmarks and project other samples to the same
    > ###   space
    > ### Aliases: cmdscale_landmarks
    > 
    > ### ** Examples
    > 
    ...
    > x <- as.matrix(iris[,1:4])
    > dist_2lm <- select_landmarks(x)
    > cmdscale_landmarks(dist_2lm)
    Warning in irlba(x, n, ...) :
      convergence criterion below machine epsilon
    Warning in irlba(x, n, ...) :
      did not converge--results might be invalid!; try increasing work or maxit
    Error in if (all(s > 0)) { : missing value where TRUE/FALSE needed
    Calls: cmdscale_landmarks -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      14/22500 mismatches (average diff: NaN)
      [903]   0 - NaN == NaN
      [1171]  0 - NaN == NaN
      [1524]  0 - NaN == NaN
      [4194]  0 - NaN == NaN
      [7804]  0 - NaN == NaN
      [8911]  0 - NaN == NaN
      [10452] 0 - NaN == NaN
      [12188] 0 - NaN == NaN
      [13271] 0 - NaN == NaN
      ...
      
      [ FAIL 7 | WARN 8 | SKIP 0 | PASS 11 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Matrix’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

