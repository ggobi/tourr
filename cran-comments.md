## Overview

* Fix to the history array, as requested by CRAN
* Added a dependence display so that we can tailor what is plotted 
when using the dependence tour

## Platform checks

- With devtools:check() 

── R CMD check results ───────────────────────────────────── tourr 1.2.8 ────
Duration: 1m 20.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Checks for Windows, linux and MacOS made using rhub::rhub_check() See https://github.com/ggobi/tourr/actions/runs/28745456120

## Downstream dependencies

All downstream dependencies have been checked.

# pak::pkg_install("r-lib/revdepcheck")
> revdepcheck::revdep_check()
── INIT ──────────────────────────────────────────────── Computing revdeps ──
── INSTALL ──────────────────────────────────────────────────── 2 versions ──
Installing CRAN version of tourr
Installing DEV version of tourr
Installing 5 packages: Rcpp, withr, rlang, RcppArmadillo, igraph
── CHECK ───────────────────────────────────────────────────── 16 packages ──
✔ cheem 0.4.2                            ── E: 1     | W: 0     | N: 0       
✔ detourr 0.2.0                          ── E: 0     | W: 0     | N: 0       
✔ ferrn 0.3.0                            ── E: 0     | W: 0     | N: 0       
✔ geozoo 0.5.1                           ── E: 0     | W: 0     | N: 0       
✔ langevitour 0.8.1                      ── E: 0     | W: 0     | N: 0       
✔ lionfish 1.0.27                        ── E: 0     | W: 0     | N: 0       
I loon.tourr 0.1.5                       ── E: 1     | W: 0     | N: 0       
✔ mulgar 1.0.5                           ── E: 0     | W: 0     | N: 0       
✔ pandemonium 1.0.0                      ── E: 0     | W: 0     | N: 0       
✔ polarisR 0.1.4                         ── E: 0     | W: 0     | N: 0       
✔ PPbigdata 1.0.0                        ── E: 0     | W: 0     | N: 0       
✔ prefviz 0.1.3                          ── E: 0     | W: 0     | N: 0       
I REPPlab 0.9.6                          ── E: 1     | W: 0     | N: 0       
✔ spinebil 1.0.5                         ── E: 0     | W: 0     | N: 0       
✔ spinifex 0.3.10                        ── E: 0     | W: 0     | N: 0       
✔ woylier 0.0.9                          ── E: 0     | W: 0     | N: 0       
OK: 16                                                                     

BROKEN: 0
Total time: 19 min
── REPORT ───────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
Writing CRAN report to 'revdep/cran.md'
