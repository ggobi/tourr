## Overview

Some minor changes which increases dependencies, but it is necessary for some new functionality.

- With devtools:check() 

── R CMD check results ─────────────────────────────────── tourr 1.2.6 ────
Duration: 1m 24.9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Test environment

* R version 4.5.0 (2025-04-11) -- "How About a Twenty-Six"

Checks made using R-CMD-check.yaml GitHub Actions on the repo for environments: 
linux, macos, windows. 

## Downstream dependencies

All downstream dependencies have been checked.

> revdepcheck::revdep_check()
── INIT ────────────────────────────────────────────── Computing revdeps ──
── INSTALL ────────────────────────────────────────────────── 2 versions ──
Installing CRAN version of tourr
Installing DEV version of tourr
Installing 1 packages: spatstat.univar
── CHECK ─────────────────────────────────────────────────── 15 packages ──
✔ cheem 0.4.0.0                          ── E: 1     | W: 0     | N: 0     
✔ composits 0.1.1                        ── E: 1     | W: 0     | N: 0     
✔ detourr 0.1.0                          ── E: 0     | W: 0     | N: 1     
✔ ferrn 0.1.0                            ── E: 0     | W: 0     | N: 1     
✔ geozoo 0.5.1                           ── E: 0     | W: 0     | N: 0     
✔ langevitour 0.8.1                      ── E: 0     | W: 0     | N: 0     
✔ liminal 0.1.2                          ── E: 0     | W: 0     | N: 0     
✔ lionfish 1.0.27                        ── E: 0     | W: 0     | N: 0     
I loon.tourr 0.1.4                       ── E: 1     | W: 0     | N: 0     
✔ mulgar 1.0.5                           ── E: 0     | W: 0     | N: 0     
✔ PPbigdata 1.0.0                        ── E: 0     | W: 0     | N: 0     
I REPPlab 0.9.6                          ── E: 1     | W: 0     | N: 0     
✔ spinebil 0.1.6                         ── E: 0     | W: 0     | N: 0     
✔ spinifex 0.3.8                         ── E: 0     | W: 0     | N: 0     
✔ woylier 0.0.9                          ── E: 0     | W: 0     | N: 0     
OK: 15                                                                   
BROKEN: 0
Total time: 19 min
── REPORT ─────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
Writing CRAN report to 'revdep/cran.md'
