## Overview

Some minor changes. Small bug fixes. A new optimiser added.

- With devtools:check() 

── R CMD check results ───────────────────────────────────────────── tourr 1.2.4 ────
Duration: 1m 25s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

- With R CMD CHECK ../tourr_1.2.4.tar.gz 

* DONE

Status: OK

## Test environment

* R version 4.4.2 (2024-10-31) -- "Pile of Leaves"

Checks made using R-CMD-check.yaml GitHub Actions on the repo for environments: 
linux, macos, windows. It fails on the vignettes due to rmarkdown 
not being available on GitHub, beyond my control, but all other checks pass. 

## Downstream dependencies

All downstream dependencies have been checked.

> revdepcheck::revdep_check()
── INIT ───────────────────────────────── Computing revdeps ──
── INSTALL ───────────────────────────────────── 2 versions ──
Installing CRAN version of tourr
Installing DEV version of tourr
── CHECK ───────────────────────────────────────────────────────────── 12 packages ──
✔ cheem 0.4.0.0                          ── E: 1     | W: 0     | N: 0               
✔ composits 0.1.1                        ── E: 1     | W: 0     | N: 0               
✔ detourr 0.1.0                          ── E: 0     | W: 0     | N: 2               
✔ ferrn 0.1.0                            ── E: 0     | W: 0     | N: 0               
✔ geozoo 0.5.1                           ── E: 0     | W: 0     | N: 0               
✔ liminal 0.1.2                          ── E: 0     | W: 0     | N: 0               
I loon.tourr 0.1.4                       ── E: 1     | W: 0     | N: 0               
✔ mulgar 1.0.2                           ── E: 0     | W: 0     | N: 0               
✔ PPbigdata 1.0.0                        ── E: 0     | W: 0     | N: 0               
I REPPlab 0.9.6                          ── E: 1     | W: 0     | N: 0               
✔ spinifex 0.3.8                         ── E: 0     | W: 0     | N: 0               
✔ woylier 0.0.9                          ── E: 0     | W: 0     | N: 0               
OK: 12                                                                             
BROKEN: 0
Total time: 17 min
