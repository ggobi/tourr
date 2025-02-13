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

* R version 4.3.3 (Angel Food Cake) 
* Windows: Using https://win-builder.r-project.org/
* Linux: Using `rhub::rc_submit()`

## Downstream dependencies

All downstream dependencies have been checked.

> revdepcheck::revdep_check()
── INIT ───────────────────────────────── Computing revdeps ──
── INSTALL ───────────────────────────────────── 2 versions ──
Installing CRAN version of tourr
Installing DEV version of tourr
── CHECK ────────────────────────────────────── 12 packages ──
✔ cheem 0.4.0.0                          ── E: 0     | W: 0     | N: 0    
✔ composits 0.1.1                        ── E: 0     | W: 0     | N: 0    
✔ detourr 0.1.0                          ── E: 0     | W: 0     | N: 1    
I diveR 0.1.2                            ── E: 1     | W: 0     | N: 0    
✔ ferrn 0.0.2                            ── E: 0     | W: 0     | N: 0    
✔ geozoo 0.5.1                           ── E: 0     | W: 0     | N: 0    
✔ liminal 0.1.2                          ── E: 0     | W: 0     | N: 0    
I loon.tourr 0.1.4                       ── E: 1     | W: 0     | N: 0    
✔ mulgar 1.0.2                           ── E: 0     | W: 0     | N: 0    
I REPPlab 0.9.6                          ── E: 1     | W: 0     | N: 0    
✔ spinifex 0.3.7.0                       ── E: 0     | W: 0     | N: 0    
✔ woylier 0.0.5                          ── E: 0     | W: 0     | N: 0    
OK: 12                                                      
BROKEN: 0
Total time: 12 min
── REPORT ────────────────────────────────────────────────────
