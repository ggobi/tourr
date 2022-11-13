## Overview

This is a small updated, with several new methods added, and multiple fixes.

Although the New Maintainer is flagged, the maintainer is the same. The only change is that full name is now used, so that it matches the ORCID records. 

\dontrun is used for example code where the method would break, and thus the code should not be run. It is important for users to be able to see this example.

## Test environment

* local R installation: R version 4.2.1 (2022-06-23)
* Windows Server 2022, R-devel, 64 bit
* Fedora Linux, R-devel, clang, gfortran
* Ubuntu Linux 20.04.1 LTS, R-release, GCC

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream dependencies

All downstream dependencies have been checked.

> revdep_check()
── INIT ────────────────────────────── Computing revdeps ──
── INSTALL ────────────────────────────────── 2 versions ──
Installing CRAN version of tourr
Installing DEV version of tourr
── CHECK ─────────────────────────────────── 10 packages ──
✔ cheem 0.2.0                            ── E: 0     | W: 0     | N: 1    
✔ composits 0.1.1                        ── E: 0     | W: 0     | N: 0    
✔ detourr 0.1.0                          ── E: 0     | W: 0     | N: 1    
✔ diveR 0.1.2                            ── E: 1     | W: 0     | N: 0    
✔ ferrn 0.0.2                            ── E: 0     | W: 0     | N: 0    
✔ geozoo 0.5.1                           ── E: 0     | W: 0     | N: 0    
✔ liminal 0.1.2                          ── E: 0     | W: 0     | N: 0    
✔ loon.tourr 0.1.3                       ── E: 1     | W: 0     | N: 0    
I REPPlab 0.9.4                          ── E: 1     | W: 0     | N: 0    
✔ spinifex 0.3.6                         ── E: 0     | W: 0     | N: 0    
OK: 10                                                   
BROKEN: 0
