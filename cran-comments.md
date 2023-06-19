## Overview

This is a fairly major update, with new display options and new parameters on many display options. There are no major structural changes to the code.

\dontrun is used for example code where the method would break, and thus the code should not be run. It is important for users to be able to see this example.

## Test environment

* local R installation: R version 4.2.1 (2022-06-23)
* Windows Server 2022, R-devel, 64 bit
* Fedora Linux, R-devel, clang, gfortran
* Ubuntu Linux 20.04.1 LTS, R-release, GCC

## ── R CMD check results ──────────────────────── tourr 1.0.0 ────
Duration: 1m 16.8s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream dependencies

All downstream dependencies have been checked.

> revdepcheck::revdep_check()
── CHECK ──────────────────────────────────────── 11 packages ──
✔ cheem 0.3.0                            ── E: 0     | W: 0     | N: 0    
✔ composits 0.1.1                        ── E: 1     | W: 0     | N: 0    
✔ detourr 0.1.0                          ── E: 0     | W: 0     | N: 1    
✔ diveR 0.1.2                            ── E: 1     | W: 0     | N: 0    
✔ ferrn 0.0.2                            ── E: 0     | W: 0     | N: 0    
✔ geozoo 0.5.1                           ── E: 0     | W: 0     | N: 0    
✔ liminal 0.1.2                          ── E: 0     | W: 0     | N: 0    
✔ loon.tourr 0.1.3                       ── E: 1     | W: 0     | N: 0    
I REPPlab 0.9.4                          ── E: 1     | W: 0     | N: 0    
✔ spinifex 0.3.6                         ── E: 0     | W: 0     | N: 0    
✔ woylier 0.0.5                          ── E: 0     | W: 0     | N: 0    
OK: 11                                                        
BROKEN: 0
Total time: 17 min

REPPlab cannot be fully checked. It doesn't Depend but only Suggests the tourr package.
