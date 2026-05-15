## Overview

* New argument for the little tour to run sequentially through the variables.
* Animations in vignettes changed, because they throw an error on Windows and linux.
* Changes in interpolate to better handle the dimension of the history array for Windows and linux.
* Change qplot to ggplot in the path index plots
* A lot of new tests have been added, thanks to Claude
* Several new vignettes on usage added, also thanks to Claude.

## Platform checks

- With devtools:check() 

── R CMD check results ─────────────────────────────────── tourr 1.2.7 ────
Duration: 2m 21.6s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Checks for Windows, linux and MacOS made using rhub::rhub_check() See https://github.com/ggobi/tourr/actions/runs/25905058431

## Downstream dependencies

All downstream dependencies have been checked.

# pak::pkg_install("r-lib/revdepcheck")
> revdepcheck::revdep_check()
> revdepcheck::revdep_check()
── INIT ───────────────────────────────────────────────── Computing revdeps ──
── INSTALL ───────────────────────────────────────────────────── 2 versions ──
Installing CRAN version of tourr
also installing the dependencies ‘R.oo’, ‘R.methodsS3’, ‘spatstat.data’, ‘spatstat.univar’, ‘spatstat.utils’, ‘polyclip’, ‘farver’, ‘labeling’, ‘RColorBrewer’, ‘viridisLite’, ‘utf8’, ‘withr’, ‘cpp11’, ‘R.utils’, ‘sgeostat’, ‘spatstat.geom’, ‘spatstat.random’, ‘sp’, ‘deldir’, ‘RcppEigen’, ‘gtable’, ‘isoband’, ‘S7’, ‘scales’, ‘crayon’, ‘hms’, ‘prettyunits’, ‘cli’, ‘lifecycle’, ‘magrittr’, ‘pillar’, ‘pkgconfig’, ‘rlang’, ‘vctrs’, ‘generics’, ‘glue’, ‘R6’, ‘tidyselect’, ‘Rcpp’, ‘gsl’, ‘bitops’, ‘igraph’, ‘alphahull’, ‘splancs’, ‘interp’, ‘ggplot2’, ‘progress’, ‘RcppArmadillo’, ‘tibble’, ‘dplyr’, ‘ash’, ‘energy’, ‘geozoo’, ‘cassowaryr’, ‘minerva’

Installing DEV version of tourr
Installing 55 packages: Rcpp, withr, vctrs, rlang, lifecycle, glue, cli, utf8, pkgconfig, pillar, magrittr, R6, prettyunits, hms, crayon, viridisLite, RColorBrewer, labeling, farver, cpp11, scales, S7, isoband, gtable, tidyselect, tibble, generics, gsl, RcppEigen, deldir, sp, polyclip, spatstat.utils, spatstat.univar, spatstat.data, R.methodsS3, R.oo, splancs, spatstat.random, spatstat.geom, sgeostat, R.utils, interp, ggplot2, RcppArmadillo, progress, dplyr, energy, alphahull, igraph, bitops, minerva, cassowaryr, geozoo, ash
── CHECK ────────────────────────────────────────────────────── 16 packages ──
✔ cheem 0.4.2                            ── E: 1     | W: 0     | N: 0        
✔ detourr 0.2.0                          ── E: 0     | W: 0     | N: 0        
✔ ferrn 0.3.0                            ── E: 0     | W: 0     | N: 0        
✔ geozoo 0.5.1                           ── E: 0     | W: 0     | N: 0        
✔ langevitour 0.8.1                      ── E: 0     | W: 0     | N: 0        
✔ lionfish 1.0.27                        ── E: 0     | W: 0     | N: 0        
I loon.tourr 0.1.5                       ── E: 1     | W: 0     | N: 0        
✔ mulgar 1.0.5                           ── E: 0     | W: 0     | N: 0        
✔ pandemonium 0.2.4                      ── E: 0     | W: 0     | N: 0        
✔ polarisR 0.1.4                         ── E: 0     | W: 0     | N: 0        
✔ PPbigdata 1.0.0                        ── E: 0     | W: 0     | N: 0        
✔ prefviz 0.1.2                          ── E: 0     | W: 0     | N: 0        
I REPPlab 0.9.6                          ── E: 1     | W: 0     | N: 0        
✔ spinebil 1.0.5                         ── E: 0     | W: 0     | N: 0        
✔ spinifex 0.3.10                        ── E: 0     | W: 0     | N: 0        
✔ woylier 0.0.9                          ── E: 0     | W: 0     | N: 0        
OK: 16                                                                      

BROKEN: 0
Total time: 23 min
── REPORT ────────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
Writing CRAN report to 'revdep/cran.md'
