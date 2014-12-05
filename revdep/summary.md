# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.1.2 (2014-10-31) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (0.99.85)            |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |

## Packages

|package       |*  |version |date       |source         |
|:-------------|:--|:-------|:----------|:--------------|
|ash           |*  |1.0-14  |2013-02-11 |CRAN (R 3.1.0) |
|colorspace    |*  |1.2-4   |2013-09-30 |CRAN (R 3.1.0) |
|ggplot2       |*  |1.0.0   |2014-05-21 |CRAN (R 3.1.0) |
|reshape2      |*  |1.4     |2014-04-23 |CRAN (R 3.1.0) |
|TeachingDemos |*  |2.9     |2013-01-21 |CRAN (R 3.1.0) |
|testthat      |   |0.9.1   |2014-10-01 |CRAN (R 3.1.1) |

# Check results
4 checked out of 4 dependencies 

## cepp (1.0)
Maintainer: Mohit Dayal <mohitdayal2000@gmail.com>

```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘randtoolbox’ which was already attached by Depends.
  Please remove these calls from your code.
See the information on DESCRIPTION files in the chapter ‘Creating R
packages’ of the ‘Writing R Extensions’ manual.
```
```
checking R code for possible problems ... NOTE
geodesic_path: no visible global function definition for ‘freeze’
geodesic_path : interpolate: no visible global function definition for
  ‘thaw’
```
```
checking Rd line widths ... NOTE
Rd file 'data-olive.Rd':
  \examples lines wider than 100 characters:
     o1   <- optim(par=F1, fn=oil1,gr=basis_nearby,method='SANN',control=list(fnscale=-1,maxit=50,trace=6))

Rd file 'pp.Rd':
  \examples lines wider than 100 characters:
     o1 <- optim(par=F1,fn=ranif1,gr=basis_nearby,method='SANN',control=list(fnscale=-1,maxit=200,trace=1))

These lines will be truncated in the PDF manual.
```

## geozoo (0.4.3)
Maintainer: Barret Scloerke <schloerke@gmail.com>

```
checking dependencies in R code ... NOTE
Package in Depends field not imported from: ‘bitops’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
See the information on DESCRIPTION files in the chapter ‘Creating R
packages’ of the ‘Writing R Extensions’ manual.
```
```
checking R code for possible problems ... NOTE
.cube.wires: no visible global function definition for ‘bitXor’
```

## REPPlab (0.9)
Maintainer: Daniel Fischer <daniel.fischer@uta.fi>

```
checking whether package ‘REPPlab’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/Rtmpvp5xPL/check_cran9c5c57c7ffa2/REPPlab.Rcheck/00install.out’ for details.
```

## tourrGui (0.4)
Maintainer: Dianne Cook <dicook@iastate.edu>

```
checking package dependencies ... ERROR
Packages required but not available: ‘RGtk2’ ‘gWidgets’

Package suggested but not available for checking: ‘rggobi’

See the information on DESCRIPTION files in the chapter ‘Creating R
packages’ of the ‘Writing R Extensions’ manual.
```

