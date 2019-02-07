## Test environments
* local OS X install, R 3.5.2
* win-builder (devel)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

I checked all three downstream dependencies: geozoo, REPPlab, tourGui. There are no problems with geozoo. REPPlab only suggests tourr, so we have notified the authors of changes in the tourr and how to make minor updates their code. The touGui package depends on RGtk2 which is only available on linux and Mac, and a new version of this package is also being submitted to CRAN.
