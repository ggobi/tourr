## Test environments
* local OS X install, R 3.1.2
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
I have also run R CMD check on four all downstream dependencies of tourr 
(https://github.com/hadley/tourr/blob/master/revdep/summary.md). I get a lot of notes, but none of them seem to be related to tourr. The only changes in this version were to fix `R CMD check` NOTEs. I've notified the maintainers of the problems.
