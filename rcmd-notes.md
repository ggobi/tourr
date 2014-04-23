This release adds no new functionality, but removes a rggobi dependency and  fixes a number of outstanding R CMD check notes. Results from testing the dependencies can be found at: XXX. As far as I can there were no significant new problems.

One significant note is generated across my local OS X install, ubuntu running on travis-ci and win builder:

* checking R code for possible problems ... NOTE
  display_trails : render_data: no visible binding for '<<-' assignment 
  to 'past_x'
  
  This note arises because I'm doing some non-standard stuff with
  environments and the heuristics can't see where `past_x` is coming
  from. The code is correct.

