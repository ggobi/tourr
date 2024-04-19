# tourr 1.2.0 

* major change: rescale is now FALSE by default.
* flea is now standardised measurements, and flea_raw is the original units.
* TeachingDemos removed as a Suggests, and replaced with aplpack for drawing Chernoff faces.
* addition of a pre-specified ellipse can be added to the 2D display.
* palette can now be a vector of values.
* a new projection pursuit index for finding anomalies relative to a null variance-covariance matrix. May still need more work.
* point shapes can now be specified like palettes

# tourr 1.1.0 

* Updated version to indicate some nice new additions for the package

# tourr 1.0.2

* Fix to duplicating bases during interpolate
* New display type: animate_idx

# tourr 1.0.1

* Edges now working correctly in display_groupxy 
* Interpolate had an error in calculating distance between planes which is fixed

# tourr 1.0.0

* Small changes in prep for CRAN
* Time for it to be a full version number

# tourr 0.6.18

* Changed line colours to take a factor, and be handled the same as points.

# tourr 0.6.17

* Fixed sizing issue in display_trails

# tourr 0.6.16

* Set rescale default to be FALSE

# tourr 0.6.15

* display_dist() has controls to change binwidth, and to scale the height of the bars or density in each projection.

# tourr 0.6.14

* Finally modified density plot to include colours

# tourr 0.6.13

* Added palette option display functions

# tourr 0.6.12

* Fixed a bug in display_slice(), display_density2d(), display_groupxy(), display_pca(), display_sage(), manual_slice(), caused by changes in handling colour vector in display_xy()

# tourr 0.6.11

* Added line with for edges to display_xy()

# tourr 0.6.10

* Changed default colour scale

# tourr 0.6.9

* Bug fixes

# tourr 0.6.8

* Support for displaying point labels

# tourr 0.6.7

* Color and shape legend added for animate_xy

# tourr 0.6.6

* Added a render_anim to create a plotly animation

# tourr 0.6.5

* Fixed a bug in save_history so that if start is provided it doesn't get duplicated
* Fix to is_orthonormal so it doesn't just stop if not a matrix

# tourr 0.6.4

* Added routine to draw projected data and axes

# tourr 0.6.3

* Added argument to render tours as gifs without looping

# tourr 0.6.2

* Shorten run time of several examples
* Changed most dontrun to donttest
* Fix examples based on CRAN checks
* splines2d force data to be data frame for model
* splines2d function fixed to pass in the data as required by mgcv::gam
* Handle problem of mismatching number of frames in render_gif
* Slice center navigation now has positions
* Fixed bug in slice tour when anchor was not provided
* Added slice center guide
* Adding manual slicing
* Error fixed in is_orthonormalise(), which is to check whether a vector is normalised 
* Error fixed in orthonormalise_by(), which needed to run over columns of new matrix, too

# tourr 0.6.1

* A radial tour is added that will allow a variable to be zero'd from a projection.

* Fixes made according to CRAN requests

# tourr 0.6.0

* Two vignettes have been added to help users get started.

* Little tour now cycles twice through variable list, in a randomised order.

* Two new projection pursuit indexes have been added, based on function from the archived package mbgraphic.

* A slice tour is now available using the function animate_slice(). A new index for searching for interesting slices is available.

* A principal component analysis tour display is now available, that renders the axes of the original variables. This requires appropriate scaling of data ahead of time. See the example code.

* A new record option is available to print out diagnostic information for every step. Bind the animate code to a name will store the record object. The ferrn package at https://github.com/huizezhang-sherry/ferrn can be used to diagnose the optimisation techniques.

* Improvements to the optimisation functions have been made: search_better has an interrupt if a higher index is achieved mid-path; search_polish takes a best basis from another routine, and makes small steps to get a very best projection close by.

* Set the default fps to 10, to compatibly draw with RStudio window.

# tourr 0.5.6

* Added a `NEWS.md` file to track changes to the package.

* Indexes holes and cmass functions now follow the same style as lda_pp and pda_pp, which means that you need to use them with holes(), and cmass()

* A new display type, groupxy has been added, which will run the same tour on subsets of the data in a split plot window


