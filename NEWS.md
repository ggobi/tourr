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


