#' A graphical user interface enabling interactive control of scatterplot tour.
#'
#' @param data matrix, or data frame containing numeric columns, defaults to flea dataset
#' @param ... other arguments passed on to \code{\link{animate}} and \code{\link{display_xy}}
#' @param tour_path tour path, defaults to the grand tour
#' @author Di Cook \email{dicook@@iastate.edu} and Bei Huang\email{beihuang@@iastate.edu}
#' @keywords display_xy
#' @examples
#' gui_xy()

gui_xy <- function(data = flea, ...) {
  require("colorspace")
  require("RGtk2")
  require("gWidgets")

  os <- find_platform()$os
  num <- sapply(data, is.numeric)
  
  tour <- NULL
  tour_anim <- NULL
  update_tour <- function(...) {
    tour <<- create_tour(data,
      var_selected = svalue(Variables), 
      cat_selected = svalue(Class), 
      axes_location = svalue(dl),
      tour_type = svalue(TourType),
      guided_type = svalue(GuidedType),
      lambda = svalue(LambdaValue),
      aps = svalue(sl)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }
  
  draw_frame <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)  

    tour_step <- tour_anim$step2(svalue(sl) / 33)
    if (is.null(tour_step$proj)) return(FALSE)
    
    if (os == "win") {
      tour$display$render_frame()
    } else {
      tour$display$render_transition()      
    }
    with(tour_step, tour$display$render_data(tour$data, proj, target))
    Sys.sleep(1/33)
    
    TRUE
  }
  
  
  # ==================Controls==========================
  w <- gwindow("2D Tour plot example", visible = FALSE)
  vbox <- glayout(cont = w)

  # Variable selection column
  # Divide all the variable names into two parts according to numeric variables and categorical variables.
  # The numeric variable names go under Variable Selection column, 
  # and the categorical variable names go under Class Selection column.

  vbox[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox[2, 1] <- Variables <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)
  tooltip(Variables) <- "Select variables to display in the 2D Tour."

  vbox[3, 1, anchor = c(-1, 0)] <- "Class Selection"
  vbox[4, 1, anchor = c(-1, 0)] <- Class <- gtable(names(data)[!num], 
    multiple = TRUE)
  tooltip(Class) <- "Select a class variable to color the points."


  # Tour selection column
  # The are seven kinds of tour type which allow users to switch willingfully.
 
  vbox[1, 2, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Local", "Guided")
  vbox[2, 2] <- TourType <- gradio(tour_types)
  tooltip(TourType) <- "Select a 2D Tour type."

  # Guided Tour index selection column
  # Indices including "holes", "cm", "lda", "pda".

  vbox[3, 2, anchor=c(-1, 0)] <- "Guided indices"
  IntIndex <-c("holes","cm","lda_pp","pda_pp")
  vbox[4, 2, anchor=c(-1,-1)] <-  GuidedType <- gdroplist(IntIndex)
  tooltip(GuidedType) <- "Select an index type for guided tour."
  
  # Lambda selection column
  # Lambda's range is from 0 to 1.

  vbox[3, 3, anchor=c(-1, 0)] <-"Lambda"
  vbox[4, 3] <- LambdaValue <- gspinbutton(from=0, to = 1, by = 0.01)
  tooltip(LambdaValue) <- "Select lambda's value to calculate pda index."

  # speed and pause
  # This slider can control the speed of the 2D tour, which ranged from 0 to 5.

  vbox[5,1, anchor = c(-1, 0)] <- "Speed"
  vbox[6,1, expand = TRUE] <- sl <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  tooltip(sl) <- "Drag to set the speed of the 2D Tour."
 
  # Pause box allow users to pause the dynamic 2D tour and have a close examination on the details.
  vbox[6, 2] <- chk_pause <- gcheckbox("Pause", 
    handler = function(h, ...) pause(svalue(h$obj)))
  tooltip(chk_pause) <- "Click here to pause or continue the 2D Tour."

  # axes control
  # There are three kinds of axes locations which allow users to switch willingfully.
  vbox[1,3, anchor=c(-1,0)] <- "Axes Locations"
  locations <- c("center", "bottomleft", "off")
  vbox[2,3, anchor=c(-1,0)] <- dl <- gradio(locations)
  tooltip(dl) <- "Select a location for the 2D Tour axes."

  # buttons control
  anim_id <- NULL
  pause <- function(paused) {
    svalue(chk_pause) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <<- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  
  buttonGroup <- ggroup(horizontal = F, cont=vbox)  
  
  # addSpace(buttonGroup,10)
  button1<- gbutton("Apply", cont = buttonGroup, handler = function(...) {
    pause(FALSE)
    update_tour()
  })
  tooltip(button1) <- "Click here to update the options."
 
  # addSpace(buttonGroup,10)
  button2<-gbutton("Quit",cont=buttonGroup, handler = function(...) {
    pause(TRUE)
    dispose(w)
  })
  tooltip(button2) <- "Click here to close this window."
 

  # addSpace(buttonGroup,10)
  message1<-gbutton("Help",cont=buttonGroup, handler = function(...) {
gmessage("The tour is a movie of low dimensional projections of high dimensional data. The projections are usually 1-, 2-, or 3-dimensional. They areused to expose interesting features of the high-dimensional data, such as outliers, clusters, and nonlinear dependencies.

When the projection dimension is 2, the data is usually shown as a scatterplot. Densities or histograms are used to display 1-dimensional projections. Projections of 3 or higher dimensions can be shown as stereo, parallel coordinates, scatterplot matrices or icons.

There are several different types of tours: grand, guided, little, and local. The grand tour generates a random path, while the guided uses an index on interest such as holes, central mass, lda or pda to guide the choice of projections to particular structure. The little tourmoves between existing variables, only covering a subset of all the space. The local tour contrains the choice of projection to be those near the current view.

The GUI allows user to control the tour by checkboxes for the variable selection, slider for the speed, and toggle boxes for pause.",
title="gui_help",icon="info")
  })
tooltip(message1) <- "Click here for help."


  vbox[5:6, 3, anchor = c(0, 1)] <- buttonGroup
  
  # If on a mac, open a Cairo device, if there's not already one open
  # The cairo device has a much better refresh rate than Quartz
  if (find_platform()$os == "mac" && names(dev.cur()) != "Cairo") {
    require(Cairo)
    CairoX11()
  } else if (length(dev.list()) == 0) {
    # Open new display if necessary
    dev.new()
    # Turn off display list to maximise speed
    dev.control(displaylist = "inhibit")
  }
  
  update_tour()
  pause(FALSE)
  visible(w) <- TRUE
  
  invisible()
}


create_tour <- function(data, var_selected, cat_selected, axes_location, tour_type, guided_type, lambda, aps) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }
  
  # Work out point colours
  cat <- data[cat_selected]
  if (length(cat_selected) > 0) {
    # collapse to single variable if multiple selected
    int <- interaction(cat, drop = TRUE)
    pal <- rainbow_hcl(length(levels(int)))
    col <- pal[as.numeric(int)]
  } else {
    col <- "black"
  }
  
  display <- display_xy(data, axes = axes_location, center = TRUE, 
    col = col)

  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(), 
    "Little" = little_tour(), 
    "Guided" = switch(guided_type, "holes"=guided_tour(holes), 
				"cm"=guided_tour(cm),
				"lda_pp" = guided_tour(lda_pp(data[,cat_selected])),
				"pda_pp" = guided_tour(pda_pp(data[,cat_selected],lambda))),
    "Local" = local_tour()
  )
  
  
  sel <- data[var_selected]
  # Sphere the data if we're using a guided tour
  if (length(grep(tour_type, "Guided")) > 0) {
    sel <- sphere(sel)
  }
  
  list(
    data = rescale(sel),
    tour_path = tour,
    display = display,
    aps = aps
  )
}

