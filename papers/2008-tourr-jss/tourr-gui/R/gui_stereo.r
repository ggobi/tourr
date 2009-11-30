library(RGtk2)
library(gWidgets)


gui_stereo <- function(data = flea, ...) {
  os <- find_platform()$os
  num <- sapply(data, is.numeric)
  
  tour <- NULL
  tour_anim <- NULL
  update_tour <- function(...) {
    tour <<- create_tour(data,
      var_selected = svalue(Variables),
      tour_type = svalue(TourType),
      aps = svalue(sl)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }
  
  draw_frame <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(TRUE)  

    tour_step <- tour_anim$step2(svalue(sl) / 33)
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
  vbox[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox[2, 1] <- Variables <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)

  # Tour selection column
  vbox[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox[2, 3] <- TourType <- gradio(tour_types)


  # speed and pause
  vbox[3,1, anchor = c(-1, 0)] <- "Speed"
  vbox[4,1, expand = TRUE] <- sl <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox[4, 3] <- chk_pause <- gcheckbox("Pause", 
    handler = function(h, ...) pause(svalue(h$obj)))

  # buttons control
  anim_id <- NULL
  pause <- function(paused) {
    svalue(chk_pause) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  buttonGroup <- ggroup(horizontal = F, cont=vbox)  
  
  # addSpace(buttonGroup,10)
  gbutton("Apply", cont = buttonGroup, handler = function(...) {
    pause(FALSE)
    update_tour()
  })

  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup, handler = function(...) {
    pause(TRUE)
    dispose(w)
  })

  vbox[2:3, 4, anchor = c(0, -1)] <- buttonGroup
  
  # If on a mac, open a Cairo device, if there's not already one open
  # The cairo device has a much better refresh rate than Quartz
  if (find_platform()$os == "mac" && names(dev.cur()) != "Cairo") {
    require(Cairo)
    CairoX11()
  }
  
  update_tour()
  pause(FALSE)
  visible(w) <- TRUE
  
  invisible()
}


create_tour <- function(data, var_selected, tour_type, aps) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }

  blue = rgb(0, 0.91, 0.89)
  red = rgb(0.98, 0.052, 0)
  display <- display_stereo(data,tour_path=tour_type,blue,red)


  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(3),
    "Little" = little_tour(3),
    "Guided(holes)" = guided_tour(holes,3), 
    "Guided(cm)" = guided_tour(cm,3),
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected]),3),
    "Local" = local_tour(3)
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