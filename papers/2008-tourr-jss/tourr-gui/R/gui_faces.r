library(RGtk2)
library(gWidgets)
library(TeachingDemos)


gui_faces <- function(data = flea, ...) {
  os <- find_platform()$os
  num <- sapply(data, is.numeric)
  
  tour <- NULL
  tour_anim <- NULL
  update_tour <- function(...) {
    tour <<- create_tour(data,
      var_selected = svalue(Variables), 
      VarIndex = svalue(Variables, index = T),
      dim_selected = svalue(Dimensions),
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
  vbox[3,3, anchor = c(-1, 0)] <- "Speed"
  vbox[4,3, expand = TRUE] <- sl <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox[4, 4] <- chk_pause <- gcheckbox("Pause", 
    handler = function(h, ...) pause(svalue(h$obj)))
 
  # dimension control
  vbox[3, 1, anchor = c(-1, 0)] <- "Choose Dimension"
  dimensions <- c(2:length(data[num]))
  vbox[4, 1, anchor = c(-1, 0)] <- Dimensions <- gradio(dimensions)
 
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
  gbutton("Apply", cont = buttonGroup, handler = function(...){
    pause(FALSE)
    update_tour()
  })

  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup, handler = function(...) {
    pause(TRUE)
    dispose(w)
  })

  vbox[3, 4, anchor = c(0, 1)] <- buttonGroup
  
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


create_tour <- function(data, var_selected, VarIndex, dim_selected, tour_type, aps) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }

  display <- display_faces(data, axes = axes_location)
    

  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(as.numeric(dim_selected)), 
    "Little" = little_tour(as.numeric(dim_selected)), 
    "Guided(holes)" = guided_tour(holes,as.numeric(dim_selected)), 
    "Guided(cm)" = guided_tour(cm,as.numeric(dim_selected)), 
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected]),as.numeric(dim_selected)),
    "Local" = local_tour(as.numeric(dim_selected))
  )
  
  sel <- data[VarIndex,1:6]
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