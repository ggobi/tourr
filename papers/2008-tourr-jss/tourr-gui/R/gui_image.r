library(RGtk2)
library(gWidgets)


gui_image <- function(data = ozone, ...) {
  os <- find_platform()$os
  num <- sapply(data, is.numeric)
  
  tour <- NULL
  tour_anim <- NULL
  update_tour <- function(...) {
    tour <<- create_tour(data,
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

  # Tour selection column
  vbox[1, 1, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox[2, 1] <- TourType <- gradio(tour_types)

  # speed and pause
  vbox[3,1, anchor = c(-1, 0)] <- "Speed"
  vbox[4,1, expand = TRUE] <- sl <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox[1, 2] <- chk_pause <- gcheckbox("Pause", 
    handler = function(h, ...) pause(svalue(h$obj)))

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
  gbutton("Apply", cont = buttonGroup, handler = update_tour)
  
  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup, handler = function(...) {
    pause(TRUE)
    dispose(w)
  })

  vbox[2, 2, anchor = c(0, 1)] <- buttonGroup
  
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


create_tour <- function(data, tour_type, aps) {
 if (aps > 9999) {
   gmessage("Please quit", icon = "warning")
   return()
 }

  xs <- dim(data)[1]
  ys <- dim(data)[2]
  zs <- dim(data)[3]
  dim(data) <- c(xs * ys, zs)

  display <- display_image(data, xs, ys)
  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(1), 
    "Little" = little_tour(), 
    "Guided(holes)" = guided_tour(holes), 
    "Guided(cm)" = guided_tour(cm), 
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected])),
    "Local" = local_tour()
  )
      
  list(
    data = rescale(data),
    tour_path = tour,
    display = display,
    aps = aps
  )
}