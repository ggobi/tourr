library(colorspace)
library(RGtk2)
library(gWidgets)
library(ash)
library(TeachingDemos)


gui_tour<- function(data = flea, ...) {
  os <- find_platform()$os
  num <- sapply(data, is.numeric)
  tour <- NULL
  tour_anim <- NULL

  w <- gwindow("2D Tour plot example", visible = FALSE)
  mw = gnotebook(cont=w)  
  g8 = ggroup(cont = mw, horizontal = FALSE,label="gui_scatmat") 
  g7 = ggroup(cont = mw, horizontal = FALSE,label="gui_pcp")
  g6 = ggroup(cont = mw, horizontal = FALSE,label="gui_stereo")
  g5 = ggroup(cont = mw, horizontal = FALSE,label="gui_andrews")
  g4 = ggroup(cont = mw, horizontal = FALSE,label="gui_stars")
  g3 = ggroup(cont = mw, horizontal = FALSE,label="gui_faces")
  g2 = ggroup(cont = mw, horizontal = FALSE,label="gui_density")
  g1 = ggroup(cont = mw, horizontal = FALSE,label="gui_xy")
# =============================== Gui_faces====================
  update_tour_faces <- function(...) {
    tour <<- create_tour_faces(data,
      var_selected = svalue(Variables_faces), 
      VarIndex = svalue(Variables_faces, index = T),
      dim_selected = svalue(Dimensions_faces),
      tour_type = svalue(TourType_faces),
      aps = svalue(sl_faces)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }

# ==================Controls==========================
vbox_faces <- glayout(cont = g3)
  # Variable selection column
  vbox_faces[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_faces[2, 1] <- Variables_faces <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)

  # Tour selection column
  vbox_faces[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox_faces[2, 3] <- TourType_faces <- gradio(tour_types)

  # speed and pause
  vbox_faces[3,3, anchor = c(-1, 0)] <- "Speed"
  vbox_faces[4,3, expand = TRUE] <- sl_faces <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox_faces[4, 4] <- chk_pause_faces <- gcheckbox("Pause", 
    handler = function(h, ...) pause_faces(svalue(h$obj)))
 
  # dimension control
  vbox_faces[3, 1, anchor = c(-1, 0)] <- "Choose Dimension"
  dimensions <- c(2:length(data[num]))
  vbox_faces[4, 1, anchor = c(-1, 0)] <- Dimensions_faces <- gradio(dimensions)
 
  # buttons control
  anim_id <- NULL
  pause_faces <- function(paused) {
    svalue(chk_pause_faces) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <<- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  buttonGroup_faces <- ggroup(horizontal = F, cont=vbox_faces)  
  
  # addSpace(buttonGroup,10)
  gbutton("Apply", cont = buttonGroup_faces, handler = function(...){
    print("apply from gui_faces")
    pause_faces(FALSE)
    update_tour_faces()
  })

  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_faces, handler = function(...) {
    pause_faces(TRUE)
    dispose(w)
  })

  vbox_faces[3, 4, anchor = c(0, 1)] <- buttonGroup_faces
# ============================ End of Gui_faces ================

# =============================== Gui_scatmat==================
  update_tour_scatmat <- function(...) {
    tour <<- create_tour_scatmat(data,
      var_selected = svalue(Variables_scatmat),
      projdim_selected = svalue(Projections_scatmat), 
      tour_type = svalue(TourType_scatmat),
      aps = svalue(sl_scatmat)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
  # tour$display$render_frame()
    
    TRUE
  }
# ==================Controls==========================
vbox_scatmat <- glayout(cont = g8)
  # Variable selection column
  vbox_scatmat[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_scatmat[2, 1] <- Variables_scatmat <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)

  # Tour selection column
  vbox_scatmat[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox_scatmat[2, 3] <- TourType_scatmat <- gradio(tour_types)

  # dimension control
  vbox_scatmat[3, 1, anchor = c(-1, 0)] <- "Choose Projection Dimension"
  projections <- c(2:length(data[num]))
  vbox_scatmat[4, 1, anchor = c(-1, 0)] <- Projections_scatmat <- gradio(projections)

  # speed and pause
  vbox_scatmat[3,3, anchor = c(-1, 0)] <- "Speed"
  vbox_scatmat[4,3, expand = TRUE] <- sl_scatmat <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox_scatmat[4, 4] <- chk_pause_scatmat <- gcheckbox("Pause", 
    handler = function(h, ...) pause_scatmat(svalue(h$obj)))

  # buttons control
  anim_id <- NULL
  pause_scatmat <- function(paused) {
    svalue(chk_pause_scatmat) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  buttonGroup_scatmat <- ggroup(horizontal = F, cont=vbox_scatmat)  
  
  # addSpace(buttonGroup,10)
  gbutton("Apply", cont = buttonGroup_scatmat, handler = function(...){
    print("apply from gui_scatmat")
    pause_scatmat(FALSE)
    update_tour_scatmat()
  })

  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_scatmat, handler = function(...) {
    pause_scatmat(TRUE)
    dispose(w)
  })

  vbox_scatmat[2:3, 4, anchor = c(0, 1)] <- buttonGroup_scatmat
# ============================ End of Gui_scatmat ==============

# =============================== Gui_stars====================
  update_tour_stars <- function(...) {
    tour <<- create_tour_stars(data,
      var_selected = svalue(Variables_stars),
      dim_selected = svalue(Dimensions_stars), 
      tour_type = svalue(TourType_stars),
      aps = svalue(sl_stars)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }
# ==================Controls==========================
vbox_stars <- glayout(cont = g4)

  # Variable selection column
  vbox_stars[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_stars[2, 1] <- Variables_stars <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)

  # Tour selection column
  vbox_stars[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox_stars[2, 3] <- TourType_stars <- gradio(tour_types)

  # dimension control
  vbox_stars[3, 1, anchor = c(-1, 0)] <- "Choose Dimension"
  dimensions <- c(2:length(data[num]))
  vbox_stars[4, 1, anchor = c(-1, 0)] <- Dimensions_stars <- gradio(dimensions)

  # speed and pause
  vbox_stars[3,3, anchor = c(-1, 0)] <- "Speed"
  vbox_stars[4,3, expand = TRUE] <- sl_stars <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox_stars[4, 4] <- chk_pause_stars <- gcheckbox("Pause", 
    handler = function(h, ...) pause_stars(svalue(h$obj)))

  # buttons control
  anim_id <- NULL
  pause_stars <- function(paused) {
    svalue(chk_pause_stars) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  buttonGroup_stars <- ggroup(horizontal = F, cont=vbox_stars)  
  
  # addSpace(buttonGroup,10)
  gbutton("Apply", cont = buttonGroup_stars, handler = function(...){
    print("apply from gui_stars")
    opar <- par(mfrow = c(1,1))
    pause_stars(FALSE)
    update_tour_stars()
  })

  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_stars, handler = function(...) {
    pause_stars(TRUE)
    dispose(w)
  })

  vbox_stars[2:3, 4, anchor = c(0, 1)] <- buttonGroup_stars
  

# =============================== Gui_stereo ===================
  update_tour_stereo <- function(...) {
    tour <<- create_tour_stereo(data,
      var_selected = svalue(Variables_stereo),
      tour_type = svalue(TourType_stereo),
      aps = svalue(sl_stereo)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }
# ==================Controls==========================
vbox_stereo <- glayout(cont = g6)
  # Variable selection column
  vbox_stereo[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_stereo[2, 1] <- Variables_stereo <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)

  # Tour selection column
  vbox_stereo[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox_stereo[2, 3] <- TourType_stereo <- gradio(tour_types)


  # speed and pause
  vbox_stereo[3,1, anchor = c(-1, 0)] <- "Speed"
  vbox_stereo[4,1, expand = TRUE] <- sl_stereo <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox_stereo[4, 3] <- chk_pause_stereo <- gcheckbox("Pause", 
    handler = function(h, ...) pause_stereo(svalue(h$obj)))

  # buttons control
  anim_id <- NULL
  pause_stereo <- function(paused) {
    svalue(chk_pause_stereo) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  buttonGroup_stereo <- ggroup(horizontal = F, cont=vbox_stereo)  
  
  # addSpace(buttonGroup,10)
  gbutton("Apply", cont = buttonGroup_stereo, handler = function(...) {
    print("apply from gui_stereo")
    opar <- par(mfrow = c(1,1))
    pause_stereo(FALSE)
    update_tour_stereo()
  })

  
  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_stereo, handler = function(...) {
    pause_stereo(TRUE)
    dispose(w)
  })

  vbox_stereo[2:3, 4, anchor = c(0, -1)] <- buttonGroup_stereo
# ============================ End of Gui_stereo ===============

# =============================== Gui_pcp ======================
  update_tour_pcp <- function(...) {
    tour <<- create_tour_pcp(data,
      var_selected = svalue(Variables_pcp),
      dim_selected = svalue(Dimensions_pcp), 
      tour_type = svalue(TourType_pcp),
      aps = svalue(sl_pcp)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }
# ==================Controls==========================
vbox_pcp <- glayout(cont = g7)

  # Variable selection column
  vbox_pcp[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_pcp[2, 1] <- Variables_pcp <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)

  # Tour selection column
  vbox_pcp[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox_pcp[2, 3] <- TourType_pcp <- gradio(tour_types)

  # dimension control
  vbox_pcp[3, 1, anchor = c(-1, 0)] <- "Choose Dimension"
  dimensions <- c(2:length(data[num]))
  vbox_pcp[4, 1, anchor = c(-1, 0)] <- Dimensions_pcp <- gradio(dimensions)

  # speed and pause
  vbox_pcp[3,3, anchor = c(-1, 0)] <- "Speed"
  vbox_pcp[4,3, expand = TRUE] <- sl_pcp <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox_pcp[4, 4] <- chk_pause_pcp <- gcheckbox("Pause", 
    handler = function(h, ...) pause_pcp(svalue(h$obj)))

  # buttons control
  anim_id <- NULL
  pause_pcp <- function(paused) {
    svalue(chk_pause_pcp) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  buttonGroup_pcp <- ggroup(horizontal = F, cont=vbox_pcp)  
  
  # addSpace(buttonGroup,10)
  gbutton("Apply", cont = buttonGroup_pcp, handler = function(...){
    print("apply from gui_pcp")
    opar <- par(mfrow = c(1,1))
    pause_pcp(FALSE)
    update_tour_pcp()
  })
  
  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_pcp, handler = function(...) {
    pause_pcp(TRUE)
    dispose(w)
  })

  vbox_pcp[2:3, 4, anchor = c(0, 1)] <- buttonGroup_pcp
# ============================ End of Gui_pcp =========================================
# =============================== Gui_andrews ==========================================
  update_tour_andrews <- function(...) {
    tour <<- create_tour_andrews(data,
      var_selected = svalue(Variables_andrews),
      cat_selected = svalue(Class_andrews), 
      dim_selected = svalue(Dimensions_andrews), 
      tour_type = svalue(TourType_andrews),
      aps = svalue(sl_andrews)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }

# ==================Controls==========================
  vbox_andrews <- glayout(cont = g5)

  # Variable selection column
  vbox_andrews[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_andrews[2, 1] <- Variables_andrews <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)
  vbox_andrews[3, 1, anchor = c(-1, 0)] <- "Class Selection"
  vbox_andrews[4, 1, anchor = c(-1, 0)] <- Class_andrews <- gtable(names(data)[!num], 
    multiple = TRUE)

  # Tour selection column
  vbox_andrews[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox_andrews[2, 3] <- TourType_andrews <- gradio(tour_types)

  # dimension control
  vbox_andrews[3, 3, anchor = c(-1, 0)] <- "Choose Dimension"
  dimensions <- c(2:length(data[num]))
  vbox_andrews[4, 3, anchor = c(-1, 0)] <- Dimensions_andrews <- gradio(dimensions)

  # speed and pause
  vbox_andrews[5,1, anchor = c(-1, 0)] <- "Speed"
  vbox_andrews[6,1, expand = TRUE] <- sl_andrews <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox_andrews[6, 3] <- chk_pause_andrews<- gcheckbox("Pause", 
    handler = function(h, ...) pause_andrews(svalue(h$obj)))

  # buttons control
anim_id <- NULL
  pause_andrews <- function(paused) {
    svalue(chk_pause_andrews) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  buttonGroup_andrews <- ggroup(horizontal = F, cont=vbox_andrews)  
  
  # addSpace(buttonGroup,10)
  gbutton("Apply", cont = buttonGroup_andrews, handler = function(...) {
    print("apply from gui_andrews")
    opar <- par(mfrow = c(1,1))
    pause_andrews(FALSE)
    update_tour_andrews()
  })
  
  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_andrews, handler = function(...) {
    pause_andrews(TRUE)
    dispose(w)
  })

  vbox_andrews[5:6, 4, anchor = c(0, 1)] <- buttonGroup_andrews
# ============================ End of Gui_andrews ==========================================
# =============================== Gui_density ==========================================
  update_tour_density <- function(...) {
    tour <<- create_1d_tour(data,
      var_selected = svalue(Variables_density), 
      method_selected = svalue(MethodType),
      center_selected = svalue(CenterType),
      tour_type = svalue(TourType_density),
      aps = svalue(sl_density)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }

  # ==================Controls==========================
  vbox_density <- glayout(cont = g2)

  # Variable selection column
  vbox_density[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_density[2, 1] <- Variables_density <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)
  
  # Tour selection column
  vbox_density[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Local")
  vbox_density[2, 3] <- TourType_density <- gradio(tour_types)

  # speed and pause
  vbox_density[5,1, anchor = c(-1, 0)] <- "Speed"
  vbox_density[6,1, expand = TRUE] <- sl_density <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox_density[6, 3] <- chk_pause_density <- gcheckbox("Pause", 
    handler = function(h, ...) pause_density(svalue(h$obj)))

  # method control
  vbox_density[3, 1, anchor = c(-1, 0)] <- "Method Type"
  method_types <- c("density","hist","ash")
  vbox_density[4, 1, anchor = c(-1, 0)] <- MethodType <- gradio(method_types)
    
  # center control
  vbox_density[3,3, anchor=c(-1,0)] <- "Center or Not"
  center_types <- c("TRUE", "FALSE")
  vbox_density[4,3, anchor=c(-1,0)] <- CenterType <- gradio(center_types)

  # buttons control
  anim_id <- NULL
  pause_density <- function(paused) {
    svalue(chk_pause_density) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <<- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }

  buttonGroup_density <- ggroup(horizontal = F, cont=vbox_density
)  
  
  # addSpace(buttonGroup_density,10)
  gbutton("Apply", cont = buttonGroup_density, handler = function(...) {
    print("apply from gui_density")
    opar <- par(mfrow = c(1,1))
    pause_density(FALSE)
    update_tour_density()
  })
  
  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_density, handler = function(...) {
    pause_density(TRUE)
    dispose(w)
  })

  vbox_density[4:6, 4, anchor = c(0, 1)] <- buttonGroup_density
  

# ============================ End of Gui_density ==========================================
  
# =============================== Gui_xy ===============================================
  
  update_tour_xy <- function(...) {
    tour <<- create_tour_xy(data,
      var_selected = svalue(Variables_xy), 
      cat_selected = svalue(Class_xy), 
      axes_location = svalue(dl_xy),
      tour_type = svalue(TourType_xy),
      aps = svalue(sl_xy)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }
  
  draw_frame <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)  

    tour_step <- tour_anim$step2(svalue(sl_xy) / 33)
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


  vbox_xy <- glayout(cont = g1)

  # Variable selection column
  vbox_xy[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_xy[2, 1] <- Variables_xy <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)
  vbox_xy[3, 1, anchor = c(-1, 0)] <- "Class Selection"
  vbox_xy[4, 1, anchor = c(-1, 0)] <- Class_xy <- gtable(names(data)[!num], 
    multiple = TRUE)

  # Tour selection column
  vbox_xy[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox_xy[2, 3] <- TourType_xy <- gradio(tour_types)

  # speed and pause
  vbox_xy[5,1, anchor = c(-1, 0)] <- "Speed"
  vbox_xy[6,1, expand = TRUE] <- sl_xy <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox_xy[6, 3] <- chk_pause_xy <- gcheckbox("Pause", 
    handler = function(h, ...) pause_xy(svalue(h$obj)))

  # axes control
  vbox_xy[3,3, anchor=c(-1,0)] <- "Axes Locations"
  locations <- c("center", "bottomleft", "off")
  vbox_xy[4,3, anchor=c(-1,0)] <- dl_xy <- gradio(locations)

  # buttons control
  anim_id <- NULL
  pause_xy <- function(paused) {
    svalue(chk_pause_xy) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <<- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  
  buttonGroup_xy <- ggroup(horizontal = F, cont=vbox_xy)  
  
  # addSpace(buttonGroup_xy,10)
  gbutton("Apply", cont = buttonGroup_xy, handler = function(...) {
    print("apply from gui_xy")
    pause_xy(FALSE)
    opar <- par(mfrow = c(1,1))
    update_tour_xy()
#   par(opar)
  })
  
  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_xy, handler = function(...) {
    pause_xy(TRUE)
    dispose(w)
  })

  # addSpace(buttonGroup,10)
  gbutton("Help",cont=buttonGroup_xy, handler = function(...) {
gmessage("GUI_xy allows user to control a dynamic plot by using a checkbox, a ratiobox, a table, a slider and some bottons. And it could easily be extended. 
It's much more convenient for users to just click on this simple GUI instead of trying to figure out how to write the proper auguments for their desirable graphics.", 
title="gui_help",icon="info")
  })


  vbox_xy[4:6, 4, anchor = c(0, 1)] <- buttonGroup_xy
  
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
# =================================== End of Gui_xy ================================================
  
  pause_xy(FALSE)
  pause_density(FALSE)
  visible(w) <- TRUE
  invisible()
}



# ==================================create_tour_xy==================================================

create_tour_xy <- function(data, var_selected, cat_selected, axes_location, tour_type, aps) {
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
    "Guided(holes)" = guided_tour(holes), 
    "Guided(cm)" = guided_tour(cm), 
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected])),
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

# ==================================create_1d_tour============================================


create_1d_tour <- function(data, var_selected, method_selected, center_selected, tour_type, aps) {
  if (length(var_selected) < 2) {
    gmessage("Please select at least two variables", icon = "warning")
    return()
  }
   
  display <- display_dist(data, method = method_selected, center = center_selected) 

  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(), 
    "Little" = little_tour(), 
    "Guided(holes)" = guided_tour(holes), 
    "Guided(cm)" = guided_tour(cm), 
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

# ==================================create_tour_andrews==========================

create_tour_andrews <- function(data, var_selected, cat_selected, dim_selected, tour_type, aps) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }

  display <- display_andrews(data,tour_path=tour_type, col=col)


  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(as.numeric(dim_selected)), 
    "Little" = little_tour(as.numeric(dim_selected)), 
    "Guided(holes)" = guided_tour(holes,as.numeric(dim_selected)), 
    "Guided(cm)" = guided_tour(cm,as.numeric(dim_selected)), 
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected]),as.numeric(dim_selected)),
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
# ==================================create_tour_pcp================================
create_tour_pcp <- function(data, var_selected, dim_selected, tour_type, aps) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }


  display <- display_pcp(data,tour_path=tour_type)


  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(as.numeric(dim_selected)), 
    "Little" = little_tour(as.numeric(dim_selected)), 
    "Guided(holes)" = guided_tour(holes,as.numeric(dim_selected)), 
    "Guided(cm)" = guided_tour(cm,as.numeric(dim_selected)), 
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected]),as.numeric(dim_selected)),
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
# ==================================create_tour_stereo=================
create_tour_stereo <- function(data, var_selected, tour_type, aps) {
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
# ==================================create_tour_stars==================================
create_tour_stars <- function(data, var_selected, dim_selected, tour_type, aps) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }

  display <- display_stars(data,tour_path=tour_type)

  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(as.numeric(dim_selected)), 
    "Little" = little_tour(as.numeric(dim_selected)), 
    "Guided(holes)" = guided_tour(holes,as.numeric(dim_selected)), 
    "Guided(cm)" = guided_tour(cm,as.numeric(dim_selected)), 
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected]),as.numeric(dim_selected)),
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
# ==================================create_tour_scatmat==================================
create_tour_scatmat <- function(data, var_selected, projdim_selected, tour_type, aps) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }


  display <- display_scatmat(data,tour_path=tour_type)


  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(as.numeric(projdim_selected)), 
    "Little" = little_tour(as.numeric(projdim_selected)), 
    "Guided(holes)" = guided_tour(holes,as.numeric(projdim_selected)), 
    "Guided(cm)" = guided_tour(cm,as.numeric(projdim_selected)), 
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected]),as.numeric(projdim_selected)),
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
# ==================================create_tour_faces======================================
create_tour_faces <- function(data, var_selected, VarIndex, dim_selected, tour_type, aps) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }

  display <- display_faces(data, tour_path=tour_type)
    

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
# ========================================================================
gui_tour(flea)