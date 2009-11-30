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

  w <- gwindow("2D Tour plot example", visible = T)
  mw = gnotebook(cont=w)  
  g8 = ggroup(cont = mw, horizontal = FALSE,label="gui_scatmat") 
  g7 = ggroup(cont = mw, horizontal = FALSE,label="gui_pcp")
  g6 = ggroup(cont = mw, horizontal = FALSE,label="gui_stereo")
  g5 = ggroup(cont = mw, horizontal = FALSE,label="gui_andrews")
  g4 = ggroup(cont = mw, horizontal = FALSE,label="gui_stars")
  g3 = ggroup(cont = mw, horizontal = FALSE,label="gui_faces")
  g2 = ggroup(cont = mw, horizontal = FALSE,label="gui_density")
  g1 = ggroup(cont = mw, horizontal = FALSE,label="gui_xy") 
    
  
  interface_xy(g1,data)
  interface_density(g2,data)
  interface_faces(g3,data)
  interface_stars(g4,data)
  interface_andrews(g5,data)
  interface_stereo(g6,data)
  interface_pcp(g7,data)
  interface_scatmat(g8,data)
  
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
  
  
  # pause_xy(FALSE)
  # pause_density(FALSE)
  visible(w) <- TRUE
  invisible()
}

gui_tour(flea)