# =============================== Gui_density ==========================================
interface_density = function(g2,data){

  # =============== Function: update_tour_density ===============
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
  # ----------------- End of update_tour_density -----------------


  # =============== Function: draw_frame_density ==================
  draw_frame_density <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)

    tour_step <- tour_anim$step2(svalue(sl_density) / 33)
    if (is.null(tour_step$proj)) return(FALSE)

    if (find_platform()$os == "win") {
      tour$display$render_frame()
    } else {
      tour$display$render_transition()
    }
    with(tour_step, tour$display$render_data(tour$data, proj, target))
    Sys.sleep(1/33)

    TRUE
  }
  # -------------------- End of draw_frame_density -----------------

  num <- sapply(data, is.numeric)

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
      anim_id <<- gIdleAdd(draw_frame_density)
    }
  }

  buttonGroup_density <- ggroup(horizontal = F, cont=vbox_density)

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

}
# --------------------------- End of Gui_density -----------------------------------

# ======================= Function: create_1d_tour ========================

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
# ------------------- End of create_1d_tour ----------------------------