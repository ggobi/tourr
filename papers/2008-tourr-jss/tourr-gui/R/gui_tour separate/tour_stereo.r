# =============================== Gui_stereo ================================
interface_stereo = function(g6, data){

  # ================= Function: update_tour_stereo ==================
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
  # -------------------- End of update_tour_stereo -----------------
  
  # ================= Function: draw_frame_stereo ==================
  draw_frame_stereo <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)

    tour_step <- tour_anim$step2(svalue(sl_stereo) / 33)
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
  # -------------------- End of draw_frame_stereo -----------------

  num <- sapply(data, is.numeric)
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
      anim_id <<- gIdleAdd(draw_frame_stereo)
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
}
# ----------------------------- End of Gui_stereo ------------------------------

# ============================= create_tour_stereo ============================
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

# ----------------------------- End of create_tour_stereo ---------------------