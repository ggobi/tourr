# =============================== Gui_stars==============================

interface_stars = function(g4,data){
  # =============== Function: update_tour_stars ==================
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
  # --------------------- End of update_tour_stars ----------------
  
  # ================= Function: draw_frame_stars ==================
  draw_frame_stars <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)

    tour_step <- tour_anim$step2(svalue(sl_stars) / 33)
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
  # -------------------- End of draw_frame_stars -----------------
  
  num <- sapply(data, is.numeric)
  # ================== Controls ==========================
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
      anim_id <<- gIdleAdd(draw_frame_stars)
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
  
}
# ------------------------- End of Gui_stars ----------------------------

# ========================== Function: create_tour_stars ====================
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
# ---------------------------- End of create_tour_stars ----------------------