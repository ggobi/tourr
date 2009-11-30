# =============================== Gui_andrews ===============================
interface_andrews = function(g5, data){


  # =============== Function: update_tour_andrews ==============
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
  # ----------------- End of update_tour_andrews -----------------
  
  
  # =============== Function: draw_frame_andrews ==================
  draw_frame_andrews <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)

    tour_step <- tour_anim$step2(svalue(sl_andrews) / 33)
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
  # -------------------- End of draw_frame_andrews -----------------

  num <- sapply(data, is.numeric)

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
      anim_id <<- gIdleAdd(draw_frame_andrews)
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

}
# ----------------------------- End of Gui_andrews ----------------------------------

# ====================== Function: create_tour_andrews  ==================

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

# -------------------- End of create_tour_andrews ------------------------