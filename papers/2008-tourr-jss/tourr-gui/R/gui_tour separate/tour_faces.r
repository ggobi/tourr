
# =============================== Gui_faces ====================================
interface_faces = function(g3, data){

  # =============== Function: update_tour_faces ==================
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
  # --------------------- End of update_tour_xy ------------------
  
  # ================= Function: draw_frame =======================
  draw_frame_faces <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)

    tour_step <- tour_anim$step2(svalue(sl_faces) / 33)
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
  # ---------------------- End of draw_frame ---------------------
  
  num <- sapply(data, is.numeric)
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
        anim_id <<- gIdleAdd(draw_frame_faces)
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
}
# -------------------------- End of Gui_faces ----------------------------------
  

# ================= Function: create_tour_faces ===================
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
# --------------------- End of create_tour_faces ----------------------------