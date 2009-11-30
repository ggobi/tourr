# =============================== Gui_scatmat ================================
interface_scatmat = function(g8,data){

  # =============== Function: update_tour_scatmat ==================
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
  # --------------------- End of update_tour_scatmat ----------------
  
  # ================= Function: draw_frame_scatmat ==================
  draw_frame_scatmat <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)

    tour_step <- tour_anim$step2(svalue(sl_scatmat) / 33)
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
  # ---------------------- End of draw_frame_scatmat -----------------
  
  num <- sapply(data, is.numeric)
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
        anim_id <<- gIdleAdd(draw_frame_scatmat)
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

}
# ============================ End of Gui_scatmat ==============


# ============================ create_tour_scatmat =======================
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

# --------------------- End of create_tour_scatmat ----------------------------