#' Di Function
#'
#' @keywords internal
#' @examples
#' #t1 <- save_history(flea[, 1:6], nbases = 3)
#' #t1 <- save_history(flea[, 1:6], nbases = 100, interpolate=TRUE)
#' #t1 <- save_history(flea[, c(1,2,4,5)], nbases = 2)
#' #t1 <- save_history(flea[, c(1,2,4)], nbases = 2)
#' #t1 <- save_history(flea[, c(1,2,4)], nbases = 500)
#' #observe_2dframes(interpolate(t1))
observe_2dframes <- function(basis_set, ...) {
  if (!require("rggobi")) {
    stop("rggobi required for this visualisation")
  }
  if (is.null(basis_set)) return(NA)

  # Draw all target bases
  if (is.list(basis_set)) {
    current = basis_set[[1]]
    if (ncol(basis_set[[1]]) != 2) {
      cat("Basis set needs to be 2D \n")
      return(NA)
    }
    n <- length(basis_set)
#    bdim <<- nrow(basis_set[[1]])
    bases <<- rbind(rep(0, nrow(basis_set[[1]])), basis_set[[1]][, 1],
         basis_set[[1]][, 2], basis_set[[1]][, 1] + basis_set[[1]][, 2])
    colnames(bases) <- paste("V", 1:nrow(basis_set[[1]]), sep="")
    bases <<- rbind(bases, basis_set[, , 1][, 1],
         basis_set[, , 1][, 2], basis_set[, , 1][, 1] + basis_set[, , 1][, 2])
    bases_edges <<- matrix(c(1, 2, 1, 3, 2, 4, 4, 3, 1, 5, 1, 6, 5, 7, 7, 6),
      ncol=2, byrow=TRUE)
    for (i in 2:n) {
      bases <<- rbind(bases, basis_set[[i]][, 1], basis_set[[i]][, 2],
        basis_set[[i]][,1]+basis_set [[i]][,2])
      bases_edges <<- rbind(bases_edges, c(1,(i-2)*3+1+7), c(1,(i-2)*3+2+7),
        c((i-2)*3+1+7, (i-2)*3+3+7), c((i-2)*3+3+7, (i-2)*3+2+7))
    }
    bases_edges <<- rbind(bases_edges, bases_edges)
  } else {
    current = basis_set[, , 1]
    if (ncol(basis_set[, , 1]) != 2) {
      cat("Basis set needs to be 2D \n")
      return(NA)
    }
    n <- dim(basis_set)[3]
#    bdim <<- nrow(basis_set[, , 1])
    # This first one is used for the interpolation frame
    bases <<- rbind(rep(0, nrow(basis_set[, , 1])), basis_set[, , 1][, 1],
      basis_set[, , 1][, 2], basis_set[, , 1][, 1] + basis_set[, , 1][, 2])
    colnames(bases) <<- paste("V", 1:nrow(basis_set[, , 1]), sep="")
    bases <<- rbind(bases, basis_set[, , 1][, 1],
      basis_set[, , 1][, 2], basis_set[, , 1][, 1] + basis_set[, , 1][, 2])
    bases_edges <<- matrix(c(1, 2, 1, 3, 2, 4, 4, 3, 1, 5, 1, 6, 5, 7, 7, 6),
      ncol=2, byrow=TRUE)
    for (i in 2:n) {
      bases <<- rbind(bases, basis_set[, , i][, 1], basis_set[, , i][, 2],
        basis_set[, , i][, 1]+basis_set [, , i][, 2])
      bases_edges <<- rbind(bases_edges, c(1,(i-2)*3+1+7), c(1,(i-2)*3+2+7),
        c((i-2)*3+1+7, (i-2)*3+3+7), c((i-2)*3+3+7, (i-2)*3+2+7))
    }
    bases_edges <<- rbind(bases_edges, bases_edges)
    cat(n, nrow(bases), nrow(bases_edges), "\n")
    cat(bases_edges[nrow(bases_edges), 1], bases_edges[nrow(bases_edges), 1], "\n")
  }

#  sph <- gen_sphere(1000, bdim)
#  cat(nrow(bases), ncol(bases), ncol(sph), "\n")
#  bases <- rbind(bases, sph)
  rownames(bases) <- as.character(1:nrow(bases))
  rownames(bases) <- rownames(bases)
  bases_edges <- matrix(as.character(bases_edges),ncol=2)
  cat(rownames(bases)[1], rownames(bases)[2], "\n")
  gd <- ggobi(bases)
  d <- displays(gd)[[1]]
  pmode(d) <- "2D Tour"
  g <- gd[1]
  gcolor <- c(rep(9, 4), rep(2, nrow(bases) - 4))
  glyph_color(g) <- gcolor
  edges(g) <- bases_edges
  cat("To watch the frame interpolation, pause the tour \n")
  cat("at a good view of the frames, and turn on edges \n")

  firstime <<- TRUE
  # Then draw the interpolation frame at each step
  step <- function(step, proj, geodesic) {
    if (firstime)
      ans <<- readline("Press y when ready to start.")
    if (ans != "y") {
      Sys.sleep(2)
      ans <<- "n"
    }
    firstime <<- FALSE
    for (j in 1:nrow(proj)) {
      g[2, j] <<- proj[j, 1] # relies on having ggobi open
      g[3, j] <<- proj[j, 2]
      g[4, j] <<- proj[j, 1] + proj[j, 2]
    }
  }
  target <- function(target) {
    tans <<- readline("Press y to start next interpolation.")
    if (tans != "y") {
      Sys.sleep(2)
      tans <<- "n"
    }
  }

  bases_stuff <- list(bases=bases, bedges=bases_edges)
  return(bases_stuff)

  planned_tour(current, basis_set, step_fun = step, target_fun = target,
    total_steps = Inf, ...)

}


#' Di Function
#'
#' @keywords internal
#' @examples
#' #t1 <- save_history(flea[, c(1,2,4)], nbases = 3, d = 1)
#' #t1 <- save_history(flea[, 1:6], nbases = 3, d = 1)
#' #observe_vectors_move(t1)
#' #observe_vectors_move(interpolate(t1))
observe_vectors_move <- function(basis_set, index_f = NULL, ...) {
  if (!require("rggobi")) {
    stop("rggobi required for this visualisation")
  }

  # Collect the vectors into one matrix
  if (is.list(basis_set)) {
    current = matrix(basis_set[[1]])
    n <- length(basis_set)
    pdim <- ncol(basis_set[[1]])
    vec <<- rbind(rep(0, length(basis_set[[1]])), basis_set[[1]],
      basis_set[[1]], -basis_set[[1]])
    vec_edges <<- rbind(c(1, 2), c(1, 3), c(1, 4))
    colnames(vec) <- paste("V", 1:length(basis_set[[1]]), sep="")
    bdim <<- length(basis_set[[1]])
    for (i in 2:n) {
      vec <<- rbind(vec, basis_set[[i]], -basis_set[[i]])
      vec_edges <<- rbind(vec_edges, c(1, i + 3), c(1, 2*i + 3))
    }
    # add one more to try to compensate for error
    vec_edges <<- rbind(vec_edges, c(1, n+1))
    x <<- attr(basis_set, "data")
  } else {
    current = matrix(basis_set[, , 1])
    n <- dim(basis_set)[3]
    pdim <- ncol(basis_set[[1]])
    vec <<- rbind(rep(0, length(basis_set[[1]])), basis_set[, , 1],
      basis_set[, , 1])
    vec_edges <<- rbind(c(1, 2), c(1, 3))
    colnames(vec) <<- paste("V", 1:length(basis_set[, , 1]), sep="")
    bdim <<- length(basis_set[, , 1])
    for (i in 2:n) {
      vec <<- rbind(vec, basis_set[, , i])
      vec_edges <<- rbind(vec_edges, c(1, i + 1))
    }
    # add one more to try to compensate for error
    vec_edges <<- rbind(vec_edges, c(1, n+1))
    x <<- attr(basis_set, "data")
  }
  cat(nrow(vec_edges), nrow(vec), "\n")

  # Make the background sphere
  sph <- gen_sphere(1000, bdim)
  vec <- rbind(vec, sph)
  if (!is.null(index_f)) {
    index_val <- NULL
    for (i in 1:1000)
      index_val <- c(index_val,index_f(x%*%as.matrix(sph[i,])))
    index_val_range <<- range(index_val)
    index_val <- (index_val-index_val_range[1])/diff(index_val_range)
  }
  lims <- 1
  if (!is.null(index_f)) {
    vec <- rbind(vec, sph+sph*index_val)
    vec <- rbind(vec, x/3)
    lims <- 1
  }
  vec <- rbind(vec, rep(-lims, ncol(x)), rep(lims, ncol(x)))

  # Load structures into ggobi, color
  rownames(vec) <- as.character(1:nrow(vec))
  rownames(vec) <- rownames(vec)
  gd <- ggobi(vec)
  d <- displays(gd)[[1]]
  pmode(d) <- "2D Tour"
  g <- gd[1]
  gcolor <- c(2, 2, rep(2, n), rep(8, 1000), rep(8, 1000), rep(1, nrow(x)), 1, 1)
  glyph_color(g) <- gcolor
  gsize <- c(5, 5, rep(5, n), rep(1, 2000), rep(3, nrow(x)), 1, 1)
  glyph_size(g) <- gsize
  gtype <- c(rep(6, 2+n+2000+nrow(x)), 1, 1)
  glyph_type(g) <- gtype
  edges(g) <- vec_edges
  cat("To watch the frame interpolation, turn on edges and pause the tour. \n")
  cat("Set the limits of all variables to be -2, 2, to maintain the sphere. \n")

  firstime <<- TRUE
  # Then draw the interpolation frame at each step
  step <- function(step, proj, geodesic) {
    if (firstime)
      ans <<- readline("Press y when ready to start.")
    if (ans != "y") {
      Sys.sleep(2)
      ans <<- "n"
    }
    firstime <<- FALSE
    for (j in 1:nrow(proj))
      g[2,j] <<- proj[j]
  }
  target <- function(target, geodesic) {
    tans <<- readline("Press y to start next interpolation.")
    if (tans != "y") {
      Sys.sleep(2)
      tans <<- "n"
    }
  }

  planned_tour(current, basis_set, step_fun = step, target_fun = target,
    total_steps = Inf, ...)
}


#' Di Function
#'
#' @keywords internal
#' @examples
#' #t1 <- save_history(flea[, 1:3], nbases = 50, d = 1)
#' #t1 <- save_history(flea[, 1:6], nbases = 3, d = 1, interpolate = TRUE)
#' #observe_vectors(interpolate(t1), nbases = 3)
#' #t2 <- save_history(flea[,c(1,2,4)], tour_f = guided_tour, index_f = holes, d=1, sphere=TRUE, basis_f = basis_geodesic_search)
#' #observe_vectors(interpolate(t2), holes)
#' #t3 <- save_history(flea[,1:6], tour_f = guided_tour, index_f = holes, d=1, sphere=TRUE, basis_f = basis_geodesic_search)
#' #observe_vectors(interpolate(t3), holes)
observe_vectors <- function(basis_set, index_f = NULL, nbases = 2, ...) {
  if (!require("rggobi")) {
    stop("rggobi required for this visualisation")
  }

  # Collect the vectors into one matrix
  current = matrix(basis_set[, , 1])
  n <- dim(basis_set)[3]
  pdim <- ncol(basis_set[[1]])
  x <<- attr(basis_set, "data")
  if (!is.null(index_f)) {
    new_index_val <- (index_f(x%*%basis_set[, , 1]) - index_val_range[1])/
      diff(index_val_range)
    vec <<- t(as.matrix(basis_set[, , 1]+basis_set[, , 1]*new_index_val))
  }
  else
    vec <<- t(as.matrix(basis_set[, , 1]))
  cat(nrow(vec), ncol(vec), n, "\n")
  bdim <<- length(basis_set[, , 1])
  # Make the background sphere
  sph <- gen_sphere(1000, bdim)
  if (!is.null(index_f)) {
    index_val <- NULL
    for (i in 1:1000)
      index_val <- c(index_val,index_f(x%*%as.matrix(sph[i,])))
    index_val_range <<- range(index_val)
    index_val <- (index_val-index_val_range[1])/diff(index_val_range)
  }
  for (i in 2:n) {
    if (!is.null(index_f)) {
      new_index_val <- (index_f(x%*%basis_set[, , i]) - index_val_range[1])/
        diff(index_val_range)
      vec <<- rbind(vec, basis_set[, , i] + basis_set[, , i]*new_index_val)
    }
    else
      vec <<- rbind(vec, basis_set[, , i])
  }
  colnames(vec) <<- paste("V", 1:length(basis_set[, , 1]), sep="")

  # Put pieces together
  vec <- rbind(vec, sph)
  lims <- 1
  if (!is.null(index_f)) {
    vec <- rbind(vec, sph+sph*index_val)
#    vec <- rbind(vec, x/3)
    vec <- rbind(rep(0, ncol(vec)), vec)
    vec_edges <- c(1,2)
    for (i in 2:n)
      vec_edges <- rbind(vec_edges, c(1,i+1))
    lims <- 2
  }
  else {
    vec <- rbind(rep(0, ncol(vec)), vec)
    vec_edges <- c(1,2)
    for (i in 2:n)
      vec_edges <- rbind(vec_edges, c(1,i+1))
  }
  vec <- rbind(vec, rep(-lims, ncol(x)), rep(lims, ncol(x)))
  cat(dim(vec_edges),"\n")

  rownames(vec) <- as.character(1:nrow(vec))
  rownames(vec) <- rownames(vec)
  vec_edges <- matrix(as.character(vec_edges),ncol=2)

  # Load structures into ggobi, color
  gd <- ggobi(vec)
  d <- displays(gd)[[1]]
  pmode(d) <- "2D Tour"
  g <- gd[1]
  gcolor <- c(rep(2, n+1), rep(8, 1000), rep(8, 1000), 8, 8)
  glyph_color(g) <- gcolor
  gsize <- c(rep(5, n+1), rep(2, 2000), 1, 1)
  glyph_size(g) <- gsize
  gtype <- c(rep(6, n+1+2000+nrow(x)), 1, 1)
  glyph_type(g) <- gtype
  edges(g) <- vec_edges

  NA
}

#' Di Function
#'
#' @keywords internal
gen_sphere <- function(n = 100, p = 5) {
  t(normalise(matrix(stats::rnorm(n * p), ncol = n)))
}


#' Di Function
#'
#' @keywords internal
#' @examples
#' #testdata <- matrix(rnorm(100*2), ncol=2)
#' #testdata[1:50,1] <- testdata[1:50,1] + 10
#' #testdata <- sphere(testdata)
#' #t1 <- save_history(testdata, nbases=3, d=1, rescale=FALSE, sphere=FALSE)
#' #observe_vectors_r(t1)
#' #observe_vectors_r(t1, plt_proj=TRUE)
#' #t1 <- save_history(testdata, tour_f = guided_tour, index_f = holes, nbases=5, d=1, rescale=FALSE, sphere=FALSE, max.tries = 100, cooling = 0.95)
#' #observe_vectors_r(t1, holes)
observe_vectors_r <- function(basis_set, index_f = NULL, plt_data = TRUE, plt_proj = FALSE, ...) {

  # Collect the vectors into one matrix
  if (is.list(basis_set)) {
    current = matrix(basis_set[[1]])
    n <- length(basis_set)
    pdim <- ncol(basis_set[[1]])
    vec <<- rbind(rep(0, length(basis_set[[1]])),
      basis_set[[1]])
    colnames(vec) <- paste("V", 1:length(basis_set[[1]]), sep="")
    bdim <<- length(basis_set[[1]])
    for (i in 2:n)
      vec <<- rbind(vec, basis_set[[i]])
  } else {
    current = matrix(basis_set[, , 1])
    n <- dim(basis_set)[3]
    pdim <- ncol(basis_set[[1]])
    vec <<- rbind(rep(0, length(basis_set[[1]])),
      basis_set[, , 1])
    colnames(vec) <<- paste("V", 1:length(basis_set[, , 1]), sep="")
    bdim <<- length(basis_set[, , 1])
    for (i in 2:n)
      vec <<- rbind(vec, basis_set[, , i])
    x <<- attr(basis_set, "data")
  }

  # Make the background sphere
  sph <- gen_sphere(1000, bdim)
  if (!is.null(index_f)) {
    index_val <- NULL
    for (i in 1:1000)
      index_val <- c(index_val,index_f(x%*%as.matrix(sph[i,])))
    index_val_range <<- range(index_val)
    index_val <- (index_val-index_val_range[1])/diff(index_val_range)
  }

  # Plot sphere, index values, and lines
  par(pty="s", mar=c(1,1,1,1))
  lims <<- c(-1.1,1.1)
  if (plt_proj) lims <<- c(-2,2)
  if (!is.null(index_f)) lims <<- c(-2,2)
  plot(sph, xlab=expression(x[1]), ylab=expression(x[2]),
    xlim=lims, ylim=lims, axes=FALSE, frame=TRUE)
  rect(-10, -10, 10, 10, col="grey90")
  abline(h=seq(-2,2,0.5),col="white")
  abline(v=seq(-2,2,0.5),col="white")
  points(sph, pch=16,col="white")
  if (!is.null(index_f)) {
    points(sph+sph*index_val, pch=16, col="grey70")
  }
  if (plt_proj) {
    for (i in 2:nrow(vec))
      plot_hist_on_proj(x, vec[i,])
  }
  for (i in 2:nrow(vec)) {
    lines(vec[c(1,i),1], vec[c(1, i), 2], col="black", lwd=2)
    lab = paste("F",i-2,sep="")
    pos <- 1 # Below
    if ((abs(vec[i, 1]) > abs(vec[i, 2]))) {
      if (vec[i,1] < 0) pos <- 2 # Left
      else pos <- 4 # Right
    }
    else {
      if (vec[i, 2] > 0) pos <- 3 # Above
    }
    if (!plt_proj) text(vec[i, 1], vec[i, 2], labels = lab, pos=pos, cex=2)
    lines(-vec[c(1,i),1], -vec[c(1, i), 2], col="black", lty=2, lwd=2)
  }
  if (plt_data) {
    points(x[,1]/3,x[,2]/3, pch=16, cex=2, col="grey70")
  }

  firstime <<- TRUE
  # Then draw the interpolation frame at each step
  step <- function(step, proj, geodesic) {
    if (firstime)
      ans <<- readline("Press y when ready to start.")
    if (ans != "y") {
      Sys.sleep(2)
      ans <<- "n"
    }
    firstime <<- FALSE
    lines(c(0,proj[1]), c(0,proj[2]), col="hotpink")
    if (!is.null(index_f)) {
      new_index_val <<- (index_f(x%*%as.matrix(proj))-index_val_range[1])/
        diff(index_val_range)
      points(proj[1]+proj[1]*new_index_val, proj[2]+proj[2]*new_index_val,
        pch=16, col="hotpink", cex=1.5)
    }
    else
      points(proj[1], proj[2], col="hotpink", pch=16)
  }
  target <- function(target) {
    tans <<- readline("Press y to start next interpolation.")
    if (tans != "y") {
      Sys.sleep(2)
      tans <<- "n"
    }
  }

  planned_tour(current, basis_set, step_fun = step, target_fun = target,
    total_steps = Inf, ...)
}


#' Di Function
#'
#' @keywords internal
plot_hist_on_proj <-function(data, proj) {
  proj_data = hist(data%*%as.matrix(proj), breaks=seq(-3,3,0.5), plot=FALSE)
  reflect <- FALSE
  if (proj[1] > 0 & proj[2] > 0) {
    ang <- -acos(proj[1]) # anti-clockwise
  }
  else if (proj[1] < 0 & proj[2] > 0) {
    ang <- pi-acos(proj[1]) # clockwise
    reflect <- TRUE
  }
  else if (proj[1] < 0 & proj[2] < 0) {
    ang <- -(pi-acos(proj[1]))# anti-clockwise
    reflect <- TRUE
  }
  else if (proj[1] > 0 & proj[2] < 0)
    ang <- acos(proj[1]) # clockwise
  proj_data$breaks <- (proj_data$breaks+3.0)/6
  if (reflect) proj_data$breaks <- -proj_data$breaks
  cen <- proj
  for (j in 1:length(proj_data$density)) {
    vertices <- c(proj_data$breaks[j]*cos(ang) + cen[1],
      proj_data$breaks[j+1]*cos(ang) + cen[1],
      proj_data$breaks[j+1]*cos(ang) + proj_data$density[j]*sin(ang) + cen[1],
      proj_data$breaks[j]*cos(ang) + proj_data$density[j]*sin(ang) + cen[1],
      proj_data$breaks[j]*cos(ang) + cen[1])
    vertices <- cbind(vertices, c(-proj_data$breaks[j]*sin(ang) + cen[2],
      -proj_data$breaks[j+1]*sin(ang) + cen[2],
      -proj_data$breaks[j+1]*sin(ang) + proj_data$density[j]*cos(ang) + cen[2],
      -proj_data$breaks[j]*sin(ang) + proj_data$density[j]*cos(ang) + cen[2],
      -proj_data$breaks[j]*sin(ang) + cen[2]))
    polygon(vertices[,1], vertices[,2], col="grey70", border="white")
  }
}


#' Di Function
#'
#' @keywords internal
#' @examples
#' #t2 <- save_history(testdata, tour_f = guided_tour, index_f = holes, nbases=5, d=1, rescale=FALSE, sphere=FALSE, max.tries = 100, cooling = 0.95)
#' #library(rggobi)
#' #gd<-observe_pp_trace(t2, nbases=10000)
#' #gd<-observe_pp_trace(interpolate(t2), nbases=10000)
#' #d<-displays(gd)[[1]]
#' #pmode(d) <- "1D Tour"
#' #ggsgnl <- gSignalConnect(gd, "identify-point", linker)
#' #Open a scatterplot display of the trace
#' #gSignalHandlerDisconnect(gd, ggsgnl)
#' #flea_s <- sphere(flea[,c(1,2,4)])
#' #t2 <- save_history(flea_s, tour_f = guided_tour, index_f = holes, nbases=10, d=1, rescale=FALSE, sphere=FALSE, max.tries = 100, cooling = 0.99)
#' #gd<-observe_pp_trace(interpolate(t2), nbases=10000)
#' #d<-displays(gd)[[1]]
#' #pmode(d) <- "1D Tour"
#' #t3 <- interpolate(t2)
#' #ggsgnl <- gSignalConnect(gd, "identify-point", linker2)
#' #gSignalHandlerDisconnect(gd, ggsgnl)
observe_pp_trace <- function(basis_set, index_f = holes, nbases = 2, ...) {
  if (!require("rggobi")) {
    stop("rggobi required for this visualisation")
  }

  # Collect the vectors into one matrix
  current = matrix(basis_set[, , 1])
  n <- min(dim(basis_set)[3], nbases)
  pdim <- ncol(basis_set[[1]])
  x <<- attr(basis_set, "data")
  cat(nrow(x), ncol(x), "\n")
  if (!is.matrix(basis_set[, , 1])) {
    vec <<- t(as.matrix(basis_set[, , 1]))
    index_val <<- index_f(x%*%as.matrix(basis_set[, , 1]))
    bdim <<- length(basis_set[, , 1])
    for (i in 2:n) {
      vec <<- rbind(vec, basis_set[, , i])
      index_val <<- c(index_val, index_f(x%*%as.matrix(basis_set[, , i])))
    }
    colnames(vec) <<- paste("V", 1:length(basis_set[, , 1]), sep="")
  }
  else {
    vec <<- t(basis_set[, , 1])
    index_val <<- index_f(x%*%basis_set[, , 1])
    bdim <<- nrow(basis_set[, , 1])
    for (i in 2:n) {
      vec <<- rbind(vec, t(basis_set[, , i]))
      index_val <<- c(index_val, index_f(x%*%basis_set[, , i]))
    }
    colnames(vec) <<- paste("V", 1:nrow(basis_set[, , 1]), sep="")
  }

  # Load structures into ggobi, color
  gd <- ggobi(x)
#  d <- displays(gd)[[1]]
#  pmode(d) <- "1D Tour"
  pptrace <- cbind(1:length(index_val), index_val)
  cat(nrow(pptrace), ncol(pptrace), "\n")
  colnames(pptrace) <- c("Time","Index")
  gd$trace <- pptrace

  gd
}

#' Di Function
#'
#' @keywords internal
linker <- function(gg, gplot, idx, data) {
  if (idx != -1) {
    idx <- idx + 1
    ggobi_display_set_tour_projection(d, t2[, , idx])
  }
}

#' Di Function
#'
#' @keywords internal
linker2 <- function(gg, gplot, idx, data) {
  if (idx != -1) {
    idx <- idx + 1
    ggobi_display_set_tour_projection(d, t3[, , idx])
  }
}

#' Di Function
#' This one will show the pp indices in R, and plot the vectors in ggobi
#'
#' @keywords internal
observe_pp_trace_r<- function(basis_set, index_f = holes, nbases = 2, ...) {

  # Collect the vectors into one matrix
  current = matrix(basis_set[, , 1])
  n <- min(dim(basis_set)[3], nbases)
  pdim <- ncol(basis_set[[1]])
  x <<- attr(basis_set, "data")
  vec <<- t(as.matrix(basis_set[, , 1]))
  index_val <<- index_f(x%*%as.matrix(basis_set[, , 1]))
  cat(nrow(vec), ncol(vec), "\n")
  bdim <<- length(basis_set[, , 1])
  for (i in 2:n) {
    vec <<- rbind(vec, basis_set[, , i])
    index_val <<- c(index_val, index_f(x%*%as.matrix(basis_set[, , i])))
  }
  colnames(vec) <<- paste("V", 1:length(basis_set[, , 1]), sep="")

  # Plot sphere, index values, and lines
  par(pty="s", mar=c(1,1,1,1))
  lims <<- c(-1,1)
  if (!is.null(index_f)) lims <<- c(-2,2)
  plot(sph, xlab=expression(x[1]), ylab=expression(x[2]),
    xlim=lims, ylim=lims, axes=FALSE, frame=TRUE)
  rect(-10, -10, 10, 10, col="grey90")
  abline(h=seq(-2,2,0.5),col="white")
  abline(v=seq(-2,2,0.5),col="white")
  points(sph, pch=16,col="white")
  if (!is.null(index_f)) {
    points(sph+sph*index_val, pch=16, col="grey70")
    points(x[,1]/3,x[,2]/3, pch=16, col="grey70")
  }
  for (i in 2:nrow(vec)) {
    lines(vec[c(1,i),1], vec[c(1, i), 2], col="black", lwd=2)
    lab = paste("F",i-2,sep="")
    pos <- 1
    if (vec[i, 2] > 0) pos <- 3
    text(vec[i, 1], vec[i, 2], labels = lab, pos=pos)
    lines(-vec[c(1,i),1], -vec[c(1, i), 2], col="black", lty=2, lwd=2)
  }

  firstime <<- TRUE
  # Then draw the interpolation frame at each step
  step <- function(step, proj, geodesic) {
    if (firstime)
      ans <<- readline("Press y when ready to start.")
    if (ans != "y") {
      Sys.sleep(2)
      ans <<- "n"
    }
    firstime <<- FALSE
    lines(c(0,proj[1]), c(0,proj[2]), col="hotpink")
    if (!is.null(index_f)) {
      new_index_val <<- (index_f(x%*%as.matrix(proj))-index_val_range[1])/
        diff(index_val_range)
      points(proj[1]+proj[1]*new_index_val, proj[2]+proj[2]*new_index_val,
        pch=16, col="hotpink", cex=1.5)
    }
  }
  target <- function(target) {
    tans <<- readline("Press y to start next interpolation.")
    if (tans != "y") {
      Sys.sleep(2)
      tans <<- "n"
    }
  }

  planned_tour(current, basis_set, step_fun = step, target_fun = target,
    total_steps = Inf, ...)
}
