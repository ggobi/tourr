#X t1 <- save_history(flea[, 1:6], nbases = 3)
#X t1 <- save_history(flea[, 1:6], nbases = 100, interpolate=T)
#X t1 <- save_history(flea[, c(1,2,4)], nbases = 100, interpolate=T)
#X observe_2dframes(t1)

observe_2dframes <- function(basis_set, ...) {
  if (is.null(basis_set)) return(NA)
  
  # Draw all target bases
  if (is.list(basis_set)) {
    current = basis_set[[1]]
    if (ncol(basis_set[[1]]) != 2) {
      cat("Basis set needs to be 2D \n")
      return(NA)
    }
    n <- length(basis_set)
    bases <<- rbind(rep(0, nrow(basis_set[[1]])), basis_set[[1]][, 1],
         basis_set[[1]][, 2], basis_set[[1]][, 1] + basis_set[[1]][, 2])
    colnames(bases) <- paste("V", 1:nrow(basis_set[[1]]), sep="")
    bases <<- rbind(bases, basis_set[, , 1][, 1],
         basis_set[, , 1][, 2], basis_set[, , 1][, 1] + basis_set[, , 1][, 2])
    bases_edges <<- matrix(c(1, 2, 1, 3, 2, 4, 4, 3, 1, 5, 1, 6, 5, 7, 7, 6),
      ncol=2, byrow=T)
    for (i in 2:n) {
      bases <<- rbind(bases, basis_set[[i]][, 1], basis_set[[i]][, 2],
        basis_set[[i]][,1]+basis_set [[i]][,2])
      bases_edges <<- rbind(bases_edges, c(1,(i-1)*3+1+7), c(1,(i-1)*3+2+7),
        c((i-1)*3+1+7, (i-1)*3+3+7), c((i-1)*3+3+7, (i-1)*3+2+7))
    }    
  } else {
    current = basis_set[, , 1]
    if (ncol(basis_set[, , 1]) != 2) {
      cat("Basis set needs to be 2D \n")
      return(NA)
    }
    n <- dim(basis_set)[3]
    # This first one is used for the interpolation frame
    bases <<- rbind(rep(0, nrow(basis_set[, , 1])), basis_set[, , 1][, 1],
      basis_set[, , 1][, 2], basis_set[, , 1][, 1] + basis_set[, , 1][, 2])
    colnames(bases) <<- paste("V", 1:nrow(basis_set[, , 1]), sep="")
    bases <<- rbind(bases, basis_set[, , 1][, 1],
      basis_set[, , 1][, 2], basis_set[, , 1][, 1] + basis_set[, , 1][, 2])
    bases_edges <<- matrix(c(1, 2, 1, 3, 2, 4, 4, 3, 1, 5, 1, 6, 5, 7, 7, 6),
      ncol=2, byrow=T)
    for (i in 2:n) {
      bases <<- rbind(bases, basis_set[, , i][, 1], basis_set[, , i][, 2],
        basis_set[, , i][, 1]+basis_set [, , i][, 2])
      bases_edges <<- rbind(bases_edges, c(1,(i-1)*3+1+7), c(1,(i-1)*3+2+7),
        c((i-1)*3+1+7, (i-1)*3+3+7), c((i-1)*3+3+7, (i-1)*3+2+7))
    }    
  }

  gd <- ggobi(bases)
  d <- displays(gd)[[1]]
  pmode(d) <- "2D Tour"
  g <- gd[1]
  gcolor <- c(rep(9, 4), rep(1, nrow(bases) - 4))
  glyph_color(g) <- gcolor
  edges(g) <- bases_edges
  cat("To watch the frame interpolation, pause the tour \n")
  cat("at a good view of the frames, and turn on edges \n")
  
  firstime <<- TRUE
  # Then draw the interpolation frame at each step
  step <- function(step, proj) {
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

  planned_tour(current, basis_set, step_fun = step, target_fun = target,
    total_steps = Inf, ...)
}

#X t1 <- save_history(flea[, 1:3], nbases = 5, d = 1)
#X t1 <- save_history(flea[, 1:6], nbases = 3, d = 1)
#X observe_vectors_move(t1)
observe_vectors_move <- function(basis_set, index_f = NULL, ...) {
  
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
    x <<- attr(basis_set, "data")
  } else {
    current = matrix(basis_set[, , 1])
    n <- dim(basis_set)[3]
    pdim <- ncol(basis_set[[1]])
    vec <<- rbind(rep(0, length(basis_set[[1]])), basis_set[, , 1],
      basis_set[, , 1], -basis_set[, , 1])
    vec_edges <<- rbind(c(1, 2), c(1, 3), c(1, 4))
    colnames(vec) <<- paste("V", 1:length(basis_set[, , 1]), sep="")
    bdim <<- length(basis_set[, , 1])
    for (i in 2:n) {
      vec <<- rbind(vec, basis_set[, , i], -basis_set[, , i])
      vec_edges <<- rbind(vec_edges, c(1, 2*(i-1) + 3), c(1, 2*(i-1)+1 + 3))
      cat(2*(i-1) + 3, 2*(i-1)+1 + 3, "\n")
    }
    x <<- attr(basis_set, "data")
  }
  cat(nrow(vec_edges), nrow(vec), "\n")

  # Make the background sphere
  sph <- gen.sphere(300, bdim)
  vec <- rbind(vec, sph)
  if (!is.null(index_f)) {
    index_val <- NULL
    for (i in 1:300) 
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
  gd <- ggobi(vec)
  d <- displays(gd)[[1]]
  pmode(d) <- "2D Tour"
  g <- gd[1]
  gcolor <- c(2, 2, rep(9, 2*n), rep(8, 300), rep(8, 300), rep(1, nrow(x)), 1, 1)
  glyph_color(g) <- gcolor
  gsize <- c(5, 5, rep(2, 2*n), rep(1, 600), rep(3, nrow(x)), 1, 1)
  glyph_size(g) <- gsize
  gtype <- c(rep(6, 2+2*n+600+nrow(x)), 1, 1)
  glyph_type(g) <- gtype
  edges(g) <- vec_edges
  cat("To watch the frame interpolation, turn on edges and pause the tour. \n")
  cat("Set the limits of all variables to be -2, 2, to maintain the sphere. \n")
  
  firstime <<- TRUE
  # Then draw the interpolation frame at each step
  step <- function(step, proj) {
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

#X t1 <- save_history(flea[, 1:3], nbases = 1000, d = 1, interpolate = T)
#X t1 <- save_history(flea[, 1:6], nbases = 3, d = 1, interpolate = T)
#X observe_vectors(t1, nbases = 50)
observe_vectors <- function(basis_set, index_f = NULL, nbases = 2, ...) {
  
  # Make the background sphere
  sph <- gen_sphere(300, bdim)
  if (!is.null(index_f)) {
    index_val <- NULL
    for (i in 1:300) 
      index_val <- c(index_val,index_f(x%*%as.matrix(sph[i,])))
    index_val_range <<- range(index_val)
    index_val <- (index_val-index_val_range[1])/diff(index_val_range)
  }

  # Collect the vectors into one matrix
  if (is.list(basis_set)) {
    current = matrix(basis_set[[1]])
    n <- min(length(basis_set), nbases)
    pdim <- ncol(basis_set[[1]])
    x <<- attr(basis_set, "data")
    vec <<- t(as.matrix(basis_set[[1]]))
    bdim <<- length(basis_set[[1]])
    for (i in 2:n) {
      vec <<- rbind(vec, basis_set[[i]])
    }
    colnames(vec) <- paste("V", 1:length(basis_set[[1]]), sep="")
  } else {
    current = matrix(basis_set[, , 1])
    n <- min(dim(basis_set)[3], nbases)
    pdim <- ncol(basis_set[[1]])
    x <<- attr(basis_set, "data")
    new_index_val <- (index_f(x%*%basis_set[, , 1]) - index_val_range[1])/
        diff(index_val_range)
    vec <<- t(as.matrix(basis_set[, , 1]+basis_set[, , 1]*new_index_val))
    cat(nrow(vec), ncol(vec), "\n")
    bdim <<- length(basis_set[, , 1])
    for (i in 2:n) {
      new_index_val <- (index_f(x%*%basis_set[, , i]) - index_val_range[1])/
        diff(index_val_range)
      vec <<- rbind(vec, basis_set[, , i] + basis_set[, , 1]*new_index_val)
    }
    colnames(vec) <<- paste("V", 1:length(basis_set[, , 1]), sep="")
  }

  # Put pieces together
  vec <- rbind(vec, sph)
  lims <- 1
  if (!is.null(index_f)) {
    vec <- rbind(vec, sph+sph*index_val)
    vec <- rbind(vec, x/3)
    lims <- 2
  }
  vec <- rbind(vec, rep(-lims, ncol(x)), rep(lims, ncol(x)))
  
  # Load structures into ggobi, color
  gd <- ggobi(vec)
  d <- displays(gd)[[1]]
  pmode(d) <- "2D Tour"
  g <- gd[1]
  gcolor <- c(rep(2, n), rep(8, 300), rep(8, 300), rep(1, nrow(x)), 8, 8)
  glyph_color(g) <- gcolor
  gsize <- c(rep(5, n), rep(2, 600), rep(3, nrow(x)), 1, 1)
  glyph_size(g) <- gsize
  gtype <- c(rep(6, n+600+nrow(x)), 1, 1)
  glyph_type(g) <- gtype
  
  NA
}

gen_sphere <- function(n = 100, p = 5) {
  x <- matrix(rnorm(n * p), ncol=p)
  xnew <- t(apply(x, 1, f.norm.vec))
  xnew
}

#X testdata <- matrix(rnorm(100*2), ncol=2)
#X testdata[1:50,1] <- testdata[1:50,1] + 10
#X testdata <- sphere(testdata)
#X t1 <- save_history(testdata, nbases=3, d=1, rescale=F, sphere=F)
#X observe_vectors_r(t1)
#X observe_vectors_r(t1, holes)
observe_vectors_r <- function(basis_set, index_f = NULL, ...) {
  
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
  sph <- gen.sphere(1000, bdim)
  if (!is.null(index_f)) {
    index_val <- NULL
    for (i in 1:1000) 
      index_val <- c(index_val,index_f(x%*%as.matrix(sph[i,])))
    index_val_range <<- range(index_val)
    index_val <- (index_val-index_val_range[1])/diff(index_val_range)
  }

  # Plot sphere, index values, and lines
  par(pty="s", mar=c(1,1,1,1))
  lims <<- c(-1,1)
  if (!is.null(index_f)) lims <<- c(-2,2)
  plot(sph, xlab=expression(x[1]), ylab=expression(x[2]),
    xlim=lims, ylim=lims, axes=F, frame=T)
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
  step <- function(step, proj) {
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

#X library(rggobi)
#X gd<-observe_pp_trace(t2, nbases=1000)
#X d<-displays(gd)[[1]]
#X ggsgnl <- gSignalConnect(gd, "identify-point", linker)
#X gSignalHandlerDisconnect(gd, ggsgnl)
observe_pp_trace <- function(basis_set, g, index_f = holes, nbases = 2, ...) {
  
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

  # Load structures into ggobi, color
  gd <- ggobi(x)
#  d <- displays(gd)[[1]]
#  pmode(d) <- "1D Tour"
#  pptrace <- cbind(1:length(index_val), index_val)
#  colnames(pptrace) <- c("Time","Index")
#  gd$trace <- pptrace

  gd
}

linker <- function(gg, gplot, idx, data) {
  d <- displays(gg)[[1]] 
  if (idx != -1) {
    idx <- idx + 1
    ggobi_display_set_tour_projection(d, basis_set[, , idx])
  }
}

# This one will show the pp indices in R, and plot the vectors in ggobi
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
    xlim=lims, ylim=lims, axes=F, frame=T)
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
  step <- function(step, proj) {
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
