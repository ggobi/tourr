#X t1 <- save_history(flea[, 1:6], nbases = 3)
#X t1 <- save_history(flea[, 1:6], nbases = 3, interpolate=T)
#X observe_2dframes(t1)

observe_2dframes <- function(basis_set, ...) {
#  index <- 0

  # Draw all target bases
  if (is.list(basis_set)) {
    current = basis_set[[1]]
    n <- length(basis_set)
    bases <<- rbind(rep(0, nrow(basis_set[[1]])), basis_set[[1]][, 1],
         basis_set[[1]][, 2], basis_set[[1]][, 1] + basis_set[[1]][, 2])
    colnames(bases) <- paste("V", 1:nrow(basis_set[[1]]), sep="")
    bases <<- rbind(bases, basis_set[, , 1][, 1],
         basis_set[, , 1][, 2], basis_set[, , 1][, 1] + basis_set[, , 1][, 2])
    bases_edges <<- matrix(c(1, 2, 1, 3, 2, 4, 4, 3, 1, 5, 1, 6, 5, 7, 7, 6),
      ncol=2, byrow=T)
    for (i in 1:n) {
      bases <<- rbind(bases, basis_set[[i]][, 1], basis_set[[i]][, 2],
        basis_set[[i]][,1]+basis_set [[i]][,2])
      bases_edges <<- rbind(bases_edges, c(1,(i-1)*3+1+7), c(1,(i-1)*3+2+7),
        c((i-1)*3+1+7, (i-1)*3+3+7), c((i-1)*3+3+7, (i-1)*3+2+7))
    }    
  } else {
    current = basis_set[, , 1]
    n <- dim(basis_set)[3]
    # This first one is used for the interpolation frame
    bases <<- rbind(rep(0, nrow(basis_set[, , 1])), basis_set[, , 1][, 1],
      basis_set[, , 1][, 2], basis_set[, , 1][, 1] + basis_set[, , 1][, 2])
    colnames(bases) <<- paste("V", 1:nrow(basis_set[, , 1]), sep="")
    bases <<- rbind(bases, basis_set[, , 1][, 1],
      basis_set[, , 1][, 2], basis_set[, , 1][, 1] + basis_set[, , 1][, 2])
    bases_edges <<- matrix(c(1, 2, 1, 3, 2, 4, 4, 3, 1, 5, 1, 6, 5, 7, 7, 6),
      ncol=2, byrow=T)
    for (i in 1:n) {
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
  gcolor <- c(rep(9,4),rep(1,nrow(bases)-4))
  glyph_color(g) <- gcolor
  edges(g) <- bases_edges
  cat("To watch the frame interpolation, pause the tour \n")
  cat("at a good view of the frames, and turn on edges \n")
  
  # Then draw the interpolation frame at each step
  step <- function(step, proj) {
    for (j in 1:nrow(proj)) {
      g[2,j] <<- proj[j,1] # relies on having ggobi open
      g[3,j] <<- proj[j,2]
      g[4,j] <<- proj[j,1]+proj[j,2]
    }
  }
  target <- function(target) {
    cat("At a target\n")
  }

  planned_tour(current, basis_set, ...)
}

