setwd("/Users/dicook/papers/tour")
# Load workspace file
load(".RData")

library(rggobi)

# Look at frames
Fa<-matrix(c(1,0,0,0,0,1,0,0),ncol=2,byrow=F)
Fz<-matrix(c(0.707,0,0.707,0,0,0.707,0,0.707),ncol=2,byrow=F)
Fz<-basis_random(4,2)
x<-rbind(c(0,0,0,0),t(Fa),t(Fa[,1]+Fa[,2]),t(Fa),t(Fa[,1]+Fa[,2]),
         t(Fz),t(Fz[,1]+Fz[,2]))
colnames(x)<-c("V1","V2","V3","V4")
rownames(x)<-c(1:10)
x.edges<-matrix(c(1,2,1,3,2,4,4,3,1,5,1,6,5,7,7,6,1,8,1,9,8,10,10,9),ncol=2,byrow=T)

library(rggobi)
g<-ggobi(x)[1]
edges(g)<-x.edges
ptcol<-c(rep(1,4),rep(9,6))
glyph_color(g)<-ptcol

interp<-geodesic(Fa,Fz)
gen_interp(interp,0.01)

y<-f.gen.sphere(500,4)
colnames(y)<-c("V1","V2","V3","V4")
x<-rbind(t(Fa),t(Fa),t(Fz),y)
g<-ggobi(x)[1]
ptcol<-c(4,4,9,9,3,3,rep(8,500))
glyph_color(g)<-ptcol
ptgly<-c(6,6,6,6,6,6,rep(1,500))
glyph_type(g)<-ptgly

interp<-geodesic(Fa,Fz)
nsteps <- ceiling(sqrt(sum(interp$tau^2))/0.01)
Fti<-NULL
for (i in 1:nsteps) {
  Ft <- step_fraction(interp, i/nsteps)
  Fti<-cbind(Fti,Ft)
}
x<-rbind(t(Fa),t(Fz),t(Fti),y)
g<-ggobi(x)[1]
ptcol<-c(4,4,3,3,rep(9,nsteps*2),rep(8,500))
glyph_color(g)<-ptcol
ptgly<-c(6,6,6,6,rep(1,nsteps*2),rep(1,500))
glyph_type(g)<-ptgly
# Animate motion
for (i in 2:nsteps) {
  ptgly[c(2*i-3,2*i-2)+4]<-1
  ptgly[c(2*i-1,2*i)+4]<-6
  glyph_type(g)<-ptgly
  readline("Ready to continue?")
}

# Check little tour
ltF<-bases_little(5,2)
y<-f.gen.sphere(500,5)
x<-rbind(rep(0,nrow(ltF[[1]])),t(ltF[[1]]))
colnames(x)<-paste("V",1:nrow(ltF[[1]]),sep="")
for (i in 1:length(ltF))
  x<-rbind(x,t(ltF[[i]]),-t(ltF[[i]]))
x<-rbind(x,y)
x<-as.data.frame(x)
g<-ggobi(x)[1]
ptcol<-c(6,6,6,rep(1,length(ltF)*4),rep(8,nrow(y)))
glyph_color(g)<-ptcol
x.edges<-matrix(c(1,2,3,1),ncol=2,byrow=T)
edges(g)<-x.edges
ptgly<-c(rep(6,length(ltF)*4+3),rep(1,nrow(y)))
glyph_type(g)<-ptgly
ptsiz<-c(2,rep(2,2),rep(c(6,6,3,3),length(ltF)),rep(1,nrow(y)))
glyph_size(g)<-ptsiz
Fa<-ltF[[1]]
for (j in 2:length(ltF)) {
  Fz<-ltF[[j]]
  interp<-geodesic(Fa,Fz)
  nsteps <- ceiling(sqrt(sum(interp$tau^2))/0.01)
  for (i in 1:nsteps) {
    Ft <- step_fraction(interp, i/nsteps)
    for (k in 1:nrow(Ft)) {
      g[2,k]<-Ft[k,1]
      g[3,k]<-Ft[k,2]
    }
  }
  readline("Ready to continue?")
  Fa<-Ft
}

# Now the frames
x<-rbind(rep(0,nrow(ltF[[1]])),ltF[[1]][,1],ltF[[1]][,2],
         ltF[[1]][,1]+ltF[[1]][,2])
colnames(x)<-paste("V",1:nrow(ltF[[1]]),sep="")
x.edges<-matrix(c(1,2,1,3,2,4,4,3),ncol=2,byrow=T)
for (i in 1:length(ltF)) {
  x<-rbind(x,ltF[[i]][,1],ltF[[i]][,2],ltF[[i]][,1]+ltF[[i]][,2])
  x.edges<-rbind(x.edges,c(1,(i-1)*3+1+4),c(1,(i-1)*3+2+4),
                 c((i-1)*3+1+4,(i-1)*3+3+4),c((i-1)*3+3+4,(i-1)*3+2+4))
}
g<-ggobi(x)[1]
ptcol<-rep(1,nrow(x))
ptcol[1:4]<-6
glyph_color(g)<-ptcol
edges(g)<-x.edges
Fa<-ltF[[1]]
for (j in 2:length(ltF)) {
  Fz<-ltF[[j]]
  interp<-geodesic(Fa,Fz)
  nsteps <- ceiling(sqrt(sum(interp$tau^2))/0.01)
  for (i in 1:nsteps) {
    Ft <- step_fraction(interp, i/nsteps)
    for (k in 1:nrow(Ft)) {
      g[2,k]<-Ft[k,1]
      g[3,k]<-Ft[k,2]
      g[4,k]<-Ft[k,1]+Ft[k,2]
    }
  }
  readline("Ready to continue?")
  Fa<-Ft
}

# Test data
.GGobiCall <- getNamespace("rggobi")$.GGobiCall

ggobi_display_set_tour_projection <- function (gd, value) 
{
    normal <- all(colSums(value^2) - 1 < 0.001)
    orthog <- all(crossprod(value, value) - diag(ncol(value)) < 
        0.001)
    if (!normal) 
        stop("Matrix is not normal (colSums do not equal 1)")
    if (!orthog) 
        stop("Matrix is not orthogonal")
    invisible(.GGobiCall("setTourProjection", gd, pmode(gd), 
        value))
}

testdata<-matrix(nrow=500,ncol=5)
testdata[,1]<-rnorm(500)
testdata[,2]<-c(rnorm(400,-3),rnorm(100,3))
testdata[,3]<-c(rnorm(300,-5),rnorm(100),rnorm(100,5))
testdata[,4]<-c(rnorm(200,-6),rnorm(100,-2),rnorm(100,2),rnorm(100,6))
testdata[,5]<-c(rnorm(100,-10),rnorm(100,-5),rnorm(100),rnorm(100,5),rnorm(100,10))
pairs(testdata,pch=16)
f.std.data<-function(x) {x-mean(x)/sd(x)}
testdata<-apply(testdata,2,f.std.data)

g<-ggobi(testdata)
d <- displays(g)[[1]] 
glyph_color(g[1])<-9
Fa<-ltF[[1]]
for (j in 2:length(ltF)) {
  Fz<-ltF[[j]]
  interp<-geodesic(Fa,Fz)
  nsteps <- ceiling(sqrt(sum(interp$tau^2))/0.01)
  for (i in 1:nsteps) {
    Ft <- step_fraction(interp, i/nsteps)
    ggobi_display_set_tour_projection(d,Ft)
  }
  readline("Ready to continue?")
  Fa<-Ft
}

   # With Hadley's code --------
library(rggobi)
g<-ggobi(testdata)
d <- displays(g)[[1]]
pmode(d) <- "2D Tour"
glyph_color(g[1])<-9
    # grand tour
target <- c(NA.NA) #basis_random(5, 2)
start <- ggobi_display_get_tour_projection(d)
last_proj <- target
grand_tour(start, total_steps = Inf, velocity = 0.001,
  step_fun = function(step, proj) {
    ggobi_display_set_tour_projection(d,proj)
    last_proj <<- proj
  },
  target_fun = function(proj) {
    Sys.sleep(0.5)
    target <<- proj 
    start <- last_proj
  }        
)

    # little tour
ltF <- bases_little(5, 2)
target <- c(NA.NA) #basis_random(5, 2)
start <- ltF[[1]]
last_proj <- target
little_tour(ltF, start, total_steps = Inf, velocity = 0.01,
  step_fun = function(step, proj) {
#    ggobi_display_set_tour_projection(d,proj)
#    cat("step",step,"\n")
    last_proj <<- proj
  },
  target_fun = function(proj) {
    Sys.sleep(0.5)
    cat("at target","\n")
    target <<- proj 
    start <- last_proj
  }        
)


# Check sequence and steps
   # My code
par(pch="s")
plot(NA, NA,xlim=c(-1.2,1.2), ylim=c(-1.2,1.2), xlab="", ylab="")
Fa <- matrix(c(0, -1), ncol = 1)
Fz <- basis_random(2,1)
interp<-geodesic(Fa,Fz)
points(Fa[1],Fa[2],pch=1,col="green")
points(Fz[1],Fz[2],pch=2,col="red")
lines(c(0,Fa[1]),c(0,Fa[2]),col="green")
lines(c(0,Fz[1]),c(0,Fz[2]),col="red")
nsteps <- ceiling(sqrt(sum(interp$tau^2))/0.05)
for (i in 1:nsteps) {
  Ft <- step_fraction(interp, i/nsteps)
  cat(Ft[1],Ft[2],"\n")
  points(Ft[1],Ft[2],pch=16,cex=0.5,col="orange")
  Sys.sleep(0.1)
}
lines(c(0,Ft[1]),c(0,Ft[2]),col="orange")
Fa<-Ft

  # Hadley's code
plot(NA, NA,xlim=c(-1.2,1.2), ylim=c(-1.2,1.2), xlab="", ylab="")
Fa <- matrix(c(0, -1), ncol = 1)
target <- Fa
start <- Fa
last_proj <- Fa
points(start[1],start[2],pch=16,cex=1,col="green")
lines(c(0,start[1]),c(0,start[2]),col="green")
grand_tour(start, total_steps = Inf, velocity = 0.05,
  step_fun = function(step, proj) {
    Sys.sleep(0.5)
    points(proj[1],proj[2],pch=16,cex=0.5,col="orange")
    last_proj <<- proj
  },
  target_fun = function(proj) {
    Sys.sleep(2)
    target <<- proj 
    start <- last_proj
    points(start[1],start[2],pch=16,cex=1,col="green")
    points(target[1],target[2],pch=16,cex=1,col="red")
    lines(c(0,start[1]),c(0,start[2]),col="green")
    lines(c(0,target[1]),c(0,target[2]),col="red")
  }
)

gen_interp <- function(interp, stepsize) {
  nsteps <- ceiling(sqrt(sum(interp$tau^2))/stepsize)
  for (i in 1:nsteps) {
    Ft <- step_fraction(interp, i/nsteps)
    for (j in 1:nrow(Ft)) {
      g[5,j]<-Ft[j,1] # relies on having ggobi open
      g[6,j]<-Ft[j,2]
      g[7,j]<-Ft[j,1]+Ft[j,2]
    }
    readline("Ready to continue?")
  }
}

gen_interp_sphere <- function(interp, stepsize) {
  nsteps <- ceiling(sqrt(sum(interp$tau^2))/stepsize)
  for (i in 1:nsteps) {
    Ft <- step_fraction(interp, i/nsteps)
    for (j in 1:nrow(Ft)) {
      g[3,j]<-Ft[j,1] # relies on having ggobi open
      g[4,j]<-Ft[j,2]
    }
    readline("Ready to continue?")
  }
}

