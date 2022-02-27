#load conicfit library
library(conicfit)

#open DAR classification

# d_first_last is the vector of distance between first and last point
d_first_last <- vector()

for (i in list_open_ind){
  dx <- mid_to_mid[[i]]$x[1]
  dy <- mid_to_mid[[i]]$y[1]
  ll <- length(mid_to_mid[[i]]$x)
  dxl <- mid_to_mid[[i]]$x[ll]
  dyl <- mid_to_mid[[i]]$y[ll]
  dd <- distm(c(dx,dy), c(dxl,dyl), fun = distHaversine)
  d_first_last <- c(d_first_last, dd)
}

#fit a convex hull (ch), extract diameter and perimeter
diam <- vector()
perim <- vector()

#fit an ellipse, extract center, axis and tilt angle
av <- vector() #semi-major
bv <- vector() #semi-minor
cv <- list()
for (i in 1:length(list_open_ind)){
  cv[[i]] <- vector()
}
lcv <- 1
angv <- vector()

for (i in list_open_ind){
  x <- mid_to_mid[[i]]$x
  y <- mid_to_mid[[i]]$y
  #fit - convex hull
  ch <- chull(x, y)
  
  #plot - convex hull
  #polygon(x[ch],y[ch])
  
  #diameter - convex hull
  dm <- distm(cbind(x[ch],y[ch]), fun = distHaversine)
  diam <- c(diam, max(dm))
  
  #perimeter - convex hull 
  lc1 <- length(ch) - 1
  per <- 0 
  for (i in 1:lc1){
    j <- i + 1
    i1 <- ch[i]
    i2 <- ch[j]
    dd <- distm(c(x[i1],y[i1]), c(x[i2],y[i2]), fun = distHaversine)
    per <- per + dd
  }
  #last to first segment
  lc <- length(ch)
  dd <- distm(c(x[lc],y[lc]), c(x[ch[1]],y[ch[1]]), fun = distHaversine)
  per <- per + dd
  perim <- c(perim, per)
  
  #fit - ellipse
  data <- data.frame(x=x, y=y)
  data_m <- as.matrix(data)
  ellipDirect <- EllipseDirectFit(data_m)
  
  #conversion of algebraic parameters to geometric parameters
  #store center, axis and angle
  ellipDirectG <- AtoG(ellipDirect)$ParG
  cv[[lcv]] <- c(ellipDirectG[1], ellipDirectG[2])
  lcv <- lcv + 1
  av <- c(av, ellipDirectG[3])
  bv <- c(bv, ellipDirectG[4])
  angv <- c(angv, ellipDirectG[5])
  
  #to plot - ellipse
  #xyDirect<-calculateEllipse(ellipDirectG[1], ellipDirectG[2], ellipDirectG[3], ellipDirectG[4], 180/pi*ellipDirectG[5])
  #points(xyDirect[,1],xyDirect[,2],type='l', col='cyan')
}

