#load libraries
library(factoextra) 
library(pheatmap)

#we first cluster by initial position
xx_o <- vector()
yy_o <- vector()

for (i in list_open_ind){
  xx_o <- c(xx_o, mid_to_mid[[i]]$x[1])
  yy_o <- c(yy_o, mid_to_mid[[i]]$y[1])
}

start_points <- data.frame(x = xx_o, y = yy_o)
data <- as.matrix(start_points)

#analysis to choose the number of clusters 
#elbow method (wss: within-cluster sum of square)
fviz_nbclust(data, FUN = hcut, method = "wss")
nclust <- 4

#clustering - heatmap
hh <- pheatmap(data,main = "Initial position",
               cutree_rows = nclust, cluster_cols = F, fontsize = 14)

clusters_row <- cutree(hh$tree_row, k = nclust)
clusters_row_points <- clusters_row

col <- vector()
col[clusters_row_points == 1] <- "blue"
col[clusters_row_points == 2] <- "red"
col[clusters_row_points == 3] <- "green"
col[clusters_row_points == 4] <- "orange"

plot(xx_o, yy_o, col = col, xlab = "Lon", ylab = "Lat", pch = 19)

#group by starting point
ind_cluster <- list()

for (i in unique(clusters_row_points)){
  ind_cluster[[i]] <- list_open_ind[clusters_row_points == i] 
}

#calculate eccentricity
e <- ellipticity(bv,av)
a <- angv
df_ell <- data.frame(e = e, a = a)

clusters_tot_list_open <- list()
for (i in unique(clusters_row_points)){
  clusters_tot_list_open[[i]] <- vector()
}

#by starting point, further cluster ellipses
k <- 1

#extract eccentricity and angle
ek <- e[clusters_row_points == k] 
ak <- a[clusters_row_points == k] 
ea_df <- data.frame(e = ek, a = ak)
ea_df <- scale(ea_df)
data <- as.matrix(ea_df)

#analysis to choose the number of clusters - elbow method
fviz_nbclust(data, FUN = hcut, method = "wss")
nclust <- 3

#heatmap
hh <- pheatmap(data,main = "Ellipse features",
               cutree_rows = nclust, cluster_cols = F, fontsize = 14)

clusters_row_displ <- cutree(hh$tree_row, k = nclust)

col <- vector()
col[clusters_row_displ == 1] <- "blue"
col[clusters_row_displ == 2] <- "red"
col[clusters_row_displ == 3] <- "green"

plot(ek, ak, col = col, xlab = "e", ylab = "a", pch = 19)
clusters_tot_list_open[[k]] <- clusters_row_displ

#plot all ellipses 
plot(NA,NA,xlim = c(min(minx), max(maxx)), ylim = c(min(miny), max(maxy)), type = "l")
vk <- match(ind_cluster[[k]], list_open_ind)
for (ind_cl in vk){
  xyDirect<-calculateEllipse(cv[[ind_cl]][1], cv[[ind_cl]][2], av[ind_cl], bv[ind_cl], 180/pi*angv[ind_cl])
  points(xyDirect[,1],xyDirect[,2],type='l', col=col[ind_cl])
}
