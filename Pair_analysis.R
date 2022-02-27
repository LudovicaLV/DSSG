#load DescTools library
library(DescTools)

# add appropriate directory
load(paste(".../DataFrame_Pair.RData", sep = ""))

pairA <- "A"
pairB <- "B"

# list used to keep track of counts
list_breaks <- list()
for (i in 1:9){
  list_breaks[[i]] <- c(0,0,0,0,0)
}

#consider the angle difference for individual A and B - for classification
diffA_abs <- vector()
for (i in 1:length(data_to_save$diffA)){
  diffA_abs[i] <- abs(data_to_save$diffA[i])
}

diffB_abs <- vector()
for (i in 1:length(data_to_save$diffB)){
  diffB_abs[i] <- abs(data_to_save$diffB[i])
}

# distance intervals
pair_distMin <- c(0, 50, 100, 200, 500, 1000, 2000, 3000, 5000)
pair_distMax <- c(50, 100, 200, 500, 1000, 2000, 3000, 5000, 10000) 

#angle classification parameters
a1 <- 67.5
a2 <- 112.5
a3 <- 247.5
a4 <- 292.5

#for loop for the analysis
for (pair_dist in 1:length(pair_distMin)){
  
  distMin <- pair_distMin[pair_dist]
  distMax <- pair_distMax[pair_dist]
  
  for (i in 1:length(diffB_abs)){
    #if pair distance in distance interval
    if (isTRUE(data_to_save$dist[i] < distMax & data_to_save$dist[i] >= distMin)){
      #classification: 1 (both towards), 2 (both away), 3 (A towards, B away), 4 (A away, B towards), 5 (all the other possibilities)
      if ((diffA_abs[i] <= a1 | diffA_abs[i] > a4) & (diffB_abs[i] <= a1 | diffB_abs[i] > a4)){
        list_breaks[[pair_dist]][1] <- list_breaks[[pair_dist]][1] + 1
      }else if (diffA_abs[i] <= a3 & diffA_abs[i] > a2 & diffB_abs[i] <= a3 & diffB_abs[i] > a2){
        list_breaks[[pair_dist]][2] <- list_breaks[[pair_dist]][2] + 1
      }else if ((diffA_abs[i] <= a1 | diffA_abs[i] > a4) & diffB_abs[i] <= a3 & diffB_abs[i] > a2){
        list_breaks[[pair_dist]][3] <- list_breaks[[pair_dist]][3] + 1
      }else if (diffA_abs[i] <= a3 & diffA_abs[i] > a2 & (diffB_abs[i] <= a1 | diffB_abs[i] > a4)){
        list_breaks[[pair_dist]][4] <- list_breaks[[pair_dist]][4] + 1
      }else{
        list_breaks[[pair_dist]][5] <- list_breaks[[pair_dist]][5] + 1}
    }
  }
  
  ## we consider only behaviours 1-4 
  sum1 <- list_breaks[[pair_dist]][1]
  sum2 <- list_breaks[[pair_dist]][2]
  sum3 <- list_breaks[[pair_dist]][3]
  sum4 <- list_breaks[[pair_dist]][4]
  total <- list_breaks[[pair_dist]][1] + list_breaks[[pair_dist]][2] + list_breaks[[pair_dist]][3] + list_breaks[[pair_dist]][4]  
  
  final_sum <- c(sum1, sum2, sum3, sum4)
  total <- sum(final_sum)
  
  ## statistical analysis
  CI_m <- MultinomCI(final_sum, conf.level = 0.95, sides = "two.sided", method = "goodman")
  check_5_lim <- (sum1 >= 5) & (sum2 >=5) & (sum3 >= 5) & (sum4 >= 5)
  
  for (chosen_sum in 1:4){  
    sum <- final_sum[chosen_sum]
    if (check_5_lim == FALSE){
        print(paste(pairA, " and ", pairB, "& [", distMin , "," , distMax, ")", " & ", total, " & ", sum, " & ", chosen_sum, " & ", round(CI_m[chosen_sum, 2],4), " & ", round(CI_m[chosen_sum, 3],4), " & *", sep = ""))
    }else{
        if (CI_m[chosen_sum, 2] > 0.25){
          print(paste(pairA, " and ", pairB, "& [", distMin , "," , distMax, ")", " & ", total, " & ", sum, " & ", chosen_sum, " & ", round(CI_m[chosen_sum, 2],4), " & ", round(CI_m[chosen_sum, 3],4), " & checkmark", sep = ""))
        }else{
          print(paste(pairA, " and ", pairB, "& [", distMin , "," , distMax, ")", " & ", total, " & ", sum, " & ", chosen_sum, " & ", round(CI_m[chosen_sum, 2],4), " & ", round(CI_m[chosen_sum, 3],4), " &", sep = ""))
        }}
  }
}

#for label x-axis
inter <- c("0-50m", "50-100m", "100-200m", "200-500m", "500m-1km", "1-2km", "2-3km", "3-5km", "5-10km")

#barplot pair analysis
v <- unlist(list_breaks)
m <- matrix(v, ncol = 5, byrow = TRUE)
mt <- t(m)
c <- c("#648FFF", "#DC267F", "#FFB000", "#FE6100", "#B2B0B5") # accessible color palette
barplot(mt, names.arg = inter, col = c, ylab = "Instances", beside = TRUE)

