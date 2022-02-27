#load CSA incidence data
setwd(".../Data_CSA/")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.table, header = FALSE, sep = ",", stringsAsFactors=FALSE)
names <- list.files(path = ".")

#initial vector to store day "0"
beg_v <- vector()

for (i in 1:length(names)){
  vs <- vector()
  vd <- vector()
  exp_coeff <- vector()
  print(i)
  rt <-  data.frame(t(myfiles[[i]]))
  
  #7-day lagged moving average
  v <- rt$X1
  lv <- length(v) - 7
  la <- vector()
  for (j in 1:lv){
    f <- j + 6
    m <- mean(v[j:f])
    la <- c(la,m)
  }
  
  end <- fp_new[i] + 15
  
  for (j in fp_new[i]:end){
    b <- j
    e <- b + 14
    df <- data.frame(t = c(1:15), inc = round(la[b:e]))
    
    #fit linear model 
    mult_lm <- lm(log(inc) ~ t, data = df)
    coef(mult_lm)
    lm_coef <- coef(mult_lm)
    
    #create the plot
    plot(df$t, df$inc, main = j)
    lines(df$t, exp(lm_coef[1])*exp(lm_coef[2]*df$t), col = "dodgerblue", lwd = 2)
    
    #store fit parameter alpha, error of the fit and corresponding initial day
    exp_coeff <- c(exp_coeff, lm_coef[2])
    s <- sum(abs(mult_lm$residuals))
    vd <- c(vd,j)
    vs <- c(vs,s)
  }
  
  #extract index of minimum error
  index <- which (vs == min(vs))
  
  #extract index of minimum error and sufficient vigorous growth
  if (max (exp_coeff) >= 0.25){
    while(exp_coeff[index] < 0.25){
      vs <- vs[-index]
      exp_coeff <- exp_coeff[-index]
      vd <- vd[-index]
      index <- which (vs == min(vs))
    }}else{
      while(exp_coeff[index] < 0.2){
        vs <- vs[-index]
        exp_coeff <- exp_coeff[-index]
        vd <- vd[-index]
        index <- which (vs == min(vs))
      }
    }
  
  #store day "0"
  beg_v <- c(beg_v, vd[index])
}
