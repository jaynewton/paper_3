library(data.table)
library(lubridate)
library(dplyr) # use data.table rather dplyr if possible
library(lme4)
library(sandwich) # NeweyWest
library(lmtest) # coeftest
library(zoo)
library(ggplot2)
library(psych)
library(stringr)
#library(reshape2) # inclueded and enhanced in data.table

sink(file="C:/Users/Ding/Desktop/主代码.txt",append=T)
sink()
rm(list=ls())

#################################
#### Single Sort (Realized Variables)
#### Not Adjusted by FF3F
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_nm.RData")

da_m <- merge(da_all_m,da_realized_m)
#da_m[,`:=`(rkt=log(rkt),rvol=log(rvol))]

ym_index <- sort(unique(da_m$ym))
k <- 5
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
vari_level <- matrix(NA,nrow=length(ym_index),ncol=k) # vari_level denotes variable level
for (i in 1:length(ym_index)) {
  da_sub <- da_m[ym==ym_index[i],]
  da_sub <- da_sub[order(rkt),]
  n_mid <- floor(nrow(da_sub)/k)
  if ((nrow(da_sub)-n_mid*(k-2))%%2==0){
    n_f <- (nrow(da_sub)-n_mid*(k-2))/2 # f denotes first, l denotes last
    n_l <- n_f
  } else {
    n_f <- (nrow(da_sub)-n_mid*(k-2)-1)/2
    n_l <- n_f+1
  }
  x <- seq(from=n_f,to=nrow(da_sub),by=n_mid)[1:(k-1)]
  x <- c(x,nrow(da_sub))
  da_sub$group_n <- cut(1:nrow(da_sub), c(0,x),labels = 1:k)
  for (j in 1:k) {
    ret_p[i,j] <- da_sub[group_n==j,mean(ret_e)]
    # This part we do not need to take risk-free rate into consideration 
    #ret_p[i,j] <- da_sub[group_n==j,weighted.mean(ret_e,size)]
    vari_level[i,j] <- da_sub[group_n==j,mean(rkt)]
    #vari_level[i,j] <- da_sub[group_n==j,weighted.mean(rkt,size)]
  }
}

#### the part below calculate the performance of the whole sample period 
ret_p_m <- colMeans(ret_p,na.rm=T) # m denotes mean # full sample 
vari_level_m <- colMeans(vari_level,na.rm=T)
output <- cbind(vari_level_m,ret_p_m) 
output
result <- c(output[,2],hml=output[k,2]-output[1,2])
result

#### the part below calculate the performance in each year
# Note: we only consider the equal-weighted case.
# Note: In the year of 2007, we only have 11 observations for portfolio.
sample_y <- 2008:2017 
# sample_y denotes the year of the sample (the first year is not included)
result <- matrix(NA,nrow=length(sample_y)+1,ncol=k+1)
ret_p_m <- colMeans(ret_p[1:11,],na.rm=T) 
result[1,] <- c(ret_p_m,hml=as.numeric(ret_p_m[k]-ret_p_m[1]))
for (i in 2:(length(sample_y)+1)) {
  ret_p_m <- colMeans(ret_p[((i-1)*12):(i*12-1),],na.rm=T)
  result[i,] <- c(ret_p_m,hml=as.numeric(ret_p_m[k]-ret_p_m[1]))
}
result

# In both equal-weighted case and value-weighted case, 
# the worst performance of portfolio 5-1 (market neutral portfolio)
# appears in 2008, 2010 and 2015 when China stock suffer huge loss.
# This is the time when people most care about the bad black swan, 
# leading to higher price of the left tail.

#mean(result[c(2,4,9),6]) # 2, 4 and 9 corresponds to 2008, 2010 and 2015
# -0.002534446 # equal-weighted case
# 0.007957318 # value-weighted case

## Newey-West t statistic
ret_p_hl <- ret_p[,k]-ret_p[,1] # ret_p_hl denotes high minus low portfolio returns
model_nw <- lm(ret_p_hl ~ 1)
coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
#coeftest(model_nw)[1,3]

#### Adjusted by FF3F
FF3F_nm <- copy(FF3F_A_nm)
ret_p <- as.data.table(na.omit(ret_p))
ret_p$ym <- sort(unique(da_m$ym))
ret_p <- merge(ret_p,FF3F_nm,by="ym") 
## Newey-West t statistic
ret_p_FF3F <- matrix(NA,nrow=2,ncol=k+1) 
for (i in 1:k) { # the first column is the ym
  model_FF3F <- lm(ret_p[[i+1]]~ret_p[,mkt_e]+ret_p[,smb]+ret_p[,hml]) 
  # ret[,i+1] is wrong. See 1.5 of Frequently Asked Questions about data.table
  ret_p_FF3F[1,i] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,1]
  ret_p_FF3F[2,i] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,3]
}
ret_p_hl <- ret_p[[k+1]]-ret_p[[2]] 
model_FF3F <- lm(ret_p_hl~ret_p[,mkt_e]+ret_p[,smb]+ret_p[,hml])
ret_p_FF3F[1,k+1] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,1]
ret_p_FF3F[2,k+1] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,3]
ret_p_FF3F
#coeftest(model_FF3F)[1,3] # t value of portfolio 5-1

