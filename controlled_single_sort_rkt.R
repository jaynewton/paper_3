#################################
#### Single Sort with Control
#### The Regression Part 
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")
#load("F:/我的论文/第五篇/RData/da_realized_m_a.RData")

da_realized_m <- da_realized_m[ym<=ymd("2016-12-31"),]

da_m <- merge(da_all_m,da_realized_m)
#da_m <- merge(da_all_m,da_realized_m_a)

da_m[,rkt_max_ret:=summary(lm(log(rkt) ~ max_ret))$residuals,by=ym]
da_m[,rkt_rvol:=summary(lm(log(rkt) ~ rvol))$residuals,by=ym]
da_m[,rkt_rsk:=summary(lm(log(rkt) ~ rsk))$residuals,by=ym]
da_m[,rkt_max_rsk:=summary(lm(log(rkt) ~ max_ret+rsk))$residuals,by=ym]

#### Not Adjusted by FF3F
ym_index <- sort(unique(da_m$ym))
k <- 5
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
vari_level <- matrix(NA,nrow=length(ym_index),ncol=k) # vari_level denotes variable level
for (i in 1:length(ym_index)) {
  da_sub <- da_m[ym==ym_index[i],]
  #da_sub <- da_sub[order(rkt_max_ret),] 
  #da_sub <- da_sub[order(rkt_rvol),]
  #da_sub <- da_sub[order(rkt_rsk),]
  da_sub <- da_sub[order(rkt_max_rsk),]
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
  }
}

ret_p_m <- colMeans(ret_p,na.rm=T) # m denotes mean # full sample 
c(ret_p_m,hl=as.numeric(ret_p_m[k]-ret_p_m[1]))

## Newey-West t statistic
ret_p_hl <- ret_p[,k]-ret_p[,1] # ret_p_hl denotes high minus low portfolio returns
model_nw <- lm(ret_p_hl ~ 1) # nw denotes Newey-West
coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]

#### Adjusted by FF3F
ret_p <- as.data.table(na.omit(ret_p))
ret_p$ym <- sort(unique(da_m$ym))
ret_p <- merge(ret_p,FF3F_nm,by="ym") 
## Newey-West t statistic
ret_p_FF3F <- matrix(NA,nrow=2,ncol=k+1) 
for (i in 1:k) { # the first column is the week
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
