#################################
#### Strong Lottery Demand Stocks Only
load("F:/我的论文/第五篇/RData/da_all_m.RData")

ym_index <- sort(unique(da_all_m$ym))
k <- 2
da_all_m_intermediate <- data.table() 
for (i in 1:length(ym_index)) {
  da_sub <- da_all_m[ym==ym_index[i],]
  da_sub <- da_sub[order(max_ret),]
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
  da_sub <- da_sub[group_n %in% k,]
  da_all_m_intermediate <- rbind(da_all_m_intermediate,da_sub)
}
da_all_m_intermediate <- da_all_m_intermediate[order(ym,SecCode),]
da_all_m_strong_lottery_max  <- da_all_m_intermediate
da_all_m_strong_lottery_max[,group_n:=NULL]

#save(da_all_m_strong_lottery_max,file="C:/Users/Ding/Desktop/da_all_m_strong_lottery_max.RData")

################################# 
#### Firm-Level Cross-Sectional Regression  
load("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/RData/da_all_m_strong_lottery_max.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/RData/da_beta_5y.RData")

da_m_lm <- merge(da_all_m_strong_lottery_max,da_mom_m,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_realized_m,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_beta_5y,by=c("ym","SecCode"))

da_m_lm[illiq!=0,`:=`(size=log(size),BM=log(BM),illiq=log(illiq),
                      rvol=log(rvol),rkt=log(rkt))]
da_m_lm <- da_m_lm[order(ym,SecCode),]

#### The Regression Part
lm_fit_nw <- function(model,nv) { # nw denotes Newey-West HAC t statistic
  # nv denotes numbers of variables
  lm_sta <- matrix(NA,nrow=nv+1,ncol=2) # sta denotes statistics
  for (i in 1:(nv+1)) {
    model_nw <- lm(summary(model)$coefficients[,,i][,1] ~ 1)
    lm_sta[i,1] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,1]
    lm_sta[i,2] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
  }
  print(lm_sta)
  print(mean(as.numeric(unlist(summary(model)$adj.r.squared))))
}

#### 
lm_1 <- lmList(ret_e ~ rkt | ym , data=da_m_lm)
lm_fit_nw(lm_1,1) 
lm_2 <- lmList(ret_e ~ rkt+be+size+BM | ym , data=da_m_lm)
lm_fit_nw(lm_2,4)
lm_3 <- lmList(ret_e ~ rkt+be+size+BM+mom | ym , data=da_m_lm)
lm_fit_nw(lm_3,5)
lm_4 <- lmList(ret_e ~ rkt+be+size+BM+mom+ret_e_tm+illiq | ym , data=da_m_lm)
lm_fit_nw(lm_4,7)

lm_5 <- lmList(ret_e ~ rkt+max_ret | ym , data=da_m_lm)
lm_fit_nw(lm_5,2)
lm_6 <- lmList(ret_e ~ rkt+rvol | ym , data=da_m_lm)
lm_fit_nw(lm_6,2) 
lm_7 <- lmList(ret_e ~ rkt+rsk | ym , data=da_m_lm)
lm_fit_nw(lm_7,2)
lm_8 <- lmList(ret_e ~ rkt+max_ret+rvol+rsk | ym , data=da_m_lm)
lm_fit_nw(lm_8,4)


