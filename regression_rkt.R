################################# 
#### Firm-Level Cross-Sectional Regression  
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")
#load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")

da_m_lm <- merge(da_all_m,da_realized_m,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_beta_5y,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_mom_m,by=c("ym","SecCode"))

#skew(da_realized_m$rkt) # 10.50206
# That's why we should take log of rkt when it is used in the regression.

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

lm_5 <- lmList(ret_e ~ max_ret | ym , data=da_m_lm)
lm_fit_nw(lm_5,1)
lm_6 <- lmList(ret_e ~ rvol | ym , data=da_m_lm)
lm_fit_nw(lm_6,1)
lm_7 <- lmList(ret_e ~ rsk | ym , data=da_m_lm)
lm_fit_nw(lm_7,1)

lm_8 <- lmList(ret_e ~ rkt+max_ret | ym , data=da_m_lm)
lm_fit_nw(lm_8,2)
lm_9 <- lmList(ret_e ~ rkt+rvol | ym , data=da_m_lm)
lm_fit_nw(lm_9,2) 
lm_10 <- lmList(ret_e ~ rkt+rsk | ym , data=da_m_lm)
lm_fit_nw(lm_10,2)
lm_11 <- lmList(ret_e ~ rkt+max_ret+rvol+rsk | ym , data=da_m_lm)
lm_fit_nw(lm_11,4)



