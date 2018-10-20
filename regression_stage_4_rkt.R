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

#### Decomposition Methodology (with One Candidate Explanatory Variable)
FUN_DECOMPOSITION <- function(model) {
  da_delta_m <- data.table(ym=ymd(names(summary(model)$coefficients[,,2][,1])),
                           delta_candidate=as.numeric(summary(model)$coefficients[,,2][,1])) 
  da_delta_m <- merge(da_delta_m,da_vari_m,by="ym")
  da_gamma_m <- merge(da_gamma_core_m,
                      da_delta_m[,.(gamma_candidate=cov(ret_e,delta_candidate*candidate)/var(rkt)),by=ym],
                      by="ym")
  da_gamma_m[,gamma_residual:=gamma_core-gamma_candidate]
  
  lm_sta <- matrix(NA,nrow=2,ncol=2)
  for (i in 1:2) {
    model_nw <- lm(da_gamma_m[[i+2]]~1)
    lm_sta[i,1] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,1]
    lm_sta[i,2] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
  }
  print(lm_sta)
  print(lm_sta[,1]/da_gamma_m[,mean(gamma_core)]*100)
}

#### Decomposition Methodology (with Non-Lottery Variables)
FUN_DECOMPOSITION_NON_LOTTERY <- function(model) {
  da_delta_m <- data.table(ym=ymd(names(summary(model)$coefficients[,,2][,1])),
                           delta_candidate_1=as.numeric(summary(model)$coefficients[,,2][,1]),
                           delta_candidate_2=as.numeric(summary(model)$coefficients[,,3][,1]),
                           delta_candidate_3=as.numeric(summary(model)$coefficients[,,4][,1]),
                           delta_candidate_4=as.numeric(summary(model)$coefficients[,,5][,1]),
                           delta_candidate_5=as.numeric(summary(model)$coefficients[,,6][,1]),
                           delta_candidate_6=as.numeric(summary(model)$coefficients[,,7][,1])) 
  da_delta_m <- merge(da_delta_m,da_vari_m,by="ym")
  da_gamma_m <- merge(da_gamma_core_m,
                      da_delta_m[,.(gamma_candidate_1=cov(ret_e,delta_candidate_1*candidate_1)/var(rkt),
                                    gamma_candidate_2=cov(ret_e,delta_candidate_2*candidate_2)/var(rkt),
                                    gamma_candidate_3=cov(ret_e,delta_candidate_3*candidate_3)/var(rkt),
                                    gamma_candidate_4=cov(ret_e,delta_candidate_4*candidate_4)/var(rkt),
                                    gamma_candidate_5=cov(ret_e,delta_candidate_5*candidate_5)/var(rkt),
                                    gamma_candidate_6=cov(ret_e,delta_candidate_6*candidate_6)/var(rkt)
                      ),by=ym],by="ym")
  da_gamma_m[,gamma_residual:=gamma_core-gamma_candidate_1-gamma_candidate_2
             -gamma_candidate_3-gamma_candidate_4-gamma_candidate_5-gamma_candidate_6]
  
  lm_sta <- matrix(NA,nrow=7,ncol=2)
  for (i in 1:7) {
    model_nw <- lm(da_gamma_m[[i+2]]~1)
    lm_sta[i,1] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,1]
    lm_sta[i,2] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
  }
  print(lm_sta)
  print(lm_sta[,1]/da_gamma_m[,mean(gamma_core)]*100)
}

#### Decomposition Methodology (with Lottery Variables)
FUN_DECOMPOSITION_LOTTERY <- function(model) {
  da_delta_m <- data.table(ym=ymd(names(summary(model)$coefficients[,,2][,1])),
                           delta_candidate_1=as.numeric(summary(model)$coefficients[,,2][,1]),
                           delta_candidate_2=as.numeric(summary(model)$coefficients[,,3][,1]),
                           delta_candidate_3=as.numeric(summary(model)$coefficients[,,4][,1])) 
  da_delta_m <- merge(da_delta_m,da_vari_m,by="ym")
  da_gamma_m <- merge(da_gamma_core_m,
                      da_delta_m[,.(gamma_candidate_1=cov(ret_e,delta_candidate_1*candidate_1)/var(rkt),
                                    gamma_candidate_2=cov(ret_e,delta_candidate_2*candidate_2)/var(rkt),
                                    gamma_candidate_3=cov(ret_e,delta_candidate_3*candidate_3)/var(rkt)
                      ),by=ym],by="ym")
  da_gamma_m[,gamma_residual:=gamma_core-gamma_candidate_1-gamma_candidate_2-gamma_candidate_3]
  
  lm_sta <- matrix(NA,nrow=4,ncol=2)
  for (i in 1:4) {
    model_nw <- lm(da_gamma_m[[i+2]]~1)
    lm_sta[i,1] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,1]
    lm_sta[i,2] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
  }
  print(lm_sta)
  print(lm_sta[,1]/da_gamma_m[,mean(gamma_core)]*100)
}

####  
lm_0 <- lmList(ret_e ~ rkt | ym , data=da_m_lm)
da_gamma_core_m <- data.table(ym=ymd(names(summary(lm_0)$coefficients[,,2][,1])),
                              gamma_core=as.numeric(summary(lm_0)$coefficients[,,2][,1])) 

####  
lm_1 <- lmList(rkt ~ be | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,rkt,candidate=be)]
FUN_DECOMPOSITION(lm_1)

lm_2 <- lmList(rkt ~ size | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,rkt,candidate=size)]
FUN_DECOMPOSITION(lm_2)

lm_3 <- lmList(rkt ~ BM | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,rkt,candidate=BM)]
FUN_DECOMPOSITION(lm_3)

lm_4 <- lmList(rkt ~ mom | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,rkt,candidate=mom)]
FUN_DECOMPOSITION(lm_4)

lm_5 <- lmList(rkt ~ ret_e_tm | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,rkt,candidate=ret_e_tm)]
FUN_DECOMPOSITION(lm_5)

lm_6 <- lmList(rkt ~ illiq | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,rkt,candidate=illiq)]
FUN_DECOMPOSITION(lm_6)

lm_7 <- lmList(rkt ~ max_ret | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,rkt,candidate=max_ret)]
FUN_DECOMPOSITION(lm_7)

lm_8 <- lmList(rkt ~ rvol | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,rkt,candidate=rvol)]
FUN_DECOMPOSITION(lm_8)

lm_9 <- lmList(rkt ~ rsk | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,rkt,candidate=rsk)]
FUN_DECOMPOSITION(lm_9)

lm_10 <- lmList(rkt ~ be+size+BM+mom+ret_e_tm+illiq | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,rkt,candidate_1=be,candidate_2=size,
                        candidate_3=BM,candidate_4=mom,candidate_5=ret_e_tm,candidate_6=illiq)]
FUN_DECOMPOSITION_NON_LOTTERY(lm_10)

lm_11 <- lmList(rkt ~ max_ret+rvol+rsk | ym , data=da_m_lm)
da_vari_m <- da_m_lm[,.(ym,ret_e,rkt,candidate_1=max_ret,candidate_2=rvol,candidate_3=rsk)]
FUN_DECOMPOSITION_LOTTERY(lm_11)
