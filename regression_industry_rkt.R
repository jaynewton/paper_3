################################# 
#### Firm-Level Cross-Sectional Regression  
#### Compute the Median Data Frame
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/RData/da_ind.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/RData/da_beta_5y.RData")

# Note: In most cases, a company's industry code doesn't change month by month.
# So it's OK to use this month's industry code (as we do here).
# A similar result will be obtained from the next month's industry code.
da_m_ind <- merge(da_all_m,da_ind,by=c("ym","SecCode"))
da_m_ind <- merge(da_m_ind,da_mom_m,by=c("ym","SecCode"))
da_m_ind <- merge(da_m_ind,da_realized_m,by=c("ym","SecCode"))
da_m_ind <- merge(da_m_ind,da_beta_5y,by=c("ym","SecCode"))

da_m_ind[illiq!=0,`:=`(size=log(size),BM=log(BM),illiq=log(illiq),
                       rvol=log(rvol),rkt=log(rkt))]
da_m_ind <- da_m_ind[order(ym,SecCode),]

#### The Regression Part 
lm_fit_nw <- function(nv) { # nw denotes Newey-West HAC t statistic
  # nv denotes numbers of variables
  lm_sta <- matrix(NA,nrow=nv+1,ncol=2) # sta denotes statistics
  for (i in 1:(nv+1)) {
    model_nw <- lm(lm_sta_raw[i,] ~ 1)
    lm_sta[i,1] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,1]
    lm_sta[i,2] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
  }
  print(lm_sta)
  print(mean(as.numeric(unlist(summary(model)$adj.r.squared))))
}

####
da <- da_m_ind
nv <- 1
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rkt|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 4
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rkt+be+size+BM|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 5
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rkt+be+size+BM+mom|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 7
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rkt+be+size+BM+mom+ret_e_tm+illiq|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

####
da <- da_m_ind
nv <- 1
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~max_ret|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 1
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rvol|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 1
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rsk|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 2
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rkt+max_ret|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 2
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rkt+rvol|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 2
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rkt+rsk|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind
nv <- 4
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rkt+max_ret+rvol+rsk|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()


