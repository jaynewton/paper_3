#################################
#### Descriptive Statistics
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")

#### For all Variables
da_m <- NULL
da_m <- merge(da_all_m,da_realized_m,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_beta_5y,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_mom_m,keyby=c("ym","SecCode"))

cor(da_m[,.(rkt,max_ret,rvol,rsk,be,size,BM,mom,ret_tm,illiq)],method="pearson")
#cor(da_m[,.(rkt,max_ret,rvol,rsk,be,size,BM,mom,ret_tm,illiq)],method="spearman")

ds_2 <- describe(da_m[,.(rkt,max_ret,rvol,rsk,be,size,BM,mom,ret_tm,illiq)])[,c("mean","median","sd","skew","kurtosis","min","max")]
ds_2[,"kurtosis"] <- ds_2[,"kurtosis"]+3
#ds_2
format(ds_2,digits=3)

#################################
#### Descriptive Statistics (Take a Natural Log of rkt)
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")

#### For all Variables
da_m <- NULL
da_m <- merge(da_all_m,da_realized_m,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_beta_5y,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_mom_m,keyby=c("ym","SecCode"))

da_m[,`:=`(rkt=log(rkt),rvol=log(rvol))]

ds_2 <- describe(da_m[,.(rkt,max_ret,rvol,rsk)])[,c("mean","median","sd","skew","kurtosis","min","max")]
ds_2[,"kurtosis"] <- ds_2[,"kurtosis"]+3
#ds_2
format(ds_2,digits=4)

