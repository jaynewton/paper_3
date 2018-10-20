#################################
#### Kurtosis (Based on Daily Data in a Year)
#### Scholes and Williams(1977)
load("F:/我的论文/第五篇/RData/da_all_1.RData")
load("F:/我的论文/第五篇/RData/da_all_2.RData")
load("F:/我的论文/第五篇/RData/da_all_3.RData")
load("F:/我的论文/第五篇/RData/da_all_4.RData")
load("F:/我的论文/第五篇/RData/da_all_5.RData")
load("F:/我的论文/第五篇/RData/da_all_6.RData")
load("F:/我的论文/第五篇/RData/da_all_7.RData")
load("F:/我的论文/第五篇/RData/da_all_8.RData")
# Note: Calculation in this part is based on the formation month.

da_kt <- NULL
da_kt <- as.data.table(rbind(da_all_1,da_all_2,da_all_3,da_all_4,
                               da_all_5,da_all_6,da_all_7,da_all_8))
da_kt <- da_kt[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                     (SecCode>2000 & SecCode<3000) | (SecCode>300000 & SecCode<301000),]
da_kt <- na.omit(da_kt)

da_kt[,TDate:=ymd(TDate)]
da_kt[,`:=`(y=lubridate::year(TDate), 
              m=lubridate::month(TDate))]
da_kt[,ym:=ymd(paste0(y,"-",m,"-01"))]
da_kt <- da_kt[order(SecCode,TDate),]
da_kt[,adpr_ld:=c(NA,adpr[-(.N)]),by=SecCode]
da_kt <- na.omit(da_kt)
da_kt[,ret_d:=log(adpr)-log(adpr_ld)] 
da_kt[,`:=`(ret_e=ret_d-rf)] 
da_kt <- na.omit(da_kt)

da_kt <- da_kt[,.(ym,SecCode,ret_e)]
da_kt_tm <- da_kt[order(ym,SecCode),]

save(da_kt_tm,file="C:/Users/Ding/Desktop/da_kt_tm.RData")

#################################
load("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/RData/da_kt_tm.RData")

da_kt <- da_kt_tm[ym>=ymd("1999-2-1") & ym<=ymd("2004-12-31"),]
#da_kt <- da_kt_tm[ym>=ymd("2004-2-1") & ym<=ymd("2009-12-31"),]
#da_kt <- da_kt_tm[ym>=ymd("2009-2-1") & ym<=ymd("2013-12-31"),]
#da_kt <- da_kt_tm[ym>=ymd("2013-2-1") & ym<=ymd("2017-12-31"),]

da_kt_1 <- copy(da_kt)
da_kt_1[,ym:=ym+months(1)]
da_kt_2 <- copy(da_kt)
da_kt_2[,ym:=ym+months(2)]
da_kt_3 <- copy(da_kt)
da_kt_3[,ym:=ym+months(3)]
da_kt_4 <- copy(da_kt)
da_kt_4[,ym:=ym+months(4)]
da_kt_5 <- copy(da_kt)
da_kt_5[,ym:=ym+months(5)]
da_kt_6 <- copy(da_kt)
da_kt_6[,ym:=ym+months(6)]
da_kt_7 <- copy(da_kt)
da_kt_7[,ym:=ym+months(7)]
da_kt_8 <- copy(da_kt)
da_kt_8[,ym:=ym+months(8)]
da_kt_9 <- copy(da_kt)
da_kt_9[,ym:=ym+months(9)]
da_kt_10 <- copy(da_kt)
da_kt_10[,ym:=ym+months(10)]
da_kt_11 <- copy(da_kt)
da_kt_11[,ym:=ym+months(11)]
da_kt_all <- rbind(da_kt,da_kt_1,da_kt_2,
                    da_kt_3,da_kt_4,da_kt_5,
                    da_kt_6,da_kt_7,da_kt_8,
                    da_kt_9,da_kt_10,da_kt_11)
da_kt_1 <- NULL
da_kt_2 <- NULL
da_kt_3 <- NULL
da_kt_4 <- NULL
da_kt_5 <- NULL
da_kt_6 <- NULL
da_kt_7 <- NULL
da_kt_8 <- NULL
da_kt_9 <- NULL
da_kt_10 <- NULL
da_kt_11 <- NULL
da_kt_12 <- NULL

da_kt_all <- da_kt_all[order(ym,SecCode),]

da_kt_all_1 <- da_kt_all[ym>=ymd("2000-1-1") & ym<=ymd("2004-12-31"),]
#da_kt_all_2 <- da_kt_all[ym>=ymd("2005-1-1") & ym<=ymd("2009-12-31"),]
#da_kt_all_3 <- da_kt_all[ym>=ymd("2010-1-1") & ym<=ymd("2013-12-31"),]
#da_kt_all_4 <- da_kt_all[ym>=ymd("2014-1-1") & ym<=ymd("2017-12-31"),]

save(da_kt_all_1,file="C:/Users/Ding/Desktop/da_kt_all_1.RData")
#save(da_kt_all_2,file="C:/Users/Ding/Desktop/da_kt_all_2.RData")
#save(da_kt_all_3,file="C:/Users/Ding/Desktop/da_kt_all_3.RData")
#save(da_kt_all_4,file="C:/Users/Ding/Desktop/da_kt_all_4.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/RData/da_kt_all_1.RData")
#load("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/RData/da_kt_all_2.RData")
#load("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/RData/da_kt_all_3.RData")
#load("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/RData/da_kt_all_4.RData")

# Since we are faced with storage space limitation, don't use copy() here.
da_kt <- da_kt_all_1
#da_kt <- da_kt_all_2
#da_kt <- da_kt_all_3
#da_kt <- da_kt_all_4

da_kt <- da_kt[,.(ym,SecCode,ret_e,intercept,mkt_e)]

FUN_KT <- function(vari) {
  kt <- ifelse(length(vari)<200,as.numeric(NA),
               kurtosi(vari)+3)
  return(kt)
}

now()
da_kt_y <- da_kt[,.(kt=FUN_KT(ret_e)),by=.(ym,SecCode)]
now()

da_kt_y_1 <- na.omit(da_kt_y)
#da_kt_y_2 <- na.omit(da_kt_y)
#da_kt_y_3 <- na.omit(da_kt_y)
#da_kt_y_4 <- na.omit(da_kt_y)

save(da_kt_y_1,file="C:/Users/Ding/Desktop/da_kt_y_1.RData")
#save(da_kt_y_2,file="C:/Users/Ding/Desktop/da_kt_y_2.RData")
#save(da_kt_y_3,file="C:/Users/Ding/Desktop/da_kt_y_3.RData")
#save(da_kt_y_4,file="C:/Users/Ding/Desktop/da_kt_y_4.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/RData/da_kt_y_1.RData")
load("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/RData/da_kt_y_2.RData")
load("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/RData/da_kt_y_3.RData")
load("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/RData/da_kt_y_4.RData")

da_kt_y <- rbind(da_kt_y_1,da_kt_y_2,da_kt_y_3,da_kt_y_4)

save(da_kt_y,file="C:/Users/Ding/Desktop/da_kt_y.RData")
