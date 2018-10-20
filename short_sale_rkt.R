#################################
#### Stocks Available for Short Sale 
load("F:/我的论文/第五篇/RData/da_all_m.RData")
da_short <- read.csv("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/补充数据/融资融券/融资融券.csv",
                     stringsAsFactors=F)[,c(1,5,7:10)]

da_short <- data.table(da_short)
names(da_short) <- c("SecCode","security_category","underly_category",
                     "in_date","out_date","underly_flag")
da_short <- da_short[security_category==1 & underly_category==20 & underly_flag==1,
                     .(SecCode,in_date,out_date)]
#da_short[,out_date] # empty column
da_short[,out_date:=NULL]

da_short[,in_date:=ymd(in_date)]
da_short[,in_date_ym:=ymd(paste0(lubridate::year(in_date),"-",
                                 lubridate::month(in_date),"-01"))]
da_short[,in_date_ym:=in_date_ym+months(1)]

da_short <- da_short[in_date_ym <= ymd("2017-12-1"),]
da_short_m <- da_short[,.(ym=seq.Date(in_date_ym,ymd("2017-12-1"),by="month")),by=SecCode]

da_no_short_m <- fsetdiff(da_all_m[,.(SecCode,ym)],da_short_m)
#da_no_short_m <- setdiff(da_all_m[,.(SecCode,ym)],da_short_m) # same result but a little bit slower
da_no_short_m[,short:=0]

da_short_m <- fintersect(da_all_m[,.(SecCode,ym)],da_short_m)
#da_short_m <- fintersect(da_all_m[,.(SecCode,ym)],da_short_m) # same result but a little bit slower
#da_short_m <- merge(da_all_m[,.(SecCode,ym)],da_short_m) # same result
da_short_m[,short:=1]

da_short_m <- rbind(da_short_m,da_no_short_m)
da_short_m <- da_short_m[order(SecCode,ym),]

save(da_short_m,file="C:/Users/Ding/Desktop/da_short_m.RData")

