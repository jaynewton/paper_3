#################################
#### Top 10 Unrestricted Stock Holders’ Unrestricted A Shares Holding Percentage to Unrestricted A Shares

now()
da_top <- NULL  
for (i in 2001:2017) {   
  path <- paste0("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/补充数据/机构投资者持股比例/",i,".csv")
  da_top <- rbind(da_top,read.csv(path,header=T)[,c(3,6,79)])
}
now()

names(da_top) <- c("SecCode","TDate","top")

da_top <- na.omit(as.data.table(da_top))

da_top[,TDate:=ymd(TDate)]
da_top[,ym:=ymd(paste0(lubridate::year(TDate),"-",lubridate::month(TDate),"-01"))]
da_top <- da_top[order(SecCode,TDate),]
da_top_m <- da_top[,.(SecCode,ym,top)]

#### Before December of Year 2004 
da_top_m_a <- da_top_m[ym<ymd("2004-12-1"),]
da_top_m_a <- da_top_m_a[,m:=lubridate::month(ym)]
da_top_m_a <- da_top_m_a[m %in% c(6,12)]
da_top_m_a[,m:=NULL]

da_top_lm_1_a <- copy(da_top_m_a)
da_top_lm_1_a <- da_top_lm_1_a[,ym:=ym-months(1)] # lm denotes last month
da_top_lm_2_a <- copy(da_top_m_a)
da_top_lm_2_a <- da_top_lm_2_a[,ym:=ym-months(2)] 
da_top_nm_1_a <- copy(da_top_m_a)
da_top_nm_1_a <- da_top_nm_1_a[,ym:=ym+months(1)] # nm denotes next month
da_top_nm_2_a <- copy(da_top_m_a)
da_top_nm_2_a <- da_top_nm_2_a[,ym:=ym+months(1)] 
da_top_nm_3_a <- copy(da_top_m_a)
da_top_nm_3_a <- da_top_nm_3_a[,ym:=ym+months(1)] 

da_top_m_a <- rbind(da_top_lm_2_a,da_top_lm_1_a,da_top_m_a,
                     da_top_nm_1_a,da_top_nm_2_a,da_top_nm_3_a)
da_top_m_a <- da_top_m_a[order(ym,SecCode),]

#### December of Year 2004
da_top_m_b <- da_top_m[ym==ymd("2004-12-1"),]

da_top_lm_1_b <- copy(da_top_m_b)
da_top_lm_1_b <- da_top_lm_1_b[,ym:=ym-months(1)] # lm denotes last month
da_top_lm_2_b <- copy(da_top_m_b)
da_top_lm_2_b <- da_top_lm_2_b[,ym:=ym-months(2)] 
da_top_nm_1_b <- copy(da_top_m_b)
da_top_nm_1_b <- da_top_nm_1_b[,ym:=ym+months(1)] # nm denotes next month

da_top_m_b <- rbind(da_top_lm_2_b,da_top_lm_1_b,da_top_m_b,da_top_nm_1_b)
da_top_m_b <- da_top_m_b[order(ym,SecCode),]

#### After Year 2005 (Year 2005 is included)
da_top_m_c <- da_top_m[ym>=ymd("2005-1-1"),]

da_top_lm_c <- copy(da_top_m_c)
da_top_lm_c <- da_top_lm_c[,ym:=ym-months(1)] # lm denotes last month
da_top_nm_c <- copy(da_top_m_c)
da_top_nm_c <- da_top_nm_c[,ym:=ym+months(1)] # nm denotes next month

da_top_m_c <- rbind(da_top_lm_c,da_top_m_c,da_top_nm_c)
da_top_m_c <- da_top_m_c[order(ym,SecCode),]

#### combine all
da_top_m <- rbind(da_top_m_a,da_top_m_b,da_top_m_c)
da_top_m <- da_top_m[order(ym,SecCode),]

save(da_top_m,file="C:/Users/Ding/Desktop/da_top_m.RData")

