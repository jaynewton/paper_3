#################################
#### Institutional Holdings

now()
da_inst <- NULL # inst denotes institution 
for (i in 2001:2017) {   
  path <- paste0("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/补充数据/机构投资者持股比例/",i,".csv")
  da_inst <- rbind(da_inst,read.csv(path,header=T)[,c(3,6,18)])
}
now()
# Note: June of 2001 is the first month that institutional holdings data is provided

names(da_inst) <- c("SecCode","TDate","inst")

da_inst <- na.omit(as.data.table(da_inst))

da_inst[,TDate:=ymd(TDate)]
da_inst[,ym:=ymd(paste0(lubridate::year(TDate),"-",lubridate::month(TDate),"-01"))]
da_inst <- da_inst[order(SecCode,TDate),]

da_inst_m <- da_inst[,.(SecCode,ym,inst)]

#### Before December of Year 2004 
da_inst_m_a <- da_inst_m[ym<ymd("2004-12-1"),]
da_inst_m_a <- da_inst_m_a[,m:=lubridate::month(ym)]
da_inst_m_a <- da_inst_m_a[m %in% c(6,12)]
da_inst_m_a[,m:=NULL]

da_inst_lm_1_a <- copy(da_inst_m_a)
da_inst_lm_1_a <- da_inst_lm_1_a[,ym:=ym-months(1)] # lm denotes last month
da_inst_lm_2_a <- copy(da_inst_m_a)
da_inst_lm_2_a <- da_inst_lm_2_a[,ym:=ym-months(2)] 
da_inst_nm_1_a <- copy(da_inst_m_a)
da_inst_nm_1_a <- da_inst_nm_1_a[,ym:=ym+months(1)] # nm denotes next month
da_inst_nm_2_a <- copy(da_inst_m_a)
da_inst_nm_2_a <- da_inst_nm_2_a[,ym:=ym+months(1)] 
da_inst_nm_3_a <- copy(da_inst_m_a)
da_inst_nm_3_a <- da_inst_nm_3_a[,ym:=ym+months(1)] 

da_inst_m_a <- rbind(da_inst_lm_2_a,da_inst_lm_1_a,da_inst_m_a,
                     da_inst_nm_1_a,da_inst_nm_2_a,da_inst_nm_3_a)
da_inst_m_a <- da_inst_m_a[order(ym,SecCode),]

#### December of Year 2004
da_inst_m_b <- da_inst_m[ym==ymd("2004-12-1"),]

da_inst_lm_1_b <- copy(da_inst_m_b)
da_inst_lm_1_b <- da_inst_lm_1_b[,ym:=ym-months(1)] # lm denotes last month
da_inst_lm_2_b <- copy(da_inst_m_b)
da_inst_lm_2_b <- da_inst_lm_2_b[,ym:=ym-months(2)] 
da_inst_nm_1_b <- copy(da_inst_m_b)
da_inst_nm_1_b <- da_inst_nm_1_b[,ym:=ym+months(1)] # nm denotes next month

da_inst_m_b <- rbind(da_inst_lm_2_b,da_inst_lm_1_b,da_inst_m_b,da_inst_nm_1_b)
da_inst_m_b <- da_inst_m_b[order(ym,SecCode),]

#### After Year 2005 (Year 2005 is included)
da_inst_m_c <- da_inst_m[ym>=ymd("2005-1-1"),]

da_inst_lm_c <- copy(da_inst_m_c)
da_inst_lm_c <- da_inst_lm_c[,ym:=ym-months(1)] # lm denotes last month
da_inst_nm_c <- copy(da_inst_m_c)
da_inst_nm_c <- da_inst_nm_c[,ym:=ym+months(1)] # nm denotes next month

da_inst_m_c <- rbind(da_inst_lm_c,da_inst_m_c,da_inst_nm_c)
da_inst_m_c <- da_inst_m_c[order(ym,SecCode),]

#### combine all
da_inst_m <- rbind(da_inst_m_a,da_inst_m_b,da_inst_m_c)
da_inst_m <- da_inst_m[order(ym,SecCode),]

save(da_inst_m,file="C:/Users/Ding/Desktop/da_inst_m.RData")

