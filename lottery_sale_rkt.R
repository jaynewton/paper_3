da_lottery_m <- read.csv("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/补充数据/彩票销售量/lottery.csv",
                     header=F,stringsAsFactors=F)[-2:-24,]

da_lottery_m <- as.data.table(da_lottery_m)
names(da_lottery_m) <- c("ym",str_sub(da_lottery_m[1,],20,-1)[-1])
da_lottery_m <- da_lottery_m[-1,]
da_lottery_m[,ym:=ymd(ym)]

da_lottery_m <- melt(da_lottery_m, id.vars="ym",variable.name="province",
                     value.name="lottery",variable.factor=F)

#da_lottery_m[,class(lottery)] # "character"
da_lottery_m[,lottery:=as.numeric(lottery)]

#da_lottery_m[,sort(unique(province))]

save(da_lottery_m,file="C:/Users/Ding/Desktop/da_lottery_m.RData")
