da_gdp_y <- read.csv("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/补充数据/各省GDP/GDP_province.csv",
                   header=F,stringsAsFactors=F)[-2:-24,]

da_gdp_y <- as.data.table(da_gdp_y)
names(da_gdp_y) <- c("y",str_sub(da_gdp_y[1,],29,-1)[-1])

da_gdp_y <- da_gdp_y[-1,]
da_gdp_y <- da_gdp_y[y>=2007,]

da_gdp_y <- melt(da_gdp_y, id.vars="y",variable.name="province",
                     value.name="gdp",variable.factor=F)

#da_gdp_y[,class(gdp)] # "character"
da_gdp_y[,gdp:=as.numeric(gdp)]

da_gdp_m <- da_gdp_y[,.(ym=seq.Date(ymd(paste0(y[1],"-1-1")),ymd(paste0(y[.N],"-12-1")),by="month"),
                        gdp=rep(gdp,each=12)),by=province]

#da_gdp_m[,sort(unique(province))]

save(da_gdp_m,file="C:/Users/Ding/Desktop/da_gdp_m.RData")

