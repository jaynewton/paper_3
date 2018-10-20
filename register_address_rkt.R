da_address <- read.csv("F:/我的论文/第五篇/主代码/realized kurtosis puzzle/补充数据/上市公司信息/Company Information.csv",
                     stringsAsFactors=F)[,c(6,22)]

da_address <- as.data.table(da_address)
names(da_address) <- c("SecCode","province")
da_address[,class(SecCode)]
da_address[,SecCode:=as.integer(SecCode)]
da_address <- na.omit(da_address)

da_address <- da_address[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                           (SecCode>2000 & SecCode<3000) | (SecCode>300000 & SecCode<301000),]

#da_address[,sort(unique(province))]
da_address[province=="Jilin(Prov.)",province:="Jilin"]
da_address[province=="Fujiang",province:="Fujian"]
da_address[province=="Zhejinag",province:="Zhejiang"]

save(da_address,file="C:/Users/Ding/Desktop/da_address.RData")
