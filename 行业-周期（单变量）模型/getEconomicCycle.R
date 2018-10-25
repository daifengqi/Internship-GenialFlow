setwd('E:/workspace_qdf/R')

economic <- read.csv('economicData/economic_data.csv',
                     stringsAsFactors = F)
economic <- economic[2:337,]
rownames(economic) <- 1:nrow(economic)

# 这里选择开始的年份
Year <- 1997
ecostar <- (Year-1990)*12+1 # 每年12个月
ppi <- economic[ecostar:nrow(economic),1:2]
ppi[,2] <- as.numeric(ppi[,2])
draw <- seq(from=3, to=nrow(ppi),by=3)
ppidata <- (ppi[draw-2, 2]+ppi[draw-1, 2]+ppi[draw, 2])/3

ts.plot(ppidata)
adf.test(ppidata) # 无法通过平稳性检验
# 没有关系，因为通过协整检验是比较容易的

# PPI数据能代表宏观经济周期吗