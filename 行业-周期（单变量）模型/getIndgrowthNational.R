setwd('E://workspace_qdf/R')
library('tseries') # 程序包：时间序列

dat <- read.csv('economicData/工业增加值（国家统计局）.csv')
dat <- t(dat)
ind <- as.data.frame(dat[104:5,])
ind[,1] <- as.numeric(as.character(ind[,1]))
ts.plot(ind[1], type='o', col='red')
ind_dif <- diff(ind[,1])
ind_dif <- c(0, ind_dif)
adf.test(ind_dif) # 平稳性检验
ts.plot(ind_dif)  # 数据探查

indgrowth <- ind_dif # 取名，以在result.R中使用 ##############
ts.plot(ind[,1])     # 看图

gdp <- read.csv('economicData/国内生产总值（国家统计局）.csv')
gdp <- t(gdp)
gdp <- as.data.frame(gdp[104:5,1])
gdp[,1] <- as.numeric(as.character(gdp[,1]))

# 对比gdp和工业增加值的数据
ts.plot(scale(diff(gdp[,1])), col = 'red') # 红色是GDP
lines(scale(diff(ind[,1])), col = 'blue')  # 蓝色是工业增加值
# 结论：基本具有完全一致的走势
# 这两个周期绝对不能代表中国的宏观经济周期
