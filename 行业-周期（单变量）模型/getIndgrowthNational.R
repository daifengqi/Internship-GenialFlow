setwd('E://workspace_qdf/R')
library('tseries') # 程序包：时间序列

dat <- read.csv('economicData/工业增加值（国家统计局）.csv')
dat <- t(dat)
ind <- as.data.frame(dat[104:5,])
ind[,1] <- as.numeric(as.character(ind[,1]))
ind_dif <- diff(ind[,1])
ind_dif <- c(0, ind_dif)

adf.test(ind_dif)
ts.plot(ind_dif)
ind_dif