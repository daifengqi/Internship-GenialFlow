# 读入数据
setwd('E:/github/respository/GenialFlow')
library('tseries') #程序包：时间序列

dat <- read.csv('economicData/indGrowth.csv', header = T, stringsAsFactors = F)
# 平稳性检验
ind_growth <- dat[,2]
time <- dat[,1][2:nrow(dat)]
ind_dif <- diff(ind_growth)
dat[,1] <- as.Date(dat[,1])
# 获取对数&差分数据
ind_ln <- log(ind_growth)
lnind_dif <- diff(ind_ln)
df <- as.data.frame(cbind(time, lnind_dif),
                    stringsAsFactors = F)
df[,2] <- as.numeric(df[,2])
# 取93-17年的数据
x <- df[36:335, 2]
adf.test(x)
draw <- seq(from=3, to=length(x), by=3)

# 获取最终的工业增加值数据
indgrowth <- (x[draw-2]*0.33+x[draw-1]*0.33+x[draw])*0.33
ts.plot(indgrowth)  # 查看时间序列图
adf.test(indgrowth) # 平稳性检验 p=0.02656
