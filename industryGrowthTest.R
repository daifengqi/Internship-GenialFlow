# 读入数据
setwd('E:/workspace_qdf/R')
dat <- read.csv('economicData/indGrowth.csv', header = T, stringsAsFactors = F)

# 平稳性检验
library('tseries') #载入tseries包
ind_growth <- dat[,2]
time <- dat[,1][2:nrow(dat)]
ind_dif <- diff(ind_growth)

dat[,1] <- as.Date(dat[,1])

#############################################(可跳过)
# 原始数据
plot(ind_growth~dat[,1])     # 作图
adf.test(ind_growth)         # ADF检验
# 获取对数&差分数据
ind_ln <- log(ind_growth)
ind_dif <- diff(ind_growth)
# 差分
plot(ind_dif~dat[,1][2:nrow(dat)]) # 作图
adf.test(ind_dif)                  # ADF检验
# 对数
plot(ind_ln~dat[,1])  # 作图
adf.test(ind_ln)      # ADF检验

# 对数再差分
lnind_dif <- diff(ind_ln)
plot(ind_ln, type='l')

#par(mfrow=c(2,1))############################
df <- as.data.frame(cbind(time, lnind_dif),
                    stringsAsFactors = F)
df[,2] <- as.numeric(df[,2])


# 取93-17年的数据
x <- df[36:335, 2]
adf.test(x)
plot(x, type = 'l')
draw <- seq(from=3, to=length(x), by=3)
growth <- (x[draw-2]*0.33+x[draw-1]*0.33+x[draw])*0.33
adf.test(growth)
plot(growth, type = 'l')

# 合并
fit <- cbind(idx_house, growth)
#fit <- na.omit(fit)
adf.test(fit[,1])
adf.test(fit[,2])

# 对NA进行插补
fit[1,1] <- fit[2,1]
na <- which(is.na(fit[,1]))
fit[,1][na] <- (fit[,1][na+1]+fit[,1][na-1])/2 # 上下平均值插补NA

# 现在都能通过平稳性检验
write.csv(fit, 'univariate.csv', row.names = T)

