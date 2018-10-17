# 读入数据
dat <- read.csv('indGrowth.csv', header = T, stringsAsFactors = F)

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

# 标准化&对数处理
fit <- as.data.frame(apply(fit, 2, scale))
center <- sweep(fit, 2, apply(fit, 2, min),'-') #在列的方向上减去最小值
R <- apply(fit, 2, max) - apply(fit,2,min) #算出极差，即列上的最大值-最小值
fit_star<- sweep(center, 2, R, "/") + 1 #把减去均值后的矩阵在列的方向上除以极差向量
lnfit <- as.data.frame(apply(fit_star, 2, log)) # 取对数

# 检验
adf.test(lnfit[,1])
adf.test(lnfit[,2])
plot(lnfit[,2])
