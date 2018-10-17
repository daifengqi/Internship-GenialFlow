setwd('E:/workspace_qdf/R')

# 读入数据
dat <- read.csv('univariate.csv')
dat[,1] <- as.Date(as.character(dat[,1]))
colnames(dat)[c(1,2)] <- c('time','index')


# 长期均衡关系
m1 <- lm(index~growth, data = dat)
summary(m1)
# 对参差进行平稳性检验
err <- m1$residuals
adf.test(err) 
# 通过检验，故传统回归的结论可以使用
plot(err, type = 'l')

# 格兰杰因果关系检验
library('lmtest')
index <- dat[,2]
growth <- dat[,3]
grangertest(index~growth, order = 1)
grangertest(growth~index, order = 1)

# var(向量自回归)
library('vars')
VARselect(index, lag.max=8,type="const")["selection"]
# Result :一阶就行
# 拟合var模型
var1 <- VAR(dat[,c(2,3)], p=1, type = 'const') 
# 系统平稳性
var1_stabil <- stability(var1, type = 'OLS-MOSUM',
          h = 0.15, dynamic = FALSE,
          rescale = TRUE)
plot(var1_stabil) # 模型通过了平稳性检验




