setwd('E:/github/respository/GenialFlow')

# 读入数据
# dat <- read.csv('univariate.csv')
# dat[,1] <- as.Date(as.character(dat[,1]))
# colnames(dat)[c(1,2)] <- c('time','index')

# 工业增加值(growth)在industryGrowthTest.R里得到
# idx通过getIndex.R里的函数得到
# 这个R文件从cbind()上述两列数据开始
uniModel <- function(filename, indgrowth){
  idx <- getIndex(filename)
  fit <- cbind(idx, indgrowth)
  # 对NA进行插补）
  fit <- fit[7:nrow(fit),] # 弃掉NA出现概率较高的前几年
  if(is.na(fit[1,1])) fit[1,1] <- fit[2,1]
  na <- which(is.na(fit[,1]))
  fit[,1][na] <- (fit[,1][na+1]+fit[,1][na-1])/2 # 上下平均值插补NA
  print('行业指标的平稳性检验结果:')
  print(adf.test(fit[,1]))
  colnames(fit) <- c('index', 'growth')
  return(fit)
}
df <- uniModel('电气设备净利润.csv', growth)

# 长期均衡关系
m1 <- lm(index~growth, data = df)
summary(m1)
# var(向量自回归)
library('vars')
infocri <- unlist(VARselect(df, lag.max=12,type="const")["selection"])
k <- min(infocri)
# Result : 选择滞后阶数k
# 拟合var模型
var1 <- VAR(df, p=k, type = 'const') 
# 格兰杰因果关系检验
library('lmtest')
index <- df[,1]
growth <- df[,2]
grangertest(index~growth, order = k)
grangertest(growth~index, order = k)



