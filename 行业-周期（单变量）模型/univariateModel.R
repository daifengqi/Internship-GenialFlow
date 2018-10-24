setwd('E:/workspace_qdf/R')
library('vars')   # 程序包：向量自回归
library('lmtest') # 程序包：时间序列检验

# README!
# 工业增加值(indgrowth)在industryGrowthTest.R里得到;
# idx通过getIndex.R里的函数得到;

# 这个R文件从cbind()上述两列数据开始
uniModel <- function(filename, economiccycle, finance_data){
  idx <- getIndex(filename, finance_data)
  fit <- cbind(idx, economiccycle)
  # 对NA进行插补
  startrow <- 1
  fit <- fit[startrow:nrow(fit),] # 弃掉NA出现概率较高的前几年
  if(is.na(fit[1,1])) fit[1,1] <- fit[2,1]
  na <- which(is.na(fit[,1]))
  fit[,1][na] <- (fit[,1][na+1]+fit[,1][na-1])/2 # 上下平均值插补NA
  dif = F # 是否对行业指标数据变换
  if(dif){
    fit[,1] <- log(fit[,1]-min(fit[,1])+1)
    fit[,1] <- c(0, diff(fit[,1]))
    fit <- fit[3:nrow(fit),]
  }
  colnames(fit) <- c('index', 'economic_cycle')
  return(fit)
}
dff <- uniModel('净利润/银行净利润.csv', ppidata)
# 对指标标准化
df <- as.data.frame(apply(dff, 2, scale))
# 查看两变量的走势图
ts.plot(df[,2], col = 'blue') # 蓝色是经济周期
lines(df[,1], col = 'red')    # 红色是行业表现
# 协整关系检验
m1 <- lm(index~economic_cycle, data = df)
dwtest(m1)
# 长期均衡关系
summary(m1)
# var(向量自回归)
infocri <- unlist(VARselect(df, lag.max=12,type="const")["selection"])
k <- min(infocri)
k # 查看滞后期数

# 格兰杰因果关系检验
index <- df[,1]
economic_cycle <- df[,2]
grangertest(economic_cycle~index, order = k)
grangertest(index~economic_cycle, order = k)




