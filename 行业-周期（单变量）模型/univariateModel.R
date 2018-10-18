setwd('E:/github/respository/GenialFlow')
library('vars')   # 程序包：向量自回归
library('lmtest') # 程序包：时间序列检验

# README!
# 工业增加值(indgrowth)在industryGrowthTest.R里得到;
# idx通过getIndex.R里的函数得到;

# 这个R文件从cbind()上述两列数据开始
uniModel <- function(filename, indgrowth){
  idx <- getIndex(filename)
  fit <- cbind(idx, indgrowth)
  # 对NA进行插补）
  fit <- fit[5:nrow(fit),] # 弃掉NA出现概率较高的前几年
  if(is.na(fit[1,1])) fit[1,1] <- fit[2,1]
  na <- which(is.na(fit[,1]))
  fit[,1][na] <- (fit[,1][na+1]+fit[,1][na-1])/2 # 上下平均值插补NA
  dif = F # 是否对指标数据进行处理
  if(dif){
    fit[,1] <- log(fit[,1]-min(fit[,1])+1)
    fit[,1] <- c(0, diff(fit[,1]))
    fit <- fit[3:nrow(fit),]
  }
  print('行业指标的平稳性检验结果:')
  print(adf.test(fit[,1]))
  colnames(fit) <- c('index', 'growth')
  return(fit)
}
dff <- uniModel('主营业务利润/有色金属主营业务利润.csv', indgrowth)
# 对指标标准化
df <- as.data.frame(apply(dff, 2, scale))
# 长期均衡关系
m1 <- lm(index~growth, data = df)
summary(m1)
# var(向量自回归)
infocri <- unlist(VARselect(df, lag.max=12,type="const")["selection"])
k <- min(infocri)
k # 查看滞后期数

# 格兰杰因果关系检验
index <- df[,1]
growth <- df[,2]
grangertest(growth~index, order = k)
grangertest(index~growth, order = k)


