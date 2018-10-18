setwd('E:/workspace_qdf/R')

# 读取宏观经济数据
eco <- read.csv('economicData/economic_data.csv', header = T)
eco <- eco[-1,]
row.names(eco) <- 1:nrow(eco)
eco <- eco[1:(nrow(eco)-4),] # 选择开始年份
eco[,1] <- paste(eco[,1],'-01',sep='')
eco[,1] <- as.Date(eco[,1])
# 获取工业增加值(growth)
growth <- as.numeric(levels(eco[,3])[eco[,3]])
draw <- seq(from=3, to=nrow(eco),by=3)
growth <- round((growth[draw-2]+growth[draw-1]+growth[draw])/3,4)
growth <- as.data.frame(growth, row.names = 
                          as.character(
                            seq.Date(
                              from = as.Date(eco[1,1]),
                              to = as.Date(eco[nrow(eco),1]),
                              by = 'quarter')))

# 获取行业指标
house <- read.csv('AshareIndustry/房地产净利润.csv', header = T)
########################################
# idxMedian函数
# 功能：获取行业指标（中位数）
# 输入：行业数据框（行：所有企业 列：开始时间-结束时间 元素：指标的值）
# 输出：行名称为时间点，一列元素为指标中位数的dataframe
########################################
idxMedian <- function(df, time_start, time_end){
  num <- df[,4:ncol(df)]
  for(i in 1:ncol(num)) num[,i] <- as.numeric(num[,i])
  # 提取指标
  idx <- c()
  for(i in 1:ncol(num)){
    m <- num[,i]
    m[which(m==0)]<-NA
    mid <- median(m, na.rm = T)
    idx <- c(idx, mid)
  }
  idx <- as.numeric(idx)
  
  rlt <- as.data.frame(idx, 
                       row.names = 
                         as.character(
                           seq.Date(
                             from = as.Date(time_start),
                             to = as.Date(time_end),
                             by = 'quarter')))
  return(rlt)
}
########################################
# 获取房地产数据
idx_house <- idxMedian(df = house,
                       time_start = "1993/3/1",
                       time_end = "2017/12/1")

# 整合到一个数据框
fit <- cbind(idx_house, growth[13:(nrow(growth)-2),])
colnames(fit) <- c('index', 'growth')
fit <- na.omit(fit)

# 标准化&对数处理
fit <- as.data.frame(apply(fit, 2, scale))
center <- sweep(fit, 2, apply(fit, 2, min),'-') #在列的方向上减去最小值
R <- apply(fit, 2, max) - apply(fit,2,min) #算出极差，即列上的最大值-最小值
fit_star<- sweep(center, 2, R, "/") + 1 #把减去均值后的矩阵在列的方向上除以极差向量
lnfit <- as.data.frame(apply(fit_star, 2, log)) # 取对数

# 最后，我们得到最终代入模型的数据框:lnfit

# Step1 :平稳性检验
library('tseries') #载入tseries包
index <- lnfit[,1]
growth <- lnfit[,2]
plot.ts(growth)
adf.test(index)
adf.test(growth) # 工业增加值显然不会平稳

# Step2:回归分析-相关关系-长期均衡关系
m1 <- lm(index~growth, data = lnfit)
summary(m1)
# 同阶单整：随机误差项是平稳序列
adf.test(m1$residuals) # 在10%的显著水平下通过

# 是否协整？

# Step3:格兰杰因果关系检验
library('lmtest')
grangertest(index~growth, order = 1)
grangertest(growth~index, order = 1)


dwtest(m1)

