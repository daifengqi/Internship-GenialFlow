setwd('E:/github/respository/GenialFlow')

# 获取行业指标
# house <- read.csv('AshareIndustry/房地产净利润.csv', header = T)
########################################
# idxMedian函数
# 功能：获取行业指标（中位数）
# 输入：行业数据框（行：所有企业 列：开始时间-结束时间 元素：指标的值）
# 输出：行名称为时间点，一列元素为指标中位数的dataframe
########################################
idxMedian <- function(df, time_start, time_end){
  num <- df[,4:ncol(df)]
  for(i in 1:ncol(num)) num[,i] <- as.numeric(num[,i])
  num <- na.fill(num, 0)
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

getIndex <- function(filename){
  path <- paste('AshareIndustry/', filename, sep = '')
  profit <- read.csv(path, header = T)
  idx <- idxMedian(df = profit,
                   time_start = "1993/3/1",
                   time_end = "2017/12/1")
  return(idx)
}

# 函数getIndex
# 通过输入csv文件名一键获得时间序列数据
aa <- getIndex('ROE/钢铁ROE.csv')
diff(aa[,1])
adf.test(a)
