setwd('E:/workspace_qdf/R')
library('zoo') # 程序包：数据预处理

# 获取行业数据
# house <- read.csv('AshareIndustry/净利润/房地产净利润.csv', header = T)
########################################
# idxMedian函数
# 功能：获取行业指标（中位数）
########################################
idxMedian <- function(df, time_start, time_end, finance_data){
  num <- df[,4:ncol(df)]
  num <- na.fill(num, 0)           # 填充由于数据转换造成的缺失
  finance_data = finance_data      # 指标是否为财务数据，若是则为 T
  if(finance_data){
  num <- gsub(',','',num)
  num <- apply(num, 2, as.numeric)
  num <- na.fill(num, 0)           # 填充由于数据转换造成的缺失
  }
  num <- apply(num, 2, as.numeric)
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
  # 默认是从1993年第一季度开始，需要调整开始时间请修改
  # 比如若从1997年开始，请输入n=(1997-1993)*4+1=17
  n <- 17
  rlt <- as.data.frame(rlt[n:nrow(rlt),])
  colnames(rlt)[1] <- 'idx'
  return(rlt)
}
########################################

getIndex <- function(filename, finance_data){
  path <- paste('AshareIndustry/', filename, sep = '')
  profit <- read.csv(path, header = T, stringsAsFactors = F)
  idx <- idxMedian(df = profit,
                   time_start = "1993/3/1",
                   time_end = "2017/12/1",
                   finance_data = finance_data)
  return(idx)
}

# 函数getIndex
# 通过输入csv文件名一键获得时间序列数据
# remove(pl)
cw <- getIndex('净利润/化工净利润.csv', finance_data = T)
notcw <- getIndex('ROE/化工ROE.csv', finance_data = F)

ts.plot(pl[,1]) # 查看时间序列图

