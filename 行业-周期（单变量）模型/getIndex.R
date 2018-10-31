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
  num <- na.fill(num, 0)
  finance_data = finance_data      # 指标是否为财务数据，若是则为 T
  if(finance_data){
  num <- gsub(',','',num)
  num <- apply(num, 2, as.numeric)
  num <- na.fill(num, 0)           # 填充由于数据转换造成的缺失
  }
  num <- apply(num, 2, as.numeric)
  # 这个地方开始写，对于累计数据和每期数据的处理。
  num[which(num==0)]<-NA           # 0数据转为NA，否则差值是错误的
  accumulated_data = T
  if(accumulated_data){
    loc <- seq(from=4, to=ncol(num),by=4)
    num[,loc] <- num[,loc] - num[,(loc-1)]
    num[,(loc-1)] <- num[,(loc-1)] - num[,(loc-2)]
    num[,(loc-2)] <- num[,(loc-2)] - num[,(loc-3)]
    }
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
  # 比如若从1997年开始，请输入idxYear = 1997
  idxYear <- 2002
  n <- (idxYear-1993)*4+1
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
plot1 <- getIndex('净利润/房地产净利润.csv', finance_data = T)
plot2 <- getIndex('主营业务利润/房地产主营业务利润.csv', finance_data = T)

ts.plot(scale(plot1), type='o', col = 'red') # 查看净利润
lines(scale(plot2), type='o', col = 'blue')  # 查看主营业务利润
legend('topleft', c('净利润', '主营业务利润'), col = c('red', 'blue'),
       lty = 1)
