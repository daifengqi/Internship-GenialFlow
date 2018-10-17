setwd('E:\\workspace_qdf/R')
library(readxl)    # 程序包：读取数据
library(lubridate) # 程序包：时间数据处理
library(stringr)   # 程序包：字符串处理

lst <- read_xlsx('list.xlsx')
dat <- read_xlsx('debt.xlsx')
dat <- dat[-c(nrow(dat),nrow(dat)-1),] # 删除最后两列（说明列：数据来源Wind）

matchIndex<-function(dat,id1,lst,id2,col,needNA=T){
# -----------------------------：
# 从lst中匹配每个债券的相关指标（如违约时间），并在dat中添加该列。
# 参数说明：
# dat:原数据 id1:对应ID lst:匹配目标 id2:对应ID col:匹配目标列
# needNA:若为真，则在没有匹配到目标数据的情况下添加缺失，否则直接删除该观测。
  idx <- c()                                    # 初始化指标列
  for(i in 1:nrow(dat)){                        # 对dat循环
    idx_initial <- idx                          # 标记指标
    for(j in 1:nrow(lst)){                      # 对lst循环
      if(sum(grep(dat[i,id1],lst[j,id2]))>=1){  # 匹配债券代码
        idx <- c(idx,t(lst[j,col])[1])          # 匹配成功：获取对应代码的违约时间
        break                                   # 已获取，跳出循环
      }
      else next
    }
    if(length(idx_initial) == length(idx)){   # 如果lst中没有对应的指标
      idx <- c(idx,NA)                        # 匹配失败：添加缺失值NA
    }
  }
  if(needNA) return(cbind(idx,dat,stringsAsFactors=F)) # 将“指标(idx)”列添加到dat
  else{                                                # needNA=FALSE（不需要缺失值）
    dat <- cbind(idx,dat,stringsAsFactors=F)           
    dat <- dat[-which(is.na(idx)),]                    # 删除未匹配到数据的行
    return(dat)
  }
}


time2node<-function(x){    
  # --------------------
  # 构建 f:违约时间->顺序 的映射准备
  return((x-2015+1)*4) # example: 2015 —> 8, 2016 -> 12
}

# 获取违约时间
lst[,3]<-unlist(lapply(lst[,3],toupper))        # 将债券代码都转换为大写，以对应dat的代码
dat1 <- matchIndex(dat,1,lst,3,5,needNA = F)    # lst中第5列是违约时间

# 取序列数据
time_series <- dat1[,25:ncol(dat1)]             # 选取时间序列的指标
dat2 <- cbind(dat1[,2], dat1[,1], 
              time_series,stringsAsFactors=F)   # 只保留代码、违约时间、时序指标
colnames(dat2)[1:2] <- c('债券代码','违约时间') # 修改列名

# 重构该数据，使之成为可以建模的数据

# Step1:时间顺序排序（债券）
# ----------------------------------------------------------------
# 说明：将2014年-2018年（前半年）的数据放在1——18（季度）个节点上，
#       根据债券的违约时间获得其前一年（4个季度）的数据，存为指标。
# ----------------------------------------------------------------
# 获得债券的违约年份和季度、节点
dat3 <- dat2[year_>=2015,]    # 只要15年之后的数据
ts<-as.POSIXlt(dat3$违约时间) # 转换为时间数据
year_ <- year(ts)             # 提取年份
quarter_ <- quarter(ts)       # 提取季度
order_ <- unlist(lapply(year_,time2node)) + quarter_ - 1 # 得到违约时间所在顺序系节点


# Step2:时间顺序排序（指标）
time_data <- dat3[,3:ncol(dat3)]                 # 获得序列数据
time_data_order <- rep(1:18, ncol(time_data)/18) # 产生序列节点
for(i in order_){                                # 对每个观测的排序节点(order_)进行循环
  r = 1                                          # 标记所在行数
  loc <- which(time_data_order==i)               # 获得对应指标列位置
  values <- time_data[r,(loc[1]-3):loc[1]]       # 提取对应指标列
  for(l in loc[2:length(loc)]) values <- cbind(values,time_data[r,(l-3):l])
  # 先对指标改名，改名之后才能rbind
  # 未完成
  # 待续
  # Continue
  r = r + 1
}






