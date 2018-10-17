setwd('D:\\R/GenialFlow')
#setwd('E:\\workspace_qdf/R')
library('zoo')  # 程序包：时间序列
library('nnet') # 程序包：编码

# 读取数据+调整
eco <- read.csv('ECO.csv', header = T)
eco <- eco[-1,]
row.names(eco) <- 1:nrow(eco)
eco <- eco[82:(nrow(eco)-4),]
eco[,1] <- paste(eco[,1],'-01',sep='')
eco[,1] <- as.Date(eco[,1])

# 获取时序+指标数值化
tseries <- eco[,1]
idx <- eco[,c(2,3,4,8)]
idx <- as.data.frame(apply(idx, 2, as.numeric))

# 对月度数据取平均得到季度数据
draw <- seq(from=3, to=nrow(idx),by=3)
quaterData <- idx[draw,4]
tseries <- tseries[draw]
monthData <- as.data.frame(cbind(round((idx[draw-2,1]+idx[draw-1,1]+idx[draw,1])/3,4),
                                 round((idx[draw-2,2]+idx[draw-1,2]+idx[draw,2])/3,4),
                                 round((idx[draw-2,3]+idx[draw-1,3]+idx[draw,3])/3,4)))

# 获得数据的初步形式
dat <- as.data.frame(cbind(quaterData,monthData),
                     stringsAsFactors = F)
row.names(dat) <- tseries
colnames(dat) <- c('GDP平减指数','PPI', '工业增加值','CPI')

# 标准化
scale_dat <- as.data.frame(apply(dat, 2, scale))

########################################
# Cycle函数
# 功能：获取经济周期
# 输入：经济增长指标，通货膨胀指标
# 输出：经济周期（含哑变量）的dataframe
########################################
Cycle <- function(var_eco, var_inf, k, lagT,
                  ifplot = FALSE){
  # 移动平均+描述观察
  k = k # 窗宽
  zoo1 <- zoo(var_inf, tseries) # 通货膨胀指标
  zoo2 <- zoo(var_eco, tseries) # 经济增长指标
  if(k==0) dat1 <- cbind(as.data.frame(zoo1),as.vector(zoo2))
  else{
    rm1 <- rollmean(zoo1,k)
    rm2 <- rollmean(zoo2,k)
    if(ifplot){  # 是否画图
      par(mfrow=c(2,1))
      plot(rm1, xlab = '时间', ylab = '通货膨胀')
      plot(rm2, xlab = '时间', ylab = '经济增长')
    }
    dat1 <- cbind(as.data.frame(rm1),as.vector(rm2))
  }
  colnames(dat1) <- c('通货膨胀','经济增长')
  # 经济周期
  lagT = lagT # 差分期数
  tseries_rm <- row.names(dat1)
  tseries_diff <- tseries_rm[(lagT+1):length(tseries_rm)]
  eco_up <- tseries_diff[which(diff(dat1[,2], lag = lagT)>=0)] # 经济增长（减缓）期
  eco_down <- tseries_diff[which(diff(dat1[,2], lag = lagT)<0)]
  inflation <- tseries_diff[which(diff(dat1[,1], lag = lagT)>=0)] # 通货膨胀（通缩）期
  deflation <- tseries_diff[which(diff(dat1[,1], lag = lagT)<0)]
  overheated <- intersect(eco_up,inflation)    # 过热（美林投资时钟）
  stagflation <- intersect(eco_down,inflation) # 滞涨
  decline <- intersect(eco_down,deflation)     # 衰退
  recovery <- intersect(eco_up,deflation)      # 复苏
  state <- c()
  # 1过热 2滞涨 3衰退 4复苏
  for(i in 1:length(tseries_diff)){
    if(tseries_diff[i] %in% overheated) state <- c(state,"过热")
    else if(tseries_diff[i] %in% stagflation) state <- c(state,"滞涨")
    else if(tseries_diff[i] %in% decline) state <- c(state,"衰退")
    else if(tseries_diff[i] %in% recovery) state <- c(state,"复苏")
    else state <- c(state, NA)
  }
  dat2 <- cbind(dat1[(lagT+1):nrow(dat1),],state)
  # 分类变量编码
  state_dummy <- as.data.frame(class.ind(dat2[,3]))
  dat3 <- cbind(dat2[,c(1,2,3)], state_dummy)
  return(dat3)
}
########################################
########################################

# 试用
# 工业增加值->经济增长，GDP平减->通货膨胀
# 具体指标查看scale_dat的列名称
gT1k0 <- Cycle(var_eco = scale_dat[,3], # 工业增加值
               var_inf = scale_dat[,1], # GDP平减
               lagT = 1,
               k = 0)


# 使用PPI代表通胀
pT1k0 <- Cycle(var_eco = scale_dat[,3],# 工业增加值
               var_inf = scale_dat[,2], # PPI
               lagT = 1,
               k = 0)

