setwd('D:\\R/GenialFlow')
#setwd('E:\\workspace_qdf/R')
library('zoo')  # 程序包：时间序列相关

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

# Step 1：对月度数据取平均得到季度数据
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


##########################################
# 上：获得两列数据（经济增长&通货膨胀）指标
# 下：代入指标计算经济周期
##########################################


# Step 2：移动平均+描述观察
k = 3 # 滑动宽度
zoo1 <- zoo(scale_dat[,1], tseries) # 平减指数
rm1 <- rollmean(zoo1,k)
zoo2 <- zoo(scale_dat[,3], tseries) # 工业增加值
rm2 <- rollmean(zoo2,k)
par(mfrow=c(2,1))
plot(rm1, xlab = '时间', ylab = '平减指数')
plot(rm2, xlab = '时间', ylab = '工业增加值')

# 取移动平均后的数据框
dat1 <- cbind(as.data.frame(rm1),as.vector(rm2))
colnames(dat1) <- c('flation_平减指数','eco_工业增加值')

# 经济周期计算期限
lagT = 2 # 跨期取差分
tseries_rm <- row.names(dat1)
tseries_diff <- tseries_rm[(lagT+1):length(tseries_rm)]
# 经济增长（减缓）期
eco_up <- tseries_diff[which(diff(dat1[,2], lag = lagT)>=0)]
eco_down <- tseries_diff[which(diff(dat1[,2], lag = lagT)<0)]
# 通货膨胀（通缩）期
inflation <- tseries_diff[which(diff(dat1[,1], lag = lagT)>=0)]
deflation <- tseries_diff[which(diff(dat1[,1], lag = lagT)<0)]

# 美林投资时钟(ex:经济增长+通胀=过热)
overheated <- intersect(eco_up,inflation)    # 过热
stagflation <- intersect(eco_down,inflation) # 滞涨
decline <- intersect(eco_down,deflation)     # 衰退
recovery <- intersect(eco_up,deflation)      # 复苏

# 经济周期加入数据框
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
write.csv(dat2,'ecoCycle.csv')
# 从15年开始，工业增加值就基本稳定了
# 可能还要加入别的指标来调试一下



