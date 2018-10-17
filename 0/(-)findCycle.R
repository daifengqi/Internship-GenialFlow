# setwd('D:\\R/GenialFlow')
setwd('E:\\workspace_qdf/R')

# 读取GDP/CPI数据
gc <- read.csv('GDPCPI.csv', skip=1, stringsAsFactors = F)
gc <- gc[1:(nrow(gc)-2),]
gc <- gc[25:nrow(gc),]

gc <- gc[1:(nrow(gc)-2),]
draw <- seq(from=3, to=nrow(gc),by=3)

# 对月度CPI比值取平均得到季度数据
quaterGDP <- gc[draw,2]
quaterCPI <- round((gc[draw-2,3]+gc[draw-1,3]+gc[draw,3])/3,2)


dat <- as.data.frame(cbind(gc[draw,1],quaterGDP,quaterCPI),
                     stringsAsFactors = F)
# 转为数值型
dat[,2] <- unlist(lapply(dat[,2], as.numeric))
dat[,3] <- unlist(lapply(dat[,3], as.numeric))

# GDP/CPI变化率曲线
par(mfrow=c(2,1))
if(TRUE){
  plot(dat[,3],type='l',col='red',xlab='时间',ylab='指标')
  lines(dat[,2],type='l',col='blue')
  legend('topright',c('CPI','GDP'),col=c('red','blue'),lty=c(1,1))
}
# 移动平均
# 第一列转化为时间数据
library(zoo)
dat[,1] <- paste(dat[,1],'-01',sep='')
dat[,1] <- as.Date(dat[,1])


getCycle <- function(k,dataIn){
# GDP移动平均
k = k # 滑动宽度
zooGDP <- zoo(dat[,2], dat[,1])
rmGDP <- rollmean(zooGDP,k)
zooCPI <- zoo(dat[,3], dat[,1])
rmCPI <- rollmean(zooCPI,k)
# 获取GDP,CPI变化率的数据框
dat1 <- cbind(as.data.frame(rmGDP),as.vector(rmCPI))
colnames(dat1) <- c('GDP_change','CPI_change')
# GDP/CPI移动平均之后的曲线
if(FALSE){
  plot(dat1[,2],type='l',col='red',xlab='时间',ylab='指标')
  lines(dat1[,1],type='l',col='blue')
  legend('topright',c('CPI','GDP'),col=c('red','blue'),lty=c(1,1))
}

tseries <- row.names(dat1)
tseries <- tseries[2:length(tseries)]
# 经济增长（减缓）期
eco_up <- tseries[which(diff(dat1[,1])>=0)]
eco_down <- tseries[which(diff(dat1[,1])<0)]
# 通货膨胀（通缩）期
inflation <- tseries[which(diff(dat1[,2])>=0)]
deflation <- tseries[which(diff(dat1[,2])<0)]
# 美林投资时钟
overheated <- intersect(eco_up,inflation)    # 过热
stagflation <- intersect(eco_down,inflation) # 滞涨
decline <- intersect(eco_down,deflation)     # 衰退
recovery <- intersect(eco_up,deflation)      # 复苏

tseries <- row.names(dat1)
tseries <- tseries[2:length(tseries)]
state <- c()
# 1过热 2滞涨 3衰退 4复苏
for(i in 1:length(tseries)){
  if(tseries[i] %in% overheated) state <- c(state,"过热")
  else if(tseries[i] %in% stagflation) state <- c(state,"滞涨")
  else if(tseries[i] %in% decline) state <- c(state,"衰退")
  else if(tseries[i] %in% recovery) state <- c(state,"复苏")
  else state <- c(state, NA)
}
dat2 <- cbind(dat1[2:nrow(dat1),],state)
return(dat2)
}

c1 <- getCycle(k = 1, dataIn = dat)
c3 <- getCycle(k = 3, dataIn = dat)
c5 <- getCycle(k = 5, dataIn = dat)

