setwd('D:\\R/GenialFlow')
#setwd('E:\\workspace_qdf/R')
library('lmtest') # 程序包：时间序列检验
library('tseries')#载入tseries包

cycleRobust <- function(dfCycle, y, dif = FALSE){
  fit <- cbind(dfCycle, y[row.names(dfCycle),])
  colnames(fit)[8] <- 'idx'
  fit[,8] <- scale(fit[,8])
  if(dif){
    m1 <- lm(idx[2:nrow(fit)]~state[1:(nrow(fit)-1)], data = fit)
  }
  m1 <- lm(idx~state, data = fit)
  return(as.data.frame(fit))
}

dat <- cycleRobust(gT1k0, idx_steel)
dat <- dat[,c(3, 8)]
ts <- dat[,2]
adf.test(na.omit(ts)) # 平稳性检验

grangertest(dat$idx, dat$state, order = 1)
grangertest(dat$state, dat$idx, order = 3)

