setwd('D:\\R/GenialFlow')
#setwd('E:\\workspace_qdf/R')

cycleRobust <- function(dfCycle, y, dif = FALSE){
  fit <- cbind(dfCycle, y[row.names(dfCycle),])
  colnames(fit)[8] <- 'idx'
  fit[,8] <- scale(fit[,8])
  if(dif){
    m1 <- lm(idx[2:nrow(fit)]~state[1:(nrow(fit)-1)], data = fit)
  }
  m1 <- lm(idx~state, data = fit)
  summary(m1) # 函数没有返回，作用是查看回归结果
}

# 试用
cycleRobust(dfCycle = gT1k0,
            y = idx_house)

