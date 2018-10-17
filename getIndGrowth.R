#setwd('E:/workspace_qdf/R')
setwd('D:/R/GenialFlow')


# 读取工业增加值估计值
rlind <- read.csv('industryGrowth94-13.csv', header = F,
                  stringsAsFactors = F)
rlind[,1] <- as.Date(rlind[,1])

# 读取工业增加值增速
eco <- read.csv('economicData/economic_data.csv', header = T,
                stringsAsFactors = F)
eco <- eco[-1,]
eco[,1] <- paste(eco[,1],'-01',sep='')
eco[,1] <- as.Date(eco[,1])
eco_ind <- eco[,c(1,3)]

# 合并增速与增值估计值
colnames(eco_ind)[1] <- 'V1'


# 检验（已通过，可跳过此部分）
# ##############################
dat <- merge(rlind, eco_ind, by='V1',
             all = F) # 先合并已有数据用于检验
colnames(dat) <- c('time', 'growth', 'growth_speed')
time <- dat[,1]
growth <- dat[,2]
growth_speed <- dat[,3]
speed_real <- growth[13:length(growth)]/growth[1:(length(growth)-12)]
speed_real <- (speed_real -1)*100
# 作图检验估计值的准确性
plot(speed_real~time[13:length(time)], type='l', col = 'red')
lines(growth_speed[13:length(growth_speed)]~time[13:length(time)], type='l', col = 'blue')
# 基本重合， 故可以使用
# ##############################


dat <- merge(rlind, eco_ind, by='V1',
             all = T) # 使用全部数据
dat <- dat[-c(nrow(dat), nrow(dat)-1),]
# 填充数据
for(i in 4:1){
  r1 <- 12*i+1
  r2 <- 12*(i+1)
  gro_spd <- dat[,3][r1:r2]
  gro_spd <- as.numeric(gro_spd)/100
  gro <- dat[,2][r1:r2]
  gro_new <- gro/(1+gro_spd)
  r1 <- r1 - 12
  r2 <- r2 - 12
  dat[r1:r2,2] <- gro_new
}
for(i in 23:27){
  r1 <- 12*i+1
  r2 <- 12*(i+1)
  r3 <- r1 + 12
  r4 <- r2 + 12
  gro_spd <- dat[,3][r3:r4]
  gro_spd <- as.numeric(gro_spd)/100
  gro <- dat[,2][r1:r2]
  gro_new <- gro*(1+gro_spd)
  dat[r3:r4,2] <- gro_new
}
dat <- dat[1:344,]
colnames(dat) <- c('time', 'ind_growth', 'growth_speed')
write.csv(dat, 'indGrowth.csv', row.names = F)
