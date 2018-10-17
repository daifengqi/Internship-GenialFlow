setwd('E:\\workspace_qdf/R/AshareIndustry')

dat <- read.csv('AshareIndustryAll.csv', header = T, stringsAsFactors = F)
# unique(as.factor(dat$申万一级行业)) 28个分类
summary(as.factor(dat$申万一级行业))
# 采掘     传媒 电气设备     电子   房地产 纺织服装 非银金融     钢铁 公用事业 国防军工     化工 
# 61      150      191      225      130       87       67       32      154       52      322 
# 机械设备   计算机 家用电器 建筑材料 建筑装饰 交通运输 农林牧渔     汽车 轻工制造 商业贸易 食品饮料 
# 319      202       65       72      125      110       92      169      124       97       89 
# 通信 休闲服务 医药生物     银行 有色金属     综合 
# 106       35      285       28      118       44 

write.csv(dat[which(dat[,2] == '采掘'),], file = '采掘净利润.csv')
write.csv(dat[which(dat[,2] == '传媒'),], file = '传媒净利润.csv')
write.csv(dat[which(dat[,2] == '电气设备'),], file = '电气设备净利润.csv')
write.csv(dat[which(dat[,2] == '电子'),], file = '电子净利润.csv')
write.csv(dat[which(dat[,2] == '房地产'),], file = '房地产净利润.csv')
write.csv(dat[which(dat[,2] == '纺织服装'),], file = '纺织服装净利润.csv')
write.csv(dat[which(dat[,2] == '非银金融'),], file = '非银金融净利润.csv')
write.csv(dat[which(dat[,2] == '钢铁'),], file = '钢铁净利润.csv')
write.csv(dat[which(dat[,2] == '公用事业'),], file = '公用事业净利润.csv')
write.csv(dat[which(dat[,2] == '国防军工'),], file = '国防军工净利润.csv')
write.csv(dat[which(dat[,2] == '化工'),], file = '化工净利润.csv')
write.csv(dat[which(dat[,2] == '机械设备'),], file = '机械设备净利润.csv')
write.csv(dat[which(dat[,2] == '计算机'),], file = '计算机净利润.csv')
write.csv(dat[which(dat[,2] == '家用电器'),], file = '家用电器净利润.csv')
write.csv(dat[which(dat[,2] == '建筑材料'),], file = '建筑材料净利润.csv')
write.csv(dat[which(dat[,2] == '建筑装饰'),], file = '建筑装饰净利润.csv')
write.csv(dat[which(dat[,2] == '交通运输'),], file = '交通运输净利润.csv')
write.csv(dat[which(dat[,2] == '农林牧渔'),], file = '农林牧渔净利润.csv')
write.csv(dat[which(dat[,2] == '汽车'),], file = '汽车净利润.csv')
write.csv(dat[which(dat[,2] == '轻工制造'),], file = '轻工制造净利润.csv')
write.csv(dat[which(dat[,2] == '商业贸易'),], file = '商业贸易净利润.csv')
write.csv(dat[which(dat[,2] == '食品饮料'),], file = '食品饮料净利润.csv')
write.csv(dat[which(dat[,2] == '通信'),], file = '通信净利润.csv')
write.csv(dat[which(dat[,2] == '休闲服务'),], file = '休闲服务净利润.csv')
write.csv(dat[which(dat[,2] == '医药生物'),], file = '医药生物净利润.csv')
write.csv(dat[which(dat[,2] == '银行'),], file = '银行净利润.csv')
write.csv(dat[which(dat[,2] == '有色金属'),], file = '有色金属净利润.csv')
write.csv(dat[which(dat[,2] == '综合'),], file = '综合净利润.csv')
