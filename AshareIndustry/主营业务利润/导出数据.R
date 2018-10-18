setwd('E:/github/respository/GenialFlow/AshareIndustry')
hangye <- read.csv('上市公司行业.csv', header = T, stringsAsFactors = F)
roe <- read.csv('主营业务利润/主营业务利润.csv', header = T, stringsAsFactors = F)
dat <- merge(roe, hangye, by.x='证券代码',
            by.y='上市公司')
dat[,2]<-dat[,103]
dat <- dat[,1:102]
colnames(dat)[2] <- '申万一级行业'


setwd('E:/github/respository/GenialFlow/AshareIndustry/主营业务利润')
write.csv(dat[which(dat[,2] == '采掘'),], file = '采掘主营业务利润.csv')
write.csv(dat[which(dat[,2] == '传媒'),], file = '传媒主营业务利润.csv')
write.csv(dat[which(dat[,2] == '电气设备'),], file = '电气设备主营业务利润.csv')
write.csv(dat[which(dat[,2] == '电子'),], file = '电子主营业务利润.csv')
write.csv(dat[which(dat[,2] == '房地产'),], file = '房地产主营业务利润.csv')
write.csv(dat[which(dat[,2] == '纺织服装'),], file = '纺织服装主营业务利润.csv')
write.csv(dat[which(dat[,2] == '非银金融'),], file = '非银金融主营业务利润.csv')
write.csv(dat[which(dat[,2] == '钢铁'),], file = '钢铁主营业务利润.csv')
write.csv(dat[which(dat[,2] == '公用事业'),], file = '公用事业主营业务利润.csv')
write.csv(dat[which(dat[,2] == '国防军工'),], file = '国防军工主营业务利润.csv')
write.csv(dat[which(dat[,2] == '化工'),], file = '化工主营业务利润.csv')
write.csv(dat[which(dat[,2] == '机械设备'),], file = '机械设备主营业务利润.csv')
write.csv(dat[which(dat[,2] == '计算机'),], file = '计算机主营业务利润.csv')
write.csv(dat[which(dat[,2] == '家用电器'),], file = '家用电器主营业务利润.csv')
write.csv(dat[which(dat[,2] == '建筑材料'),], file = '建筑材料主营业务利润.csv')
write.csv(dat[which(dat[,2] == '建筑装饰'),], file = '建筑装饰主营业务利润.csv')
write.csv(dat[which(dat[,2] == '交通运输'),], file = '交通运输主营业务利润.csv')
write.csv(dat[which(dat[,2] == '农林牧渔'),], file = '农林牧渔主营业务利润.csv')
write.csv(dat[which(dat[,2] == '汽车'),], file = '汽车主营业务利润.csv')
write.csv(dat[which(dat[,2] == '轻工制造'),], file = '轻工制造主营业务利润.csv')
write.csv(dat[which(dat[,2] == '商业贸易'),], file = '商业贸易主营业务利润.csv')
write.csv(dat[which(dat[,2] == '食品饮料'),], file = '食品饮料主营业务利润.csv')
write.csv(dat[which(dat[,2] == '通信'),], file = '通信主营业务利润.csv')
write.csv(dat[which(dat[,2] == '休闲服务'),], file = '休闲服务主营业务利润.csv')
write.csv(dat[which(dat[,2] == '医药生物'),], file = '医药生物主营业务利润.csv')
write.csv(dat[which(dat[,2] == '银行'),], file = '银行主营业务利润.csv')
write.csv(dat[which(dat[,2] == '有色金属'),], file = '有色金属主营业务利润.csv')
write.csv(dat[which(dat[,2] == '综合'),], file = '综合主营业务利润.csv')



