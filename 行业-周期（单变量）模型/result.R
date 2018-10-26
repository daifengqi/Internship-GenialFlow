setwd('E:/workspace_qdf/R')
library('vars')   # 程序包：向量自回归
library('lmtest') # 程序包：时间序列检验
# 行业分类
cls <- c('采掘','传媒','电气设备','电子','房地产','纺织服装',
         '非银金融','钢铁','公用事业','国防军工','化工',
         '机械设备','计算机','家用电器','建筑材料','建筑装饰',
         '交通运输','农林牧渔','汽车','轻工制造','商业贸易',
         '食品饮料','通信','休闲服务','医药生物','银行',
         '有色金属','综合')
# 只用修改char就可以得到csv文件名合集
# char <- '主营业务利润'

# ###################################
# 函数：一键得到结果
# ###################################
onekeyrlt <- function(char, economiccycle, finance_data){
csvfile <- paste(char,'/',cls,char,'.csv', sep = '')
obv <- c()
for(i in 1:length(csvfile)){
# 对行业循环
dff <- uniModel(csvfile[i], economiccycle, finance_data) # 得到建模数据
df <- as.data.frame(apply(dff, 2, scale)) # 对指标标准化
# 回归（当期关系）
m1 <- lm(index~economic_cycle, data = df)
sum1 <- summary(m1)
lm_coef <- sum1$coefficients[2,1]         # 回归系数
lm_pvalue <- sum1$coefficients[2,4]       # 回归系数p值
lm_r2 <- sum1$r.squared                   # 解释度
# VARselect滞后期数
infocri <- unlist(VARselect(df, lag.max=12,type="const")["selection"])
k <- min(infocri)                         # 滞后期数
# 格兰杰因果关系检验
index <- df[,1]
economic_cycle <- df[,2]
g1 <- grangertest(economic_cycle~index, order = k)
g2 <- grangertest(index~economic_cycle, order = k)
g1_pvalue <- g1$`Pr(>F)`[2]               # 行业引起周期变化p值
g2_pvalue <- g2$`Pr(>F)`[2]               # 周期引起行业变化p值
# 得到观测（单个行业的情况）
obv <- c(obv, cls[i], lm_coef, lm_pvalue, lm_r2, k,
         g1_pvalue, g2_pvalue)
}
final_rlt <- matrix(obv, nrow = 7)
final_rlt <- t(final_rlt)
row.names(final_rlt) <- 1:28
colnames(final_rlt) <- c('行业','响应系数','显著程度','解释度',
                         '滞后期数','行业表现是否引起周期变化',
                         '周期是否引起行业表现变化')
final_rlt <- as.data.frame(final_rlt,
                           stringsAsFactors = F)
final_rlt[,2:7] <- apply(final_rlt[,2:7], 2, as.numeric)
return(final_rlt)
}

# 注意参数调整
# 1. 如果指标为财务数据(数字间有",")，请在getIndex.R中修改finance_data = T；
# 2. 如果报错NAs in x，请尝试调大univariateModel.R中startrow的值，
#    若超过10之后依然报错，请自行检查。
# 3. 如果报错Error in apply(num, 2, as.numeric) : 
#     dim(X) must have a positive length ，请注意1

pro <- onekeyrlt('净利润', ppidata, finance_data = T)
roe <- onekeyrlt('ROE', ppidata, finance_data = F)
income <- onekeyrlt('主营业务利润', ppidata, finance_data = T)

library('xlsx')
write.xlsx(pro, '行业净利润~经济周期.xlsx', row.names = F)

