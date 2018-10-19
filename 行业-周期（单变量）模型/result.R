setwd('E:/workspace_qdf/R')

# 只用修改char就可以得到csv文件名合集
char <- '主营业务利润'
cls <- c('采掘','传媒','电气设备','电子','房地产','纺织服装',
         '非银金融','钢铁','公用事业','国防军工','化工',
         '机械设备','计算机','家用电器','建筑材料','建筑装饰',
         '交通运输','农林牧渔','汽车','轻工制造','商业贸易',
         '食品饮料','通信','休闲服务','医药生物','银行',
         '有色金属','综合')
csvfile <- paste(char,'/',cls,char,'.csv', sep = '')


final_rlt <- data.frame()
for(i in 1:length(csvfile)){
# Begin!
dff <- uniModel(csvfile[i], indgrowth)    # 得到建模数据
df <- as.data.frame(apply(dff, 2, scale)) # 对指标标准化
adf <- adf.test(df[,1])                   # 平稳性检验
adf_pvalue <- adf$p.value                 # ADF检验的p值
# 回归（当期关系）
m1 <- lm(index~growth, data = df)
sum1 <- summary(m1)
lm_coef <- sum1$coefficients[2,1]         # 回归系数
lm_pvalue <- sum1$coefficients[2,4]       # 回归系数p值
lm_r2 <- sum1$r.squared                   # 解释度
# VARselect滞后期数
infocri <- unlist(VARselect(df, lag.max=12,type="const")["selection"])
k <- min(infocri)                         # 滞后期数
# 格兰杰因果关系检验
index <- df[,1]
growth <- df[,2]
g1 <- grangertest(growth~index, order = k)
g2 <- grangertest(index~growth, order = k)
g1_pvalue <- g1$`Pr(>F)`[2]               # 行业引起周期变化p值
g2_pvalue <- g2$`Pr(>F)`[2]               # 周期引起行业变化p值
# 得到观测（单个行业的情况）
obv <- c(cls[i], lm_coef, lm_pvalue, lm_r2, k,
         g1_pvalue, g2_pvalue)
final_rlt <- rbind(final_rlt, obv)
}
