setwd('D:\\R/GenialFlow')
#setwd('E:\\workspace_qdf/R')
# 盈收规模+利润规模 -> 标准化 -> 代数平均 -> 排序 -> 前40%

# 获取数据
dat <- read.csv('data_draft.csv', header = T, stringsAsFactors = F)
enter <- dat[,3]     # 提取行业名称
price <- dat[,11:17] # 选择用来比较行业的指标

# 整理数据
price_scale <- as.data.frame(apply(price, 2, scale))               # 数据标准化
idx <- apply(price_scale, 1, mean)                                 # 代数平均
enter_idx <- as.data.frame(cbind(enter,idx), stringsAsFactors = F) # 合并（企业+指标）
enter_idx[,2] <- unlist(lapply(enter_idx[,2], as.numeric))         # 指标数值化

# 排序
enter_idx <- enter_idx[order(enter_idx[,2], decreasing = T),]

# 获得行业领头(industry_Leader)(前30%)
enter_leader <- enter_idx[1:round(nrow(enter_idx)*0.3,0),]