setwd('D:\\R/GenialFlow/AshareIndustry')
#setwd('E:\\workspace_qdf/R/industryData')
ins2cyc <- function(csv_file, eco_cycle){
  CSVFILE <- read.csv(csv_file, header = T,
                      stringsAsFactors = F)
  idx <- idxMedian(df = CSVFILE,
                   time_start = "1993/3/1",
                   time_end = "2017/12/1")
  cycleRobust(dfCycle = eco_cycle,
              y = idx)
}


# 行业—周期模型回归结果查看
ins2cyc(csv_file = '采掘净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '传媒净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '电气设备净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '电子净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '房地产净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '纺织服装净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '非银金融净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '钢铁净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '公用事业净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '国防军工净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '化工净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '机械设备净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '计算机净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '家用电器净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '建筑材料净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '建筑装饰净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '交通运输净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '农林牧渔净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '汽车净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '轻工制造净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '商业贸易净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '食品饮料净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '通信净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '休闲服务净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '医药生物净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '银行净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '有色金属净利润.csv', eco_cycle = gT1k0)
ins2cyc(csv_file = '综合净利润.csv', eco_cycle = gT1k0)


