# 病例交叉设计
# 鉴于病例交叉设计擅长捕捉暴露因素的短期变化这一特性，因此本文采用 season 包中的 CVDdaily 这一经典数据集进行案例演示。
# 将有病例的这一天（称为病例日：case day）与附近的其它天（称为对照日：control day）进行匹配比较.
# 安装并加载 "season" 包
# install.packages("season")  
library(season) 
# 加载 CVDdaily 数据集，查看数据集中的变量.
# 加载 CVDdaily 数据集
data(CVDdaily)
# 查看数据集中的变量名  
names(CVDdaily)  

# 筛选数据，仅保留日期小于等于 1987 年 12 月 31 日的记录.
CVDdaily = subset(CVDdaily, date <= as.Date('1987-12-31'))  

# 使用病例交叉设计分析臭氧浓度对心血管疾病死亡的影响，将温度（tmpd）作为匹配的协变量，温度匹配误差范围为 1 摄氏度。
model = casecross(cvd ~ o3mean + Mon + Tue + Wed + Thu + Fri + Sat, 
                  data = CVDdaily,  
                  matchconf = 'tmpd',   
                  confrange = 1)  
# 输出模型的回归结果
summary(model)  

# 如果存在多个协变量则可以使用倾向评分，计算逆概率加权（IPW），用于调整协变量（这里为温度）的影响.
# 加载 ipw 包，进行逆概率加权分析
library(ipw)  
# 使用 ipwpoint 计算逆概率加权权重，将臭氧浓度（o3mean）作为暴露因素，采用高斯分布（正态分布）模型，分子部分为常数项，协变量（温度）作为分母部分.
ws <- ipwpoint(exposure = o3mean,  
               family = "gaussian",  
               numerator = ~ 1,   
               denominator = ~ tmpd,  
               data = CVDdaily)
# 将计算得到的逆概率加权权重赋值给 CVDdaily 数据集中的 'ws' 列.
CVDdaily$ws <- ws$ipw.weights  

# 使用数据进行病例交叉设计分析，使用权重（'ws'）作为匹配的协变量，权重匹配误差范围为 0.1。
model = casecross(cvd ~ o3mean + Mon + Tue + Wed + Thu + Fri + Sat, 
                   data = CVDdaily,  
                   matchconf = 'ws',  
                   confrange = 0.1)  
# 输出回归分析结果
summary(model)  
