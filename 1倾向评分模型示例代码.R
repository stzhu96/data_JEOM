# 倾向评分模型
# 加载处理面板数据所需的R包
# 安装相应的R包
# install.packages(“plm”)；install.packages(“ipw”)
# 用于面板数据分析
library(plm)
# 用于计算逆概率加权（IPW）  
library(ipw)  
# 使用ipwpoint函数计算逆概率加权（IPW）权重
# 指定暴露变量（在此为bc）,设置模型为高斯分布适用于连续变量，指定分子部分，这里为1，即没有额外的协变量，指定分母部分，即协变量。
ws <- ipwpoint(exposure = bc,  
               family = "gaussian",  
               numerator = ~ 1,  
               denominator = ~ zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem + rh,  
               data = df2)
# 将计算出的逆概率加权（IPW）权重存储到df2数据框的ws列中
df2$ws = ws$ipw.weights  
# 使用面板数据模型（plm函数）进行回归分析，结果变量为bmi，解释变量包括bc和一组协变量， 指定面板数据的索引变量，其中ID2表示个体ID，time表示时间，使用"within"模型，表示固定效应模型（只考虑个体内的变动），使用计算的IPW权重进行加权。
model <- plm(bmi ~ bc + zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem + rh,  
                            index = c("ID2", "time"),  
                            data = df2,  
                            model = "within",  
                            weights = ws)   
# 输出回归模型的结果摘要
summary(model)  
               