# 双重差分模型
# 政策背景说明，2011年至2015年间bc的减少主要归功于清洁空气行动；变化幅度可结合不同的排放控制措施进行解读。在传统的二元暴露双重差分（DID）分析中，居住在政策更为严格地区（导致bc减少幅度更大）的受试者被纳入治疗组，而居住在政策较为宽松地区的受试者则被纳入对照组。2011年（即清洁空气行动实施前）至2015年（即清洁空气行动实施后）bmi变化的组间差异可作为政策效果的指标。由于bc是一个连续变量，因此对bc变化与bmi之间的关联进行了回归分析。DID分析要求治疗组和对照组进行随机分配。因此，我们采用bc变化概率的倒数作为回归权重。该概率是通过bc与协变量（包括居住地（城市/农村）、性别、教育水平、2013年时的温度和湿度）的高斯回归模型计算得出的。
# 将 time 为 1 的数据保存为 df3，为2011年数据。
df3 <- df2[df2$time == 1, ]  
# 将 time 为 3 的数据保存为 df4，为2015年数据。
df4 <- df2[df2$time == 3, ]  
# 读取 Excel 文件（路径指定为 G: 盘下的文件），选择第二个工作表。
df5 <- read.xlsx("path\\sampled_df.xlsx ", sheet = 2)  
# 加载 dplyr 包，提供数据操作的函数。
library(dplyr)  
# 使用 left_join 合并 df3 和 df5，保留 df3 的所有行。
df_combined1 <- df3 %>% left_join(df5, by = "ID2")  
# 使用 left_join 合并 df4 和 df5，保留 df4 的所有行。
df_combined2 <- df4 %>% left_join(df5, by = "ID2")  
# 将 df_combined1 和 df_combined2 在行方向上合并，得到 df_combined_all （为2011和2015合并数据，和相应的权重）。
df_combined_all <- rbind(df_combined1, df_combined2)  
  
# 使用 ipwpoint 函数计算逆概率加权，使用 ipwpoint 函数计算逆概率加权，`bc` 为暴露变量，其他为协变量，`family = "gaussian"` 表示高斯分布。
ws <- ipwpoint(exposure = bc, family = "gaussian",
                 numerator = ~ 1,
                 denominator = ~ zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem2013 + rh2013, 
                 data = df_combined_all)  
# 将计算出的 IPW 权重存入 df_combined_all 的新列 `ws`
df_combined_all$ws = ws$ipw.weights  
# 使用 plm 函数进行面板数据回归分析，模型使用固定效应（within）方法，进行面板数据回归分析，`bmi` 是因变量，`bc` 和其他变量是自变量，`weights = ws` 指定了 IPW 权重。
model <- plm(bmi ~ bc + zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem + rh, 
               index = c("ID2", "time"),
               data = df_combined_all, 
               model = "within",
               weights = ws)  
# 输出回归模型的摘要
summary(model)  

  
