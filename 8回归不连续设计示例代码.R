# 回归不连续设计
# 安装 rdrobust 包，这个包用于回归不连续性设计（Regression Discontinuity Design, RDD）的分析
# install.packages("rdrobust")  
# 加载 rdrobust 包，提供用于回归不连续性设计的函数（同样我们将断点设为2013年）
library(rdrobust)  
# 将 time 为 1 的数据保存为 df3，，为2011年数据
df3 <- df2[df2$time == 1, ]  
# 将 time 为 3 的数据保存为 df4，，为2015年数据
df4 <- df2[df2$time == 3, ]  
# 读取 Excel 文件（路径指定为 G: 盘下的文件），选择第二个工作表
df5 <- read.xlsx("path\\sampled_df.xlsx ", sheet = 2)  
# 加载 dplyr 包，提供数据操作的函数
library(dplyr)  
# 使用 left_join 合并 df3 和 df5，保留 df3 的所有行
df_combined1 <- df3 %>% left_join(df5, by = "ID2")  
# 使用 left_join 合并 df4 和 df5，保留 df4 的所有行
df_combined2 <- df4 %>% left_join(df5, by = "ID2")  
# 将 df_combined1 和 df_combined2 在行方向上合并，得到 df_combined_all （为2011和2015合并数据，和相应的权重）
df_combined_all <- rbind(df_combined1, df_combined2)  

# 使用 ipwpoint 函数计算逆概率加权，使用 ipwpoint 函数计算逆概率加权，`bc` 为暴露变量，其他为协变量，`family = "gaussian"` 表示高斯分布
ws <- ipwpoint(exposure = bc, family = "gaussian",
               numerator = ~ 1,
               denominator = ~ zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem2013 + rh2013, 
               data = df_combined_all)  
# 将计算出的 IPW 权重存入 df_combined_all 的新列 `ws`
df_combined_all$ws = ws$ipw.weights  

# 使用 rdrobust 进行回归不连续性设计分析，# 使用 rdrobust 函数进行回归不连续性分析，bmi 为因变量，bc 为自变量，c 为切点
model <- rdrobust(y = df_combined_all$bmi, x = df_combined_all$bc, c = mean(df_combined_all$ws))  
# 输出回归不连续性分析的结果摘要
summary(model)  
