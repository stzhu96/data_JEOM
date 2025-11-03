# 中介分析
# 中介分析 - 分析BC是否通过bmi来影响认知水平的变化
# 使用固定效应模型分析 BMI 和 BC 之间的关系 
model1 <- plm(bmi ~ bc + zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem + rh , 
              index = c("ID2", "time"),
              data = df2, 
              model = "within")  
# 使用固定效应模型分析 BMI 和 BC 对认知（con）的影响
model2 <- plm(con ~ bc + bmi + zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem + rh , 
              index = c("ID2", "time"), 
              data = df2, 
              model = "within")  
# 计算BC对BMI的影响，BMI对认知的影响。
# 从 `model1` 模型中提取 BC 对 BMI 的影响系数
a <- coef(model1)["bc"] 
# 从 `model2` 模型中提取 BMI 对认知（con）的影响系数
b <- coef(model2)["bmi"]  

# 计算BC对因变量con的直接影响）
# 从 `model2` 模型中提取 BC 对认知（con）的直接影响系数
c <- coef(model2)["bc"]  
# 计算中介效应 = a * b, 中介效应等于BMI对认知的影响系数和BMI对认知的乘积。
mediation_effect <- a * b  

# 计算路径a的标准误差，vcov函数返回模型系数的协方差矩阵。
se_a <- sqrt(vcov(model1)["bc", "bc"])  
# 计算路径b的标准误差，vcov函数返回模型系数的协方差矩阵。
se_b <- sqrt(vcov(model2)["bmi", "bmi"])  
# 计算中介效应的标准误差（假设BC对BMI的影响，BMI对认知的影响独立）。
se_mediation <- sqrt((b * se_a)^2 + (a * se_b)^2)  
# 计算t值和p值，用于检验中介效应是否显著。
# 中介效应的t值
t_value <- mediation_effect / se_mediation  
# p值，根据t值和自由度计算
p_value <- 2 * (1 - pt(abs(t_value), df = nrow(df2) /3- 2))  

# 计算总效应, 总效应是BC对认知的直接影响系数和中介效应的和。
total_effect <- c + a * b  
# 计算总效应的标准误差
# 总效应的标准误差
se_total <- sqrt((se_a^2 * b^2) + (se_b^2 * a^2))  
# 计算总效应的t值
t_value_total <- total_effect / se_total 
# 计算总效应的p值
p_value_total <- 2 * (1 - pt(abs(t_value_total), df = nrow(df2) /3- 2))

