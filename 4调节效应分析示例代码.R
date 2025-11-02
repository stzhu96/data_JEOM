# 调节效应分析
# 调节效应 - 分析年龄在bc和bmi之间的调节效应
# 计算age的中位数
age_median <- median(df2$age, na.rm = TRUE)  
# 根据age的中位数将数据集分为高年龄和低年龄
# 将 `age` 大于中位数的数据筛选出来，创建一个新的数据框 `high_age_df`，代表高年龄组。
high_age_df <- df2[df2$age > age_median, ]  
# 将 `age` 小于或等于中位数的数据筛选出来，创建一个新的数据框 `low_age_df`，代表低年龄组。
low_age_df <- df2[df2$age <= age_median, ]  

# 进行分组分析：对高年龄组进行分析, 使用固定效应模型（`plm`）对高年龄组进行回归分析，分析黑碳（`bc`）对体重指数（`bmi`）的影响, 控制协变量（如性别、教育、疾病史等），`index = c("ID2", "time")` 表示面板数据的 ID 和时间，`model = "within"` 表示使用固定效应模型。
model <- plm(bmi ~ bc + zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem + rh , 
             index = c("ID2", "time"),
             data = high_age_df, 
             model = "within")  
# 输出高年龄组回归模型的结果
summary(model)  

# 进行分组分析：对低年龄组进行分析, 使用固定效应模型（`plm`）对低年龄组进行回归分析，分析黑碳（`bc`）对体重指数（`bmi`）的影响,与高年龄组相同，控制其他协变量，并使用固定效应模型进行回归。
model <- plm(bmi ~ bc + zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem + rh , 
             index = c("ID2", "time"), 
             data = low_age_df, 
             model = "within")  
# 输出低年龄组回归模型的结果
summary(model)  

# 对整个数据集进行回归分析，以便比较高低年龄组的差异，对整个数据集（高年龄和低年龄组）进行回归分析，分析黑碳（`bc`）对体重指数（`bmi`）的影响，控制所有协变量，并使用固定效应模型进行回归。
model <- plm(bmi ~ bc + zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem + rh , 
             index = c("ID2", "time"), 
             data = df2, 
             model = "within")  
#输出整个数据集的回归模型结果
summary(model)  
