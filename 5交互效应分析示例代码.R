# 交互效应分析
# 交互效应 分析年龄和bc对bmi的交互效应
# 使用固定效应模型（plm）进行回归分析，I(bc*age): 表示bc和age之间的交互项，`I()`函数用来创建交互项，控制其他变量（如性别、教育水平、疾病历史等， index=c("ID2","time"): 数据是面板数据，ID2是个体标识符，time是时间 model="within" 表示使用固定效应模型，即控制个体和时间的固定效应。
model <- plm(bmi ~ bc + I(bc*age) + zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem + rh , 
             index = c("ID2", "time"), 
             data = df2, 
             model = "within")
# 输出回归模型的结果
summary(model)
