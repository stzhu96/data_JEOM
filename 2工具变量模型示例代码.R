# 工具变量模型
# 加载处理面板数据所需的R包
# 用于面板数据分析
library(plm) 
# 估计暴露变量（bc）的工具变量模型：通过线性回归模型 (lm()) 估计暴露变量 bc，并使用 10Umean、10Vmean、BLDmean、BLHmean 作为工具变。
model <- lm(bc ~ `10Umean` + `10Vmean` + `BLDmean` + `BLHmean`, data = df2) 
# 预测bc
predicted_bc <- predict(model, newdata = df2) 
# 将预测值（工具变量）添加到数据中
df2$predicted_bc <- predicted_bc 
# 使用面板数据模型（plm函数）进行回归分析，结果变量为bmi，解释变量包括predicted_bc和一组协变量， 指定面板数据的索引变量，其中ID2表示个体ID，time表示时间，使用"within"模型，表示固定效应模型（只考虑个体内的变动）。
model<-plm(bmi ~ predicted_bc + zrea + sex + eduac + age + marac + retir + hibpe + diabe + acplay + drink + smoke + tem + rh , 
           index=c("ID2","time"), 
           data=df2, 
           model="within")
# 显示回归模型的统计结果
summary(model) 
