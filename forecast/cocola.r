library(readxl)
#IMPORT DATASET:

Cocola<-read_xlsx(file.choose())
View(Cocola)
str(Cocola)
summary(Cocola)
Q1 <-  ifelse(grepl("Q1",Cocola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",Cocola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",Cocola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",Cocola$Quarter),'1','0')
Cocola<-cbind(Cocola,Q1,Q2,Q3,Q4)
View(Cocola)
colnames(Cocola)
# input t
Cocola["t"] <- c(1:42)
View(Cocola)

Cocola["log_Sales"]<-log(Cocola["Sales"])
Cocola["t_square"]<-Cocola["t"]*Cocola["t"]
attach(Cocola)

train<-Cocola[1:36,]

test<-Cocola[37:42,]
########################### LINEAR MODEL #############################

linear_model <- lm(Sales ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test))
linear_pred
rmse_linear <- sqrt(mean((test$Sales-linear_pred$fit)^2, na.rm = T))
rmse_linear
######################### Exponential #################################

expo_model <- lm(log_Sales ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Sales-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model <- lm(Sales ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model <- lm(Sales ~ Q1+Q2+Q3+Q4, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model <- lm(Sales ~ t+t_square+Q1+Q2+Q3+Q4, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_Sales ~ Q1+Q2+Q3+Q4, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea 

# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
# Additive Seasonality with Quadratic trend  has least RMSE value

new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=Cocola)
new_model_pred<-data.frame(predict(new_model,newdata=Cocola,interval='predict'))
new_model_fin <- new_model$fitted.values

View(new_model_fin)
#predicted quarter values

Quarter <- as.data.frame(Cocola$Quarter)

Final <- as.data.frame(cbind(Quarter,Cocola$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 

plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",
     col.axis="Green",type="s")
View(Final)
