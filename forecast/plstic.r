#IMPORT DATASET:

PlasticSales <- read.csv("C:/Users/Rafiya/Downloads/PlasticSales.csv")
   View(PlasticSales)
   x<-data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
   colnames(x)<-month.abb
View(x)   
PlasticSales<-cbind(PlasticSales,x)
View(PlasticSales)
colnames(PlasticSales)
#input t
PlasticSales["t"]<-c(1:60)
View(PlasticSales)
PlasticSales["log_Sales"]<-log(PlasticSales["Sales"])
PlasticSales["t_square"]<-PlasticSales["t"]*PlasticSales["t"]
View(PlasticSales)
train<-PlasticSales[1:48,]
test<-PlasticSales[49:60,]
lmmodel<-lm(Sales~t,data = train)
summary(lmmodel)

########################### LINEAR MODEL #############################

linear_pred <- data.frame(predict(lmmodel, interval='predict', newdata =test))
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

######################### Additive Seasonality ##################
sea_add_model <- lm(Sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))

rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality ########################
multi_sea_model <- lm(log_Sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

# Preparing table on model and it's RMSE values 
table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)


write.csv(PlasticSales, file="plastic.csv", row.names = F)
View(PlasticSales)


Add_sea_Quad_model_final <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = PlasticSales)
summary(Add_sea_Quad_model_final)



pred_new <- predict(Add_sea_Quad_model_final, newdata = test, interval = 'predict')
pred_new <- as.data.frame(pred_new)
View(pred_new)

plot(Add_sea_Quad_model_final)


acf(Add_sea_Quad_model_final$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot

A <- arima(Add_sea_Quad_model_final$residuals, order = c(1,0,0))
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)


library(forecast)
errors_12 <- forecast(A, h = 12)

future_errors <- data.frame(errors_12)
View(future_errors)
class(future_errors)
future_errors <- future_errors$Point.Forecast


predicted_new_values <- pred_new + future_errors
View(predicted_new_values)
write.csv(predicted_new_values, file = "predicted_new_values.csv", row.names = F)
getwd()

