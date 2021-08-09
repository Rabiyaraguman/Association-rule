library(readxl)
Airlines_Data <- read_xlsx(file.choose())
View(Airlines_Data)

# Pre Processing
# So creating 12 dummy variables 
X <- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names 
View(X)
Airlines_Data<-cbind(Airliness,X)
View(Airlines_Data)
colnames(Airlines_Data)

# input t
Airlines_Data["t"] <- c(1:96)
View(Airlines_Data)

Airlines_Data["log_Passenger"]<-log(Airlines_Data["Passengers"])
Airlines_Data["t_square"]<-Airlines_Data["t"]*Airlines_Data["t"]
attach(Airlines_Data)

train<-Airlines_Data[1:84,]

test<-Airlines_Data[85:96,]
## Preprocesing completed



########################### LINEAR MODEL #############################

linear_model <- lm(Passengers ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test))
linear_pred
rmse_linear <- sqrt(mean((test$Passengers-linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model <- lm(log_Passenger ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Passengers-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model <- lm(Passengers ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Passengers-Quad_pred$fit)^2, na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model <- lm(Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Passengers-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Passengers - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_Passenger ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea 

# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)




# Multiplicative Seasonality Linear trend  has least RMSE value

new_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = Airlines_Data)
new_model_pred<-data.frame(predict(new_model,newdata=Airlines_Data,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(Airlines$Month)

Final <- as.data.frame(cbind(Month,Airlines_Data$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)
plot(Final$new_model_fin,type="o")


# Additive seasonality with Quadratic has least RMSE value

write.csv(Airlines_Data, file="Airlines_Data.csv", row.names = F)
View(Airlines_Data)

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add_sea_Quad_model_final <- lm(Footfalls ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = Airlines_Data)
summary(Add_sea_Quad_model_final)


####################### Predicting new data #############################
pred_new <- predict(Add_sea_Quad_model_final, newdata = test, interval = 'predict')
pred_new <- as.data.frame(pred_new)
View(pred_new)

plot(Add_sea_Quad_model_final)


acf(Add_sea_Quad_model_final$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot

A <- arima(Add_sea_Quad_model_final$residuals, order = c(1,0,0))
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)

# predicting next 12 months errors using arima( order =c(1,0,0))

library(forecast)
errors_12 <- forecast(A, h = 12)

future_errors <- data.frame(errors_12)
View(future_errors)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# predicted values for new data + future error values 

predicted_new_values <- pred_new + future_errors
View(predicted_new_values)
write.csv(predicted_new_values, file = "predicted_new_values.csv", row.names = F)
getwd()
