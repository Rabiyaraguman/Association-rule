#install.packages
library(readr)
library(ggplot2)

#import dataset
emp <- read.csv("C:/Users/Rafiya/Downloads/emp_data (1).csv")
   View(emp)

#EDA
summary(emp)
#scatter plot
plot(emp$Salary_hike,emp$Churn_out_rate)
?plot
attach(emp)
Salary_hike
Churn_out_rate

#coreelation coefficient
cor(Salary_hike,Churn_out_rate)

#linear regression
reg<-lm(Churn_out_rate~Salary_hike)
reg
summary(reg)
pred<-predict(reg)
pred
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/(nrow(emp)))
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.99)
predict(reg,interval = "predict")
predict
ggplot(data=emp,aes(x=Salary_hike,y=Churn_out_rate))+geom_point(color='blue')+geom_line(color='red',data = emp,aes(x=Salary_hike,y=pred))


#Logrithmatic model
x=log(Salary_hike);y=Churn_out_rate
plot(log(Salary_hike),Churn_out_rate)
cor(log(Salary_hike),Churn_out_rate)
reg_log<-lm(Churn_out_rate~log(Salary_hike))
reg_log
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(emp))
sqrt(mean(reg_log$residuals^2))
confint(reg_log,level = 0.95)
predict(reg_log,interval = "predict")


#exponantial model
#x = Waist,y=log(AT)
plot(Salary_hike,log(Churn_out_rate))
cor(Salary_hike,log(Churn_out_rate))
reg_exp<-lm(log(Churn_out_rate)~Salary_hike)
reg_exp
summary(reg_exp)
reg_exp$residuals
sqrt(sum(reg_exp$residuals^2)/nrow(emp))
logat<-predict(reg_exp)
logat
at<-exp(logat)
error<-emp$Churn_out_rate-at
error
sqrt(sum(error^2)/nrow(emp))


#ployinomal quardic with 2degree
plot(Salary_hike,Churn_out_rate)
plot(Salary_hike*Salary_hike,Churn_out_rate)
cor(Salary_hike*Salary_hike,Churn_out_rate)
plot(Salary_hike*Salary_hike,log(Churn_out_rate))
cor(Salary_hike,log(Churn_out_rate))
cor(Salary_hike*Salary_hike,log(Churn_out_rate))
reg2degree<-lm(log(Churn_out_rate)~Salary_hike+I(Salary_hike*Salary_hike))
reg2degree
summary(reg2degree)
logpol<-predict(reg2degree)
expy<-exp(logpol)
expy
err<-emp-expy
err
sqrt(sum(err^2)/nrow(emp))
confint(reg2degree,level = 0.95)
predict(reg2degree,interval = "predict")
ggplot(data=emp,aes(x=Salary_hike+I(Salary_hike^2),y=log(Churn_out_rate)))+geom_point(color='blue')+geom_line(color='red',data = emp,aes(x=Salary_hike+I(Salary_hike^2),y=logpol))

