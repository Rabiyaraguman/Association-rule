#packages
library(readr)
library(e1071)
library(ggplot2)

#import dataset
delivery <- read.csv("C:/Users/Rafiya/Downloads/delivery_time (1).csv")
   View(delivery)
#EDA
   summary(delivery)
attach(delivery)   
plot(Delivery.Time,Sorting.Time)   
boxplot(Delivery.Time,Sorting.Time)
barplot(Delivery.Time,Sorting.Time)
hist(Delivery.Time)
hist(Sorting.Time)
cov(Sorting.Time,Delivery.Time)
#LINEAR MODEL
reg<-lm(Delivery.Time~Sorting.Time)
summary(reg)
pred<-predict(reg)
pred
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(delivery))
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.95)
predict(reg,interval = "predict")
ggplot(data = delivery,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='blue')+geom_line(color='red',data = delivery,aes(x=Sorting.Time,y=pred))

#LOGARTHMATIC MODEL

plot(Sorting.Time,Delivery.Time)
cor(Sorting.Time,Delivery.Time)
x=log(Sorting.Time);y=Delivery.Time
reg$residuals
sqrt(sum(reg$residuals^2)/nrow(delivery))
sqrt(mean(reg$residuals^2))
reg_log<-lm(Delivery.Time~log(Sorting.Time))
summary(reg_log)
pred1<-predict(reg_log)
confint(reg_log,level = 0.95)
predict(reg_log,interval = "predict")
ggplot(data = delivery,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='blue')+geom_line(color='red',data = delivery,aes(x=Sorting.Time,y=pred1))

#EXPONTIAL MODEL

x=Sorting.Time;y=log(Delivery.Time)
plot(Sorting.Time,log(Delivery.Time))
cor(Sorting.Time,log(Delivery.Time))
reg_exp<-lm(log(Delivery.Time)~Sorting.Time)
summary(reg_exp)
confint(reg_exp,level = 0.95)
predict(reg_exp,interval="predict")
pred2<-predict(reg_exp)
pred2
at<-exp(pred2)
error<-delivery$Delivery.Time-at
error
sqrt(sum(error^2)/nrow(delivery))
ggplot(data = delivery,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='blue')+geom_line(color='red',data = delivery,aes(x=Sorting.Time,y=at))

#ployinomal quardic with 2degree

plot(Sorting.Time*Sorting.Time,Delivery.Time)
cor(Sorting.Time*Sorting.Time,Delivery.Time)
plot(Sorting.Time*Sorting.Time,log(Delivery.Time))
cor(Sorting.Time,log(Delivery.Time))
cor(Sorting.Time*Sorting.Time,log(Delivery.Time))
reg2degree<-lm(log(Delivery.Time)~Sorting.Time+I(Sorting.Time*Sorting.Time))
reg2degree
summary(reg2degree)
logpol<-predict(reg2degree)
expy<-exp(logpol)
expy
err<-delivery-expy
err
sqrt(sum(err^2)/nrow(delivery))
confint(reg2degree,level = 0.95)
predict(reg2degree,interval = "confidence")
ggplot(data=delivery,aes(x=Sorting.Time+I(Sorting.Time^2),y=log(Delivery.Time)))+geom_point(color='blue')+geom_line(color='red',data = delivery,aes(x=Sorting.Time+I(Sorting.Time^2),y=logpol))

#ploynainal with 3 degree

reg3degree<-lm(log(Delivery.Time)~Sorting.Time+I(Sorting.Time*Sorting.Time)+I(Sorting.Time*Sorting.Time*Sorting.Time))
summary(reg3degree)
logpol3<-predict(reg3degree)
expy3<-exp(logpol3)
ggplot(data=delivery,aes(x=Sorting.Time+I(Sorting.Time^2)+I(Sorting.Time^3),y=log(Delivery.Time)))+geom_point(color='blue')+geom_line(color='red',data = delivery,aes(x=Sorting.Time+I(Sorting.Time^2)+I(Sorting.Time^3),y=expy3))
