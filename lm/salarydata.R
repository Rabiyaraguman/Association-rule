#PACCKAGES
library(readr)
library(ggplot2)
library(e1071)

#IMPORT DATASET
Salary_Data <- read.csv("C:/Users/Rafiya/Downloads/Salary_Data (1).csv")
   View(Salary_Data)

#EDA
   summary(Salary_Data)   
#SCATTER PLOT
   plot(Salary_Data$YearsExperience,Salary_Data$Salary)
attach(Salary_Data)
YearsExperience
Salary
kurtosis(Salary)
kurtosis(YearsExperience)
skewness(Salary)
skewness(YearsExperience)
boxplot(Salary_Data)
cor(Salary,YearsExperience)


#LINEAR REGRESSION
reg<-lm(Salary~YearsExperience,data = Salary_Data)
reg
summary(reg)
pred<-predict(reg)
pred  
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(Salary_Data))
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.95)
predict(reg,interval = "predict")
ggplot(data = Salary_Data,aes(x=YearsExperience,y=Salary))+geom_point(color='blue')+geom_line(color='red',data = Salary_Data,aes(x=YearsExperience,y=pred))


#LOGARTHMATIC MODEL
x=log(YearsExperience);y=Salary
plot(log(YearsExperience),Salary)
cor(log(YearsExperience),Salary)
reglog<-lm(Salary~log(YearsExperience))
summary(reglog)
pred1<-predict(reglog)
pred1
sqrt(sum(reg$residuals^2)/nrow(Salary_Data))
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.95)
predict(reg,interval = "predict")
ggplot(data = Salary_Data,aes(x=YearsExperience,y=Salary))+geom_point(color='blue')+geom_line(color='red',data = Salary_Data,aes(x=YearsExperience,y=pred1))


#EXPONTIAL MODEL
plot(YearsExperience,log(Salary))
cor(YearsExperience,log(Salary))
regexp<-lm(log(Salary)~YearsExperience)
summary(regexp)
confint(regexp,level=0.95)
predict(regexp,interval="predict")
regexp$residuals
sqrt(sum(regexp$residuals^2)/nrow(emp))
logat<-predict(regexp)
logat
at<-exp(logat)
error<-Salary_Data$Salary
error
sqrt(sum(error^2)/nrow(Salary_Data))


#ployinomal quardic with 2degree
reg2degree<-lm(log(Salary)~YearsExperience+I(YearsExperience*YearsExperience))
reg2degree
summary(reg2degree)
logpol<-predict(reg2degree)
expy<-exp(logpol)
expy
err<-Salary_Data-expy
err
sqrt(sum(err^2)/nrow(emp))
confint(reg2degree,level = 0.95)
predict(reg2degree,interval = "confidence")
ggplot(data=Salary_Data,aes(x=YearsExperience+I(YearsExperience^2),y=log(Salary)))+geom_point(color='blue')+geom_line(color='red',data = Salary_Data,aes(x=YearsExperience+I(YearsExperience^2),y=logpol))


#ployinomal quardic with 3degree
reg3degree<-lm(log(Salary)~YearsExperience+I(YearsExperience*YearsExperience)+I(YearsExperience*YearsExperience*YearsExperience))
reg3degree
summary(reg3degree)
logpol<-predict(reg3degree)
expy<-exp(logpol)
expy
err<-Salary_Data-expy
err
sqrt(sum(err^2)/nrow(Salary_Data))
confint(reg2degree,level = 0.95)
predict(reg2degree,interval = "confidence")
ggplot(data=Salary_Data,aes(x=YearsExperience+I(YearsExperience^2),y=log(Salary)))+geom_point(color='blue')+geom_line(color='red',data = Salary_Data,aes(x=YearsExperience+I(YearsExperience^2)+I(YearsExperience^3),y=logpol))


