#PACAKAGES
library(readr)
library(ggplot2)
library(e1071)

#import dataset
cc.wg<-read.csv(file.choose())
View(cc.wg) 


#EDA
summary(cc.wg)
#SCATTER PLOT
plot(cc.wg$Calories.Consumed,cc.wg$Weight.gained..grams.)
boxplot(cc.wg$Calories.Consumed,cc.wg$Weight.gained..grams.)
hist(cc.wg$Weight.gained..grams.)
hist(cc.wg$Calories.Consumed)
cc<-cc.wg$Calories.Consumed
wg<-cc.wg$Weight.gained..grams.
cor(cc,wg)


#LINEAR MODEL
reg<-lm(wg~cc)
summary(reg)
pred<-predict(reg)
pred
confint(reg,level = 0.95)
predict(reg,interval = 'predict')
ggplot(data = cc.wg,aes(x=cc,y=wg))+geom_point(color='blue')+geom_line(color='red',data = cc.wg,aes(x=cc,y=pred))

#LOGARTHMATIC MODEL
cor(log(cc),wg)
reg_log<-lm(wg~log(cc))
summary(reg_log)
confint(reg_log,level = 0.95)
pred1<-predict(reg_log,interval = "predict")
pred1

#EXPONTIAL MODEL
reg_exp<-lm(log(wg)~cc) # regression using Exponential model
summary(reg_exp)
confint(reg_exp,level = 0.95)
exp(predict(reg_exp,interval = "predict"))


cc.wg[,"CC_sq"] = cc*cc

# Quadratic model
quad_mod <- lm(wg~cc+I(cc^2),data=cc.wg)
summary(quad_mod)
confint(quad_mod,level = 0.95)
predict(reg,interval = "predict")

cc.wg[,"CC_sq"] = cc*cc
qd_model <- lm(wg~cc+CC_sq,data=cc.wg)
summary(qd_model)
confint(qd_model,level = 0.95)
predict(reg,interval = "predict")

#POLINAMIAL MODEL
poly_mod <- lm(wg~cc+I(cc^2)+I(cc^3),data=cc.wg)
summary(poly_mod) 
confint(poly_mod,level = 0.95)
predict(reg,interval = "predict")

 