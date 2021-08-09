#import dataset
creditcard <- read.csv("C:/Users/Rafiya/Downloads/creditcard.csv")
View(creditcard)
summary(creditcard)
creditcard<-na.omit(creditcard) 
View(creditcard)

#logestic model
modelfit<-glm(majorcards~.,data=creditcard[,-1],family="binomial")
summary(modelfit)

#odds ratio
exp(coef(modelfit))

# Confusion matrix table 
prob <- predict(modelfit,type=c("response"),creditcard)
prob

confusion<-table(prob>0.5,creditcard$majorcards)
confusion

#Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 70.62

# ROC Curve 
#install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,creditcard$majorcards)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

