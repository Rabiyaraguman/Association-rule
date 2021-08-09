#import dataset
bank <- read.csv("C:/Users/Rafiya/Downloads/bank-full (1).csv", sep=";")
View(bank)
bank<-na.omit(bank)
View(bank)

attach(bank)

library(e1071)
skewness(bank)
colnames(bank)
bank <- as.data.frame(bank)
class(bank)
#linear regression
model <- lm(y~.,data=bank)
pred1 <- predict(model,bank)
pred1
summary(model)
plot(age,pred1)
plot(pred1)
#logestic regression
model1<-glm(y~.,data=bank,family = "binomial")

#odds ratio
exp(coef(model1))

# Confusion matrix table 
prob <- predict(model,bank,type="response")
prob
summary(model1)
#confusion matrix
confusion<-table(prob>0.5,bank$y)
confusion

#Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #88.30
# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
bank[,"prob"] <- prob
bank[,"pred_values"] <- pred_values
bank[,"yes_no"] <- yes_no

# View(bank_data1[,c(1,31,36:38)])

table(bank$y,bank$pred_values)
# ROC Curve 
#install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
str(rocrperf)

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
