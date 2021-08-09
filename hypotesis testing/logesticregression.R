library(caTools)
library(psych)
library(mice)
library(caret)
library(rpart)
library(nnet)
#import dataset
bank <- read.csv("C:/Users/Rafiya/Downloads/bank-full (1).csv", sep=";")
View(bank)
#Exploring dataset:
str(bank)
summary(bank)

boxplot(bank$age, main="Age box plot",yaxi="n",xlab="Age",horizontal=TRUE,col=terrain.colors(2))
hist(bank$age,col = terrain.colors(10))

#data cleaning
for(i in 1 : nrow(bank)) {
  if (bank$age[i] < 20){
    bank$age[i] = "Teenagers"
  } else if (bank$age[i] < 35 & bank$age[i] > 19){
    bank$age[i] = "Young Adults"
  } else if (bank$age[i] < 60 & bank$age[i] > 34){
    bank$age[i] = "Adults"
  } else if (bank$age[i] > 59){
    bank$age[i] = "senior citizens"
  }
     
  
}  
bank$age<-as.factor(bank$age)

bank$y<-ifelse(bank$y =="yes",1,0)
bank$y<-as.factor(bank$y)
#pairs
pairs.panels(bank[, c(1:4,17)])
pairs.panels(bank[, c(5:8,17)])
bank_sub<-bank[, c(1:4, 7:9,12,14,16,17)]
str(bank_sub)
bank_sub[bank_sub=="unknown"]<-NA


#visualization:

par(mfrow=c(2,2),las=2)
boxplot(duration~ y, data = bank_sub,col="blue")
boxplot(pdays~y, data = bank_sub,col="red")


#model construction and evaluation
library(caret)
split<-createDataPartition(y=bank_sub$y,p=0.8,list=FALSE)
train<-bank_sub[split,]
test<-bank_sub[split,]
 dim(train)
dim(test) 
table(train$y)
table(test$y)

#Logestic model
train_log<-glm(y~.,binomial(link="logit"),train)
summary(train_log)
test_prob<-predict.glm(train_log,test[,1:10],type="response")
test_prob
#plot and roc curve
pr<-prediction(test_prob, test$y )
prf<-performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)

test_prediction<-cut(test_prob,c(-Inf,0.5,Inf),labels = c("No","Yes"))
test_prediction
             
auc<-performance(pr,measure = "auc")
auc

