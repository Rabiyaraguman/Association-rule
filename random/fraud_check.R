library(randomForest)
library(MASS)
library(caret)


#import dataset
Fraud_check <- read.csv("C:/Users/Rafiya/Downloads/Fraud_check.csv")
View(Fraud_check)
set.seed(123)
hist(Fraud_check$Taxable.Income)
hist(Fraud_check$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
        breaks=c(seq(40,60,80)), col = c("blue","red", "green","violet"))

Risky_Good = ifelse(Fraud_check$Taxable.Income<= 30000, "Risky", "Good")
# if Taxable Income is less than or equal to 30000 then Risky else Good.
FCtemp= data.frame(Fraud_check,Risky_Good)
FC = FCtemp[,c(1:7)]
   
str(FC)
table(FC$Risky_Good)   
# Data Partition
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]
set.seed(213)
rf <- randomForest(Risky_Good~., data=train)
rf 
attributes(rf)
# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)
head(train$Risky_Good)
confusionMatrix(pred1, train$Risky_Good)
# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good) 
# Error Rate in Random Forest Model :
plot(rf)
# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
                  trace = TRUE, improve = 0.05)
rf1 <- randomForest(Risky_Good~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                       proximity = TRUE)
rf1
pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$Risky_Good)
   
# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$Risky_Good)
   
# no of nodes of trees
   
hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")
# Variable Importance :
   
varImpPlot(rf1)
varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
# Quantitative values 
importance(rf1)
varUsed(rf) 
# Partial Dependence Plot 
partialPlot(rf1, train, Taxable.Income, "Good")
# Extract single tree from the forest :
   
tr1 <- getTree(rf1, 2, labelVar = TRUE)
   
# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, FC$Risky_Good)
   
   