#import dataset
sal_train <- read.csv("C:/Users/Rafiya/Downloads/sal_train.csv")
View(sal_train)

sal_train$educationno <- as.factor(sal_train$educationno)
class(sal_train)


sal_test <- read.csv("C:/Users/Rafiya/Downloads/sal_test.csv")
  View(sal_test)
  
sal_test$educationno <- as.factor(sal_test$educationno)
class(sal_test)
# Building model 
 library(kernlab)
    
#svm model    
model1<-ksvm(sal_train$Salary~.,  data= sal_train, kernel = "vanilladot")
model1
Salary_prediction <- predict(model1, sal_test)
table(Salary_prediction,sal_test$Salary)
agreement <- Salary_prediction == sal_test$Salary
table(agreement)    
prop.table(table(agreement))

#rfdot model = kernal
model_rfdot<-ksvm(sal_train$Salary~.,data= sal_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=sal_test)
mean(pred_rfdot==sal_test$Salary)#85.19
    

# kernel = vanilladot
model_vanilla<-ksvm(sal_train$Salary~., 
                    data= sal_train,kernel = "vanilladot")


pred_vanilla<-predict(model_vanilla,newdata=sal_test)
mean(pred_vanilla==sal_test$Salary)#84.64

#kernal = polydot model
model_polydot<-ksvm(sal_train$Salary~., 
                    data= sal_train,kernel = "polydot")


pred_polydot<-predict(model_polydot,newdata=sal_test)
mean(pred_polydot==sal_test$Salary)#84.61

#tanhdot model=kernel
model_tanhdot<-ksvm(sal_train$Salary~., 
                    data= sal_train,kernel = "tanhdot")


pred_tanhdot<-predict(model_tanhdot,newdata=sal_test)
mean(pred_tanhdot==sal_test$Salary)#63.88


