#Read the datafiles in csv formate
liver_data<- read.csv(file.choose())
#LODING ALL THE REQUIRED LIBRARIES
library(psych)
library(ggplot2)
library(caret)
library(psych)
library(VIM)
library(mice)
library(kernlab)
library(randomForest)
library(caretEnsemble)
library(shiny)
library(shinydashboard)
II.DATA
#BASIC UNDERSTANDING OF LIVER_DATA DATAFRAME
str(liver_data)
#Using  str() i analyze the basic structure of data.  Theconsist of 11 variables and 583 observation.  Response variableis dataset is an int with liver disease and  where 1 indicate a patient with liver disease and 0 indicate patient without liverdisease.  Gender variable is a factor with 2 levels male and female, while other variable are int and num type.
#III.EXPLORATORY  PLOTS
temp<-liver_data
temp$Dataset<-as.factor(temp$Dataset)
temp$Gender<-as.numeric(temp$Gender)
#Plotting a kpie chart of atrribute dataset
mytable<-table(temp$Dataset)
lbls<-c("patients without liver disease", "patients with disease")
lbls<-paste(lbls, "\n", mytable)
pie(mytable, labels =lbls, main="pie Chart of Dataset\n (with  sample sizes)", col = rainbow(length(lbls)))
#Plotting a histogram of attribute gender according to response attribute dataset
ggplot(data = temp, aes(x=Gender))+geom_histogram(binwidth = 0.2, color="black", aes(fill=Dataset))+xlab("Gender")+ ylab("Dataset")+ggtitle("Histogram of Gender")
par(mfrow=c(3,3))
library(e1071)
#Further explored the skewness in the data by plotting a histogram
col_hist <- c("#56B4E9", "#000000", "#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
hist(liver_data$Age,col ="9", border="black",las=1,
     xlab= "Age", main= "Histogram of Age")
#plotting histogram for all the feature.
for(i in 3:10)
{
  hist(liver_data[, i], cex.axis=.5, col=col_hist[i-2], las=1, xlab = names(liver_data)[i], main = paste("Histogram of", names(liver_data)[i]))
}
x <- temp[,1:5]
y <- temp[,11]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
x <- temp[,5:10]
y <- temp[,11]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
ggplot(temp, aes(x=factor(1), y = Age))+geom_boxplot(width=0.4, fill="white")+geom_jitter(aes(color=Dataset, shape=Dataset),width=0.1, size=1)+scale_color_manual(values = c("#00AF88", "#E78500"))+labs(x=NULL)
par(mfrow=c(2,2))
col_boxplot=c("#56B4E9", "#009E73", "#E69F00", "#F0E442", "#56B4E9", "#D55E00", "#CC79A7", "#999999")
boxplot(liver_data$Age, cex.axis=.5, col="#999999", main="Age")
for(i in 3:10)
{
  boxplot(liver_data[,i], cex.axis=.5, col=col_boxplot[i-2], main=names(liver_data)[i])
}
mod<-glm(Dataset~., data=liver_data)
cooksd<-cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential obs by cooks distance")
abline(h=3*mean(cooksd, na.rm = T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels = ifelse(cooksd>3*mean(cooksd, na.rm = T),names(cooksd),""),col="red")
pairs.panels(liver_data, pch = 10)
pairs.panels(liver_data[,c(3,4,6,7,8,9)])
scatter<-ggplot(data=temp, aes(x= temp$Total_Bilirubin, y=temp$Direct_Bilirubin))
scatter+geom_point(aes(color=Dataset, shape=Dataset))+
  xlab("Total_Bilirubin")+ ylab("Direct_Bilirubin")+
  ggtitle("Total_Bilirubin")
table(is.na(liver_data))
sapply(liver_data,function(x) sum(is.na (x)))
missing_plot <- aggr(liver_data, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE,
labels=names(liver_data),cex.axis=.5, gap=3, ylab=c("Missing data", "Pattern"))
liver_data_dummy<-liver_data
dmy <- dummyVars("~.", data=liver_data_dummy, fullRank=T)
liver_data_dummy <-data.frame(predict(dmy,newdata=liver_data_dummy))
str(liver_data_dummy)
imputed_liver_dt_dummy<-mice(liver_data_dummy, m=4, maxit = 20, method= 'pmm', seed= 500)
summary(imputed_liver_dt_dummy)
liver_data_dummy_imputed <- complete(imputed_liver_dt_dummy,2)
table(is.na(liver_data_dummy_imputed))
influential <- as.numeric(names(cooksd)[(cooksd > 3*mean(cooksd, na.rm = T))])
influential
liver_data_dummy_imputed_noOut <- liver_data_dummy_imputed[-influential,]
normalize<-function(x)
{
  return((x-mean(x))/sd(x))
}
liver_data_dummy_imputed_noOut_norm<-
  as.data.frame(lapply(liver_data_dummy_imputed_noOut[,c(1,3:10)],normalize))
Dataset<-liver_data_dummy_imputed_noOut$Dataset
Gender<-liver_data_dummy_imputed_noOut$Gender.Male
liver_data_dummy_imputed_noOut_norm$Gender<-Gender
liver_data_dummy_imputed_noOut_norm$Dataset<-Dataset
summary(liver_data_dummy_imputed_noOut_norm)
liver_clean_data<-liver_data_dummy_imputed_noOut_norm
str(liver_clean_data)
set.seed(80)
liver_clean_data_lm <- liver_clean_data
liver_clean_data$Dataset <- as.factor(liver_clean_data$Dataset)
Index <-createDataPartition(liver_clean_data$Dataset, p=0.8, list = FALSE)
train_data <- liver_clean_data[Index,]
test_data<-liver_clean_data[-Index,]
train_data_lm <-liver_clean_data_lm[Index,]
test_data_lm <-liver_clean_data_lm[-Index,]
#vector machine.
set.seed(200)
model_SVM <- ksvm(Dataset ~., data = train_data, kernel= "tanhdot")
pred_SVM<-predict(model_SVM,test_data)
confusionMatrix(pred_SVM,test_data$Dataset)
#logestic regression.
model_glm <- glm(formula = Dataset ~. , family = binomial, data = liver_clean_data_lm)
summary(model_glm)
model_glm2<-glm(formula = Dataset~., family = binomial, data = subset(liver_clean_data_lm,
select = c(-Gender, -Total_Bilirubin, -Direct_Bilirubin, -Aspartate_Aminotransferase)))
summary(model_glm2)
pred_lm<-predict(model_glm2,test_data_lm[,c(1,4,5,7,8,9)],type="response")
pred_lm<-ifelse(pred_lm>=0.5,1,0)
confusionMatrix(as.factor(pred_lm),as.factor(test_data_lm$Dataset))                 
#k-nearest neighboor.
ctrl<-trainControl(method = "repeatedcv", repeats = 3)
model_knn <- train(Dataset ~ ., data = train_data, method="knn", trControl=ctrl,preProcess=
                     c("center","scale"), tuneLength=20)
pred_knn<- predict(model_knn, newdata=test_data)
confusionMatrix(pred_knn,test_data$Dataset)
#random forest
set.seed(200)
model_randomForest<-randomForest(Dataset~.,data=train_data,importance=TRUE)
pred_randomForest<-predict(model_randomForest,test_data)
confusionMatrix(pred_randomForest,test_data$Dataset)

# simple ANN with only a single hidden neuron
model_nn <- neuralnet(formula = Dataset ~ .,data = train_data)


# visualize the network topology
plot(model_nn)
## Evaluating model performance 

#results_model <- NULL
test_data[1:10]
results_model <- compute(model_nn, test_data[1:10])
results_model
# obtain predicted strength values
str(results_model)
predicted_Dataset <- results_model$net.result
predicted_Dataset
#accuracy model
SVM <- confusionMatrix(pred_SVM,test_data$Dataset)$overall['Accuracy']
GLM<-confusionMatrix(as.factor(pred_lm),as.factor(test_data$Dataset))$overall['Accuracy']
KNN<-confusionMatrix(pred_knn,test_data$Dataset)$overall['Accuracy']
RandomForest<-confusionMatrix(pred_randomForest,test_data$Dataset)$overall['Accuracy']
accuracy<-data.frame(model=c("Support Vector Machine", "Logistic Regression","knn","randomforest","ANN"),Accuracy=c(SVM,GLM,KNN,RandomForest,ANN))
ggplot(accuracy,aes(x=model, y=Accuracy))+ geom_bar(stat='identity')+ theme_bw()+ggtitle('Comparison of model Accuracy')
levels(liver_clean_data$Dataset)<-make.names(levels(liver_clean_data$Dataset))
control<-trainControl(method="repeatedcv", number=10,repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList<-c('svmRadial','glm','knn','rf')
set.seed(200)
models<-caretList(Dataset~., data=liver_clean_data,trControl = control, methodList = algorithmList)
output<-resamples(models)
summary(output)
dotplot(output)
#MODEL IMPROVEMENT
set.seed(200)
train_control_SVM<-trainControl(method = "repeatedcv", number=10)
Cross_model_SVM<-train(Dataset~., data=train_data, trControl=train_control_SVM, method="svmRadial")
print(Cross_model_SVM)
pred_svm_Cross<-predict(Cross_model_SVM, test_data)
confusionMatrix(pred_svm_Cross,test_data$Dataset)
set.seed(200)
train_control_GLM<-trainControl(method="repeatedcv", number = 10)
Cross_model_GLM<-train(Dataset~., data = train_data_lm, trControl=train_control_GLM, method="glm")
print(Cross_model_GLM)
pred_glm_cross<-predict(Cross_model_GLM,test_data_lm)
pred_glm_cross<-ifelse(pred_glm_cross >=0.5,1,0)
confusionMatrix(as.factor(pred_glm_cross),as.factor(test_data_lm$Dataset))
set.seed(200)
train_control_knn<-trainControl(method = "repeatedcv", number=10)
Cross_model_knn<-train(Dataset~., data=train_data,trControl=train_control_knn, method="knn")
print(Cross_model_knn)
pred_knn_cross<-predict(Cross_model_knn,test_data)
confusionMatrix(pred_knn_cross,test_data$Dataset)
set.seed(200)
train_control_RF<-trainControl(method="repeatedcv", number = 10)
model_rf <- train(Dataset~., data=train_data, trControl=train_control_RF, method="rf")
print(model_rf)
pred_rf_cross<-predict(model_rf,test_data)
confusionMatrix(pred_rf_cross,test_data$Dataset)

