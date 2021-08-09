library(naviebayes)
library(ggplot2)
library(caret)
library(psych)
library(e1071)

sal_train <- read.csv("C:/Users/Rafiya/Downloads/sal_train.csv")
View(sal_train)
str(sal_train)
sal_train$educationno<-as.factor(sal_train$educationno)
sal_test <- read.csv("C:/Users/Rafiya/Downloads/sal_test.csv")
View(sal_test)
str(sal_test)
sal_test$educationno<-as.factor(sal_test$educationno)
class(sal_test)
ggplot(data=sal_train,aes(x=sal_train$Salary, y = sal_train$age, fill = sal_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
plot(sal_train$workclass,sal_train$Salary)
plot(sal_train$education,sal_train$Salary)
plot(sal_train$educationno,sal_train$Salary)
plot(sal_train$maritalstatus,sal_train$Salary)
plot(sal_train$occupation,sal_train$Salary)
plot(sal_train$relationship,sal_train$Salary)
plot(sal_train$race,sal_train$Salary)
plot(sal_train$sex,sal_train$Salary)

ggplot(data=sal_train,aes(x=sal_train$Salary, y = sal_train$capitalloss, fill = sal_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

# Naive Bayes Model 
Model <- naiveBayes(sal_train$Salary ~ ., data = sal_train)
Model
Model_pred <- predict(Model,sal_test)
mean(Model_pred==sal_test$Salary)
confusionMatrix(Model_pred,sal_test$Salary)

library(tm)
sms_corpus<-VCorpus(VectorSource(sms_raw_NB$type))
sms_corpus
Corpus_clean<-tm_map(sms_corpus,content_transformer(tolower))
Corpus_clean<-tm_map(Corpus_clean,removeNumbers)
Corpus_clean<-tm_map(Corpus_clean,removeWords,stopwords())
Corpus_clean<-tm_map(Corpus_clean,removePunctuation)
Corpus_clean<-tm_map(Corpus_clean,stripWhitespace)

#dtm
sms_dtm<-DocumentTermMatrix(Corpus_clean)
sms_dtm

#train and test model 
sms_raw_NB_train <- sms_raw_NB[1:4169, ]
sms_raw_test  <- sms_raw_NB[1390:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[1390:5559, ]

sms_corpus_train <- sms_corpus[1:4169]
sms_corpus_test  <- sms_corpus[1390:5559]
prop.table(table(sms_raw_NB_test$type))
prop.table(table(sms_raw_NB_test$type))
sms_dict<-findFreqTerms(sms_dtm_train,5)
sms_dict
sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))
sms_train
sms_test
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}
convert_counts
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)
sms_test
View(sms_test)
str(sms_test)
View(sms_train)
install.packages("e1071")
library(e1071)

#naivebayes model
sms_classifier <- naiveBayes(sms_train, sms_raw_NB_train$type)
sms_classifier
sms_test_pred <- predict(sms_classifier, sms_test)
sms_test_pred
table(sms_test_pred)
prop.table(table(sms_test_pred))
install.packages("gmodels")
library(gmodels)
CrossTable(sms_test_pred, sms_raw_NB_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


