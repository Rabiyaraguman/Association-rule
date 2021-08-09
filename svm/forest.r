forest <- read.csv("C:/Users/Rafiya/Downloads/forest.csv")
View(forest)

# divide into training and test data
forest_train <- forest[1:290, ]
forest_test  <- forest[291:517, ]
forest_train
forest<-na.omit(forest)
forest
summary(forest)
##Training a model on the data ----
# begin by training a simple linear SVM
install.packages("kernlab")
library(kernlab)

#Support Vector Machines 
model1<-ksvm(size_category~temp+rain+wind+RH, 
             data= forest_train,kernel = "vanilladot")
help(ksvm)
model1
?ksvm

# basic information about the model
model1


# predictions on testing dataset
forest_predictions <- predict(model1, forest_test)
View(forest_test)

head(forest_predictions)

table(forest_predictions, forest_test$size_category)


agreement <- forest_predictions == forest_test$size_category
table(agreement)
prop.table(table(agreement))


## Improving model performance ----
model_rfdot<-ksvm(size_category~temp+rain+wind+RH, data= forest_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=forest_test)
mean(pred_rfdot==forest_test$size_category)
agreement_rbf <- pred_rfdot == forest_test$size_category
table(agreement_rbf)
prop.table(table(agreement_rbf))
