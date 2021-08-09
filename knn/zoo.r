library(lattice)

library(caret)
library(pROC)
library(mlbench)
#import dataset:

Zoo <- read.csv("C:/Users/Rafiya/Downloads/Zoo.csv")
  View(Zoo)
  zoo1 <- Zoo[,2:18]
  str(zoo1)

  
  # Data partition
  set.seed(123)
  ind <- sample(2,nrow(zoo1), replace = T, prob = c(0.7,0.3))
  train <- zoo1[ind==1,]
  test <- zoo1[ind==2,]
  
  # KNN Model 
  
  trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3
                            # classprobs are needed when u want to select ROC for optimal K Value
  )
  set.seed(222)
  fit <- train(type ~., data = train, method = 'knn', tuneLength = 20,
               trControl = trcontrol, preProc = c("center","scale"))
  
  fit # the optimum value for k should be 7
  
  plot(fit)
  varImp(fit)
  
  pred <- predict(fit, newdata = test )
  confusionMatrix(pred, test$type)
  # 89.66 % is accuracy  
  