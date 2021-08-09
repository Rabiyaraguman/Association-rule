#INSTALL PACKAGES:
library(class)
library(lattice)
library(gmodels) 
library(caret)
#import dataset:
glass <- read.csv("C:/Users/Rafiya/Downloads/glass (1).csv")
  View(glass)
  # table of glass
  table(glass$Type)
  glass$type<-as.factor(glass$Type)
  str(glass)
  round(prop.table(table(glass$Type)) * 100,  1)
  
  summary(glass[c("RI","Na","Mg")])
  # create normalization function
  norm <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  #test normalization
  norm(c(1,2,3,4,5))
  ## [1] 0.00 0.25 0.50 0.75 1.00
  norm(c(10,20,30,40,50))
  ## [1] 0.00 0.25 0.50 0.75 1.00
  
  glass_n<- as.data.frame(lapply(glass[1:9],norm))
  View(glass_n)
  summary(glass_n[c("RI","Na","Mg")])
  # glass_n <- cbind(glass$Type,glass_n[1:9])
  View(glass_n)
  
  #create training and test datasets
  set.seed(123)
  ind <- sample(2, nrow(glass_n), replace = TRUE, prob = c(0.7,0.3))
  glass_train <- glass_n[ind==1,]
  glass_test <-  glass_n[ind==2,]
  
 
  
  #Get labels for training and test datasets
  set.seed(123)
  ind1 <- sample(2, nrow(glass), replace = TRUE, prob = c(0.7,0.3))
  glass_train_labels <- glass[ind1==1,10]
  glass_test_labels <-  glass[ind1==2,10]
  #raining a model on data.
  library(class)          # to call class package
  
  NROW(glass_train_labels) 
  
  #To identify optimum value of k
  knn.26 <-  knn(train=glass_train, test=glass_test, cl=glass_train_labels, k=26)
  knn.27 <-  knn(train=glass_train, test=glass_test, cl=glass_train_labels, k=27)
  
  ## Let's calculate the proportion of correct classification for k = 26, 27 
  
  ACC.26 <- 100 * sum(glass_test_labels== knn.26)/NROW(glass_test_labels)  # For knn = 26
  ACC.27 <- 100 * sum(glass_test_labels== knn.27)/NROW(glass_test_labels)  # For knn = 27
  ACC.26  #Accuracy is 64.91%
  ACC.27  #Accuracy is 64.91%
  
  table(knn.26 ,glass_test_labels) 
  table(knn.27 ,glass_test_labels) 
  
  confusionMatrix(knn.26 ,glass_test_labels) 
  
  
  glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=5)
  table(glass_test_pred,glass_test_labels)  
 
 
  # load the gmodel library
  
  CrossTable(x=glass_test_labels,y=glass_test_pred,prop.chisq = FALSE) 
  #Improve the performance of model.
  i=1                         
  k.optm=1                     # declaration to initiate for loop
  for (i in 1:28){ 
    glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=5)
    
    k.optm[i] <- 100 * sum(glass_test_labels == glass_test_pred)/NROW(glass_test_labels)
    k=i  
    cat(k,'=',k.optm[i],'\n')       
  }
  
  
  plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")  # to plot % accuracy wrt to k-value
  
  
 