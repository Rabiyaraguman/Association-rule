

normalize<-function(x){
  return(x-min(x))/(max(x)-min(x))
}
dataa <- read.csv("C:/Users/Rafiya/Downloads/forestfires (1).csv",stringsAsFactors = TRUE)
View(dataa)
data<-dataa[,c(3:31)]

View(data)
summary(data)
class(data)
str(data)

data_norm<-as.data.frame(lapply(data, FUN=normalize))

data$size_category<-as.numeric(data$size_category)
str(data$size_category)
 View(data$size_category) 

forest_train <- data[1:290, ]
forest_test  <- data[291:517, ]
forest_train

# train the neuralnet model
install.packages("neuralnet")
library(neuralnet)
forest_model <- neuralnet(size_category~temp+rain+wind+RH, 
                           data= forest_train,act.fct = "logistic")

# visualize the network topology
plot(forest_model)

## Evaluating model performance 



#results_model <- NULL
forest_test[1:10]
results_model <- compute(forest_model, forest_test[1:10])

# obtain predicted strength values
str(results_model)
predicted_size_category <- results_model$net.result
predicted_size_category

# examine the correlation between predicted and actual values
cor(predicted_size_category, forest_test$rain)

## Improving model performance 
# a more complex neural network topology with 10 hidden neurons
forest_model2 <- neuralnet(size_category~temp+rain+wind+RH, 
                          data= forest_train,hidden = 5)
str(forest_model2)

# plot the network
plot(forest_model2)


# evaluate the results as we did before
model_results2 <- compute(forest_model2, forest_test[1:8])
predicted_size_category2 <- model_results2$net.result
cor(predicted_size_category2, forest_test$size_category)


