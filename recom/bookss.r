#import dataset
bookss <- read.csv("C:/Users/Rafiya/Downloads/bookss.csv")
 View(bookss)
 str(bookss)
library(recommenderlab) 
 library(caTools)
 
#histogram 
hist(bookss$rating) 
bookss_matrix <- as(bookss, 'realRatingMatrix')

#Popularity based 
bookss_recomm_model1 <- Recommender(bookss_matrix, method="POPULAR")

#Predictions
recommended_items1 <- predict(bookss_recomm_model1, bookss_matrix[1], n=4)
as(recommended_items1, "list")

#User Based Collaborative Filtering
bookss_recomm_model2 <- Recommender(bookss_matrix, method="UBCF")

#Predictions 
recommended_items2 <- predict(bookss_recomm_model2, bookss_matrix[1], n=4)
as(recommended_items2, "list")

