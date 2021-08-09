#INSTALL PACKAGES

library(cluster)
library(factoextra)
library(fpc)
library(dendextend)

#IMPORT DATASET

crime <- read.csv("C:/Users/Rafiya/Downloads/crime_data.csv")
View(crime)

#normalize data

normalized_data<-scale(crime[,2:5]) #excluding the ID from spreadsheet
# distance matrix
d <- dist(normalized_data, method = "euclidean") # distance matrix
d
# cluster method
fit <- hclust(d, method="complete")
fit1<-hclust(d,method = "single")  

#converting dendogram object
fit <- as.dendrogram(fit)
fit1<-as.dendrogram(fit1)
cd = color_branches(fit,k=4) 
cd1=color_branches(fit1,k=4)
plot(cd)
plot(cd1)
plot(fit,hang=-1)
plot(fit1, hang=-1)  
# rect.hclust(fit, k=2, border="red")
groups <- cutree(fit, k=4) # cut tree into 4 clusters
  
table(groups)  

Crime_Rate_Categories<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime, Crime_Rate_Categories)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

View(final1)
aggregate(crime[,-1],by=list(final$Crime_Rate_Categories),mean)

  