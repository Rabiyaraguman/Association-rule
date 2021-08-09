#INSTALL PACKAGE:

library(plyr)
library(factoextra)

#IMPORT DATASET
mydata <- read.csv("C:/Users/Rafiya/Downloads/crime_data.csv")
  View(mydata)
head(mydata)
data<-is.na(mydata)
data
str(mydata)
crime<-mydata[,(2:5)]
View(crime)
str(crime)

#NORMALIZATION:
normalize<-scale(crime)
distance<-dist(crime,method="euclidean")
str(distance)
wss = (nrow(normalize)-1)*sum (apply(normalize, 2, var))
for (i in 1:5) wss[i] = sum(kmeans(normalize, centers=i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")  
title(sub = "K-Means Clustering Scree-Plot")
fit <- eclust(normalize, "kmeans", k = 4, nstart = 25, graph = FALSE) 
fviz_cluster(fit, geom = "point", frame.type = "norm")
final1<- data.frame(fit$cluster,crime) 
View(final1)
aggregate(crime, by=list(fit$cluster), FUN=mean)
table(fit$cluster)

# k clustering alternative for large dataset - Clustering Large Applications (Clara)

install.packages("cluster")
library(cluster)
xds <- rbind(cbind(rnorm(5000, 0, 8), rnorm(5000, 0, 8)), cbind(rnorm(5000, 50, 8), rnorm(5000, 50, 8)))
xds
xcl <- clara(xds, 2, sample = 100)
clusplot(xcl)

#Partitioning around medoids
xpm <- pam(xds, 2)
clusplot(xpm)
