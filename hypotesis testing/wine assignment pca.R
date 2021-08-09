##PRINCIPLE COMPONENT ANALYSIS

#IMPORT DATASET
wine <- read.csv("C:/Users/Rafiya/Downloads/wine.csv")
   View(wine)
   View(wine[-1])
data<-wine[,-1]
attach(data)
cor(data)
?princomp

#PCA MODEL
pca<-princomp(wine,cor=TRUE,scores = TRUE,covmat = NULL)
summary(pca)
loadings(pca)
str(pca)
plot(pca)
biplot(pca)
plot(cumsum(pca$sdev*pca$sdev)*100/(sum(pca$sdev*pca$sdev)),type = "b")
pca$scores
pca$scores[,1:3]

#Considering top 3 principal component scores and binding them with mydata


wine<-cbind(wine,pca$scores[,1:3])
View(wine)
cluster<-wine[,8:10]

#NORMALIZATION the data
norm<-scale(cluster)
dist1<-dist(norm,method="euclidean")

# Clustering the data using hclust function --> Hierarchical CLUSTRING
fit1<-hclust(dist1,method = "complete")
plot(fit1)
rect.hclust(fit1,k=7,border = "green")
group<-cutree(fit1,7)

#MEMBERSHIP FUNCTION
membership<-as.matrix(group)
View(membership)
# binding column wise with orginal data
final<-cbind(membership,wine)
View(final)
View(aggregate(final[,-c(2,16,18)],by=list(membership),FUN=mean))
write.csv(final,file = "wine.csv",row.names = F,col.names = F)  
getwd()

#K-MEANS CLUSTRING :
library(plyr)
library(factoextra)
wine<-read.csv(file.choose())
str(wine)
View(wine)
norm1<-scale(wine[,15:17])
wss=(nrow(norm1)-1)*sum(apply(norm1,2,var))
for (i in 1:7)wss[i]=sum(kmeans(norm1,centers = i)$withinss) 
plot(1:7,wss,type = "b",xlab="NO.OF.CLUSTER",ylab = "GROUP SUM OF SQUARE")  
title(sub="k MEANS CLUSTRING SCREEN PLOT")
fit <- eclust(norm1, "kmeans", k = 7, nstart = 25, graph = FALSE) 
fviz_cluster(fit, geom = "point", frame.type = "norm")
final1<- data.frame(fit$cluster,wine) 
View(final1)
aggregate(wine[,2:17], by=list(fit$cluster), FUN=mean)
table(fit$cluster)
