#INSTALL PACKAGES

library(cluster)
library(factoextra)
library(plyr)
library(fpc)
library(dendextend)

#IMPORT DATASET

EastWestAirlines <- read.csv(file.choose())
View(EastWestAirlines)
head(EastWestAirlines)

#NORMALIZATION:

normalize<-scale(EastWestAirlines[,2:12])
#Distance matrix
distance<-dist(normalize,method = "euclidean")

#methods used
hcl_ward.D2<-hclust(distance,method = "ward.D2")
hcl_complete<-hclust(distance,method = "complete")
hcl_single<-hclust(distance,method = "single")
#used dendrogram object

hcl_ward.D2<-as.dendrogram(hcl_ward.D2)
hcl_complete<-as.dendrogram(hcl_complete)
hcl_single<-as.dendrogram(hcl_single)
cd=color_branches(hcl_ward.D2,k=3)
cd1=color_branches(hcl_complete,k=4)
cd2=color_branches(hcl_single,k=3)
plot(cd)
plot(cd1)
plot(cd2)
plot(hcl_ward.D2,hang=-1)
groups<-cutree(hcl_ward.D2,k=3)
table(groups)
membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(EastWestAirlines, membership)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

final1
Fin <- aggregate(EastWestAirlines[,-1],by=list(final$membership),mean)

Fin

