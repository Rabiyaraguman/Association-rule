setwd("E:\\New folder")
library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews #############################
aurl <- "http://www.amazon.in/Honor-6X-Grey-32GB/product-reviews/B01FM7JGT6/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pageNumber=1"


amazon_reviews <- NULL
for (i in 1:5){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))  # Use html()
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
  View (amazon_reviews)
}
write.table(amazon_reviews,"rafiya.txt",row.names = F)
getwd()

setwd( "E:/New folder")
write.table(amazon_reviews,"rafiya.txt",row.names = F)
amazon_reviews <- read.delim('rafiya.txt')
str(amazon_reviews)
View(amazon_reviews)
#Build corpus and tdm&dtm
library(tm)
Corpus<-amazon_reviews[-1,]
head(Corpus)
Corpus<-Corpus(VectorSource(Corpus))
inspect(Corpus[1:5])

#Clean the text
Corpus<-tm_map(Corpus,tolower)
inspect(Corpus[1:5])
Corpus<-tm_map(Corpus,removePunctuation)
inspect(Corpus[1:5])
Corpus<-tm_map(Corpus,removeNumbers)
inspect(Corpus[1:5])
Corpus<-tm_map(Corpus,stripWhitespace)
inspect(Corpus[1:5])
Corpus<-tm_map(Corpus,tolower)
inspect(Corpus[1:5])
cleanset<-tm_map(Corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords,c('good','phone'))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#tdm
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 
w <- rowSums(tdm)  
w <- subset(w, w>= 25) 
barplot(w, las = 2, col = rainbow(50))

# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 150,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'circle', rotateRatio = 0.5, minSize = 1)

# lettercloud 
letterCloud(w,word = "a",frequency(5), size=2)

# Sentiment Analysis for tweets:
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read File 
amazon_reviews <- read.delim('rafiya.txt')
reviews <- as.character(amazon_reviews[-1,])
class(reviews)

#Sentiment scores 
scores <- get_nrc_sentiment(reviews)
head(scores)
reviews[4]
get_nrc_sentiment("good")
get_nrc_sentiment("battery")

# barplot 
barplot(colSums(scores), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Amazon Reviews
        ')
