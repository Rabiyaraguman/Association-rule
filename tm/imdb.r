setwd("E:\\New folder")
library(rvest)
library(XML)
library(magrittr)

# IMDBReviews #############################
aurl <- "https://www.imdb.com/title/tt6139732/?ref_=tt_urv"
IMDB_reviews <- NULL
for (i in 0:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)



write.table(IMDB_reviews,file="ww.txt")
getwd()


write.table(IMDB_reviews,"ww.txt",row.names = F)
getwd()

woman <- read.delim('ww.txt')
str(woman)
View(woman)
library(tm)
corpus <- woman[-1,]
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('can','film'))
cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
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

