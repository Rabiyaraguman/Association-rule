setwd("E:\\New folder")
library(rvest)
library(XML)
library(magrittr)
aurl<-"https://www.amazon.in/Smooth-Touch-Matte-Shell-MacBook/product-reviews/B0177UN6WO/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1"
apple_reviews<-NULL
for (i in 1:10) {
  murl<-read_html(as.character(paste(aurl,i,sep = "=")))
  rev<-murl %>% 
  html_nodes(".review-text") %>% 
  html_text()
  apple_reviews<-c(apple_reviews,rev)
}
length(apple_reviews)
write.table(apple_reviews,"ple.txt",row.names = F)
getwd()


library("tm")
library("SnowballC")
library("wordcloud")
library(RColorBrewer)
library("textstem")
x<-as.character(apple_reviews)
x<-iconv(x,"UTF-8")
x<-Corpus(VectorSource(x))
inspect(x[1])
x1<-tm_map(x,tolower)
inspect(x1[1])
x1<-tm_map(x1,removeNumbers)
x1<-tm_map(x1,removePunctuation)
x1<-tm_map(x1,removeWords,stopwords('english'))
x1<-tm_map(x1,stripWhitespace)
inspect(x1[1])
x1<-lemmatize_words(x1)
x1<-tm_map(x1,stemDocument)
tdm<-TermDocumentMatrix(x1)
tdm<-as.matrix(tdm)
tdm
v<-sort(rowSums(tdm),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,5)


w<-rowSums(tdm)
w_sub<-subset(w,w>=10)
barplot(w_sub,las=3,col=rainbow(20))


x1<-tm_map(x1,removeWords,c('apple','air','laptop','can'))
x1<-tm_map(x1,stripWhitespace)
tdm<-TermDocumentMatrix(x1)
tdm<-as.matrix(tdm)
w1<-rowSums(tdm)


wordcloud(words = names(w1),freq = w1,random.order = F,colors = rainbow(20),scale = c(2,.2),rot.per = 0.4)

pos.words=scan(file.choose(),what = "character",comment.char = ";")
neg.words=scan(file.choose(),what = "character",comment.char = ";")
pos.words=c(pos.words,"wow","kudos","hurray")


pos.matches=match(names(w),c(pos.words))
pos.matches=!is.na(pos.matches)
freq_pos<-w[pos.matches]
p_names<-names(freq_pos)


wordcloud(p_names,freq_pos,scale = c(3.5,.2),colors = rainbow(20))


neg.matches=match(names(w),c(neg.words))
neg.matches=!is.na(neg.matches)
freq.neg<-w[neg.matches]
n_names<-names(freq.neg)

wordcloud(n_names,freq.neg,scale = c(3.5,.2),colors = rainbow(20))

tdm<-TermDocumentMatrix(x1)
findAssocs(tdm,c("cover"),corlimit = 0.3)
