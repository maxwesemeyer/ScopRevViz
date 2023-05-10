#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("tm")
library(tm)
library(ggplot2)
library(dplyr)
#install.packages('Rcpp')
library(Rcpp)

setwd("D:/ForLand/2_coauthor_network")
getwd()

extraction_all <- read.csv('input/Extraction_sheet_all.csv', sep=';', encoding = 'UTF-8')
################################################################################
# ABSTRACT
text_1 <- extraction_all$Abstract 
docs <- Corpus(VectorSource(text_1))
inspect(docs)
################################################################################
# cleaning
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

toEmpty <- content_transformer(function (x , pattern ) gsub(pattern, "", x))

#docs <- tm_map(docs, toEmpty)
docs <- tm_map(docs, toEmpty, "€")
docs <- tm_map(docs, toEmpty, "â")
docs <- tm_map(docs, toEmpty, "™")
docs <- tm_map(docs, toEmpty, "iii)")
docs <- tm_map(docs, toEmpty, "ii)")
docs <- tm_map(docs, toEmpty, "i)")
docs <- tm_map(docs, toEmpty, "also")
docs <- tm_map(docs, toEmpty, "˜")
docs <- tm_map(docs, toEmpty, "¦")
docs <- tm_map(docs, toEmpty, "–")
docs <- tm_map(docs, toEmpty, "RQ")

docs = tm_map(docs, content_transformer(tolower)) # lowercase 
docs = tm_map(docs, removeNumbers) # numbers
docs = tm_map(docs, stripWhitespace) # extra white space
docs <- tm_map(docs, removePunctuation) # punctuation
docs <- tm_map(docs, removeWords, stopwords(kind='en')) # common "stop words"
################################################################################
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
df <- df[which(df$freq >=5),]
write.csv(df, 'output/tables/WordCloud_Abstract.csv')
set.seed(12345)

jpeg("output/figures_plots/WordCloud_Abstract_15.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

wordcloud(words = df$word, freq = df$freq, min.freq = 15,                 # min.freq anpassen, 1 oder 2
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()

jpeg("output/figures_plots/WordCloud_Abstract_10.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

wordcloud(words = df$word, freq = df$freq, min.freq = 10,                 # min.freq anpassen, 1 oder 2
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()

jpeg("output/figures_plots/WordCloud_Abstract_5.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

wordcloud(words = df$word, freq = df$freq, min.freq = 5,                 # min.freq anpassen, 1 oder 2
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()
################################################################################
# RESEARCH QUESTIONS
text_1 <- extraction_all$Research.question.s...aim.s...verbatim 

taboo_words <- c("study", "aim")

docs <- Corpus(VectorSource(text_1))
################################################################################
# cleaning
docs <- docs %>%
  tm_map(removeWords, taboo_words) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
toEmpty <- content_transformer(function (x , pattern ) gsub(pattern, "", x))

#docs <- tm_map(docs, toEmpty)
docs <- tm_map(docs, toEmpty, "€")
docs <- tm_map(docs, toEmpty, "â")
docs <- tm_map(docs, toEmpty, "™")
docs <- tm_map(docs, toEmpty, "iii)")
docs <- tm_map(docs, toEmpty, "ii)")
docs <- tm_map(docs, toEmpty, "i)")
docs <- tm_map(docs, toEmpty, "˜")
docs <- tm_map(docs, toEmpty, "¦")
docs <- tm_map(docs, toEmpty, "can")
docs <- tm_map(docs, toEmpty, "RQ")

docs = tm_map(docs, content_transformer(tolower)) # lowercase 
docs = tm_map(docs, removeNumbers) # numbers
docs = tm_map(docs, stripWhitespace) # extra white space
docs <- tm_map(docs, removePunctuation) # punctuation
docs <- tm_map(docs, removeWords, stopwords(kind='en')) # common "stop words"
################################################################################
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


jpeg("output/figures_plots/WordCloud_RQs.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 4,                 # min.freq anpassen, 1 oder 2
          max.words=150, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()

################################################################################
# Content related purpose
text_1 <- extraction_all$Content.related.purpose.of.IACS.data..verbatim 


docs <- Corpus(VectorSource(text_1))
################################################################################
# cleaning
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

toEmpty <- content_transformer(function (x , pattern ) gsub(pattern, "", x))

#docs <- tm_map(docs, toEmpty)
docs <- tm_map(docs, toEmpty, "€")
docs <- tm_map(docs, toEmpty, "â")
docs <- tm_map(docs, toEmpty, "™")
docs <- tm_map(docs, toEmpty, "iii)")
docs <- tm_map(docs, toEmpty, "ii)")
docs <- tm_map(docs, toEmpty, "i)")
docs <- tm_map(docs, toEmpty, "˜")
docs <- tm_map(docs, toEmpty, "¦")
docs <- tm_map(docs, toEmpty, "can")
docs <- tm_map(docs, toEmpty, "RQ")

docs = tm_map(docs, content_transformer(tolower)) # lowercase 
docs = tm_map(docs, removeNumbers) # numbers
docs = tm_map(docs, stripWhitespace) # extra white space
docs <- tm_map(docs, removePunctuation) # punctuation
docs <- tm_map(docs, removeWords, stopwords(kind='en')) # common "stop words"
################################################################################
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


jpeg("output/figures_plots/WordCloud_Content_rel_purp.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,                 # min.freq anpassen, 1 oder 2
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()
################################################################################
# Methodological purpose
text_1 <- extraction_all$Methodological.purpose.of.IACS.data..verbatim 

docs <- Corpus(VectorSource(text_1))
################################################################################
# cleaning
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
toEmpty <- content_transformer(function (x , pattern ) gsub(pattern, "", x))

#docs <- tm_map(docs, toEmpty)
docs <- tm_map(docs, toEmpty, "€")
docs <- tm_map(docs, toEmpty, "â")
docs <- tm_map(docs, toEmpty, "™")
docs <- tm_map(docs, toEmpty, "iii)")
docs <- tm_map(docs, toEmpty, "ii)")
docs <- tm_map(docs, toEmpty, "i)")
docs <- tm_map(docs, toEmpty, "˜")
docs <- tm_map(docs, toEmpty, "¦")
docs <- tm_map(docs, toEmpty, "can")
docs <- tm_map(docs, toEmpty, "RQ")

docs = tm_map(docs, content_transformer(tolower)) # lowercase 
docs = tm_map(docs, removeNumbers) # numbers
docs = tm_map(docs, stripWhitespace) # extra white space
docs <- tm_map(docs, removePunctuation) # punctuation
docs <- tm_map(docs, removeWords, stopwords(kind='en')) # common "stop words"
################################################################################
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


jpeg("output/figures_plots/WordCloud_Method_rel_purp.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,                 # min.freq anpassen, 1 oder 2
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()