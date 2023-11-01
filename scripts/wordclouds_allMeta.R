#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)
#install.packages("tm")
library(tm)
library(ggplot2)
library(dplyr)
#install.packages('Rcpp')
library(Rcpp)
library(kableExtra)


# use the correction script by Paula 
#paper = read.csv('input/Meta_all/papers_and_exlusions.csv', sep=',', encoding="UTF-8")
paper$abstract

################################################################################
# ABSTRACT
text_1 <- paper$abstract
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
docs <- tm_map(docs, toEmpty, "also")
docs <- tm_map(docs, toEmpty, "˜")
docs <- tm_map(docs, toEmpty, "¦")
docs <- tm_map(docs, toEmpty, "–")
docs <- tm_map(docs, toEmpty, "RQ")
docs <- tm_map(docs, toEmpty, "can")
docs <- tm_map(docs, toEmpty, "due")
docs <- tm_map(docs, toEmpty, "der")
docs <- tm_map(docs, toEmpty, "may")


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
set.seed(12345)
df <- df[which(df$freq >=50),]
write.csv(df, 'output/tables_allMeta/WordCloud_Abstract.csv')


jpeg("output/figures_plots_allMeta/WordCloud_Abstract_100.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")
wordcloud(words = df$word, freq = df$freq, min.freq = 100,                 # min.freq anpassen, 1 oder 2
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()

jpeg("output/figures_plots_allMeta/WordCloud_Abstract_75.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

wordcloud(words = df$word, freq = df$freq, min.freq = 75,                 # min.freq anpassen, 1 oder 2
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()


jpeg("output/figures_plots_allMeta/WordCloud_Abstract_50.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

wordcloud(words = df$word, freq = df$freq, min.freq = 50,                 # min.freq anpassen, 1 oder 2
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()



################################################################################
# Title
text_1 <- paper$title
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
df <- df[which(df$freq >=7),]
write.csv(df, 'output/tables_allMeta/WordCloud_Title.csv')

df %>% select("freq") %>% kbl() %>% kable_paper(full_width = F)




jpeg("output/figures_plots_allMeta/WordCloud_Title_15.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")
wordcloud(words = df$word, freq = df$freq, min.freq = 15,                 # min.freq anpassen, 1 oder 2
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()

jpeg("output/figures_plots_allMeta/WordCloud_Title_10.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

wordcloud(words = df$word, freq = df$freq, min.freq = 10,                 # min.freq anpassen, 1 oder 2
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()


jpeg("output/figures_plots_allMeta/WordCloud_Title_7.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

wordcloud(words = df$word, freq = df$freq, min.freq = 7,                 # min.freq anpassen, 1 oder 2
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dev.off()


