library(ggplot2)
library(dplyr)
library(stringr)
library(kableExtra)
library(magick)

#authors with the highest number of papers

authors = c()
years = c()

for(row in 1:nrow(paper)){
  
  cat = paper[row, "authors_corrected"]
  #print(cat)
  year = paper[row, "year"]
  splitted = str_split(cat, ',')
  #print(splitted)
  for(index in 1:length(splitted[[1]])) {
    cleaned = str_replace_all(splitted[[1]][index], "[[:punct:]]", "")
    #cleaned = str_replace_all(cleaned, " *", "")
    cleaned = str_squish(cleaned)
    #print(cleaned)
    if(cleaned=="Schmid E") {
      print(paper[row,])
    }
    #cleaned = substr(cleaned,1,nchar(cleaned)-2)
    years = c(years, year)
    authors = c(authors, cleaned)
    #print(cleaned)
    if(cleaned == "and Optics"){
      #print(paste(index, splitted))
      #print(cat)
    }
  }
}

author_df <- data.frame('year' = years, 'cat' = authors)
grouped <- author_df %>% group_by(cat) %>% summarise(n = n())
ordered <- grouped[order(grouped$n, decreasing = T)[1:29],] 
ordered$cat <- factor(ordered$cat, levels=ordered$cat)

jpeg("output/authors_count_29.jpeg", 
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     type = "cairo")

ordered %>% ggplot(., aes(y=n, x=cat)) + geom_boxplot() + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,40,5)) + ylab("count") + 
  theme(axis.title.x=element_blank()) 

dev.off()
#,
#axis.text.x=element_blank(),
#axis.ticks.x=element_blank())