library(ggplot2)
library(dplyr)
library(stringr)
library(kableExtra)
library(magick)
# webshot::install_phantomjs()

################################################################################
# numbers
"""
Volltexte gescreent ohne Länder: 402
Volltexte gescreent mit Ländern: 409

Volltexte Excluded: 129 + 22
Volltexte Included: 280 (7 aus Countries Liste) (2 Paper = Duplicate) == 278

duplicates are: Cost of best management practices to combat agricultural...
Does land fragmentation...

# EXclude 3 additional papers after charting of the 60 papers: They don't use IACS
•['Schmitzberger,I', 'Wrbka, Th.', 'Steurer, B', 'Aschenbrenner, G', 'Peterseil, J','Zechmeister, H G']How farming styles influencebiodiversity maintenance in Austrian agricultural landscapes2005Agriculture, Ecosystems &Environment

• ['Lawson, Gerry', 'Dupraz, Christian','Watté, Jeroen']Chapter 9 - Can Silvoarable Systems MaintainYield, Resilience, and Diversity in the Face of Changing Environments?2019
['Quendler, Erika']","WOMEN'S ROLES IN AUSTRIAN AGRICULTURE",2017,"Poljoprivreda i Sumarstvo""


"""

################################################################################
setwd("D:/ForLand/2_coauthor_network")
getwd()

# read the csv including the exclusion to extract the exclusion reason
paper = read.csv('input/papers_and_exlusions.csv', sep=',', encoding="UTF-8")
################################################################################
# Change exclusion.yes.no to "yes" for the three papers we excluded in the chartig process

paper[which(paper$authors=="['Quendler, Erika']" | 
              paper$authors=="['Lawson, Gerry', 'Dupraz, Christian', 'Watté, Jeroen']" |
              paper$authors=="['Schmitzberger, I', 'Wrbka, Th.', 'Steurer, B', 'Aschenbrenner, G', 'Peterseil, J', 'Zechmeister, H G']"),"exclusion.yes.no"] <- "yes"

################################################################################
# Change exclusion.yes.no to "no" for the four papers we included again, when checking
# exlusion reasons (15.12.22)

paper[which(paper$doi.url=="https://doi.org/10.1016/j.ecolecon.2021.107044"  |  
              paper$doi.url=="http://dx.doi.org/10.17221/143/2014-SWR"  | 
              paper$doi.url=="https://doi.org/10.1016/j.agee.2016.09.025" |       
              paper$doi.url=="http://dx.doi.org/10.3390/rs11020118"),"exclusion.yes.no"] <- "no"

################################################################################

#paper <- paper[which(paper$exclusion.yes.no=='yes'),]
# 155 (after charting: 158) (after checking exclusion reasons 154)
########
# exclude the ones Heidi included again...
ex_list = c("['Bogaerts, Theo', 'Williamson, Ian P', 'Fendel, Elfriede M']",
            "['Inan, Hall Ibrahim', 'Sagris, Valentina', 'Devos, Wim', 'Milenov, Pavel', 'van Oosterom, Peter', 'Zevenbergen, Jaap']",
            "['Sládková, J']",
            "['Neumann, Barbara', 'Lütz, Michael', 'Schüpbach, Beatrice', 'Szerencsits, Erich']")
paper[which(paper$authors %in% ex_list),"exclusion.yes.no"] <- "no"
# 151 (after charting: 154) (after checking exclusion reasons 150)

exluded_countries <- read.csv('C:/Users/Max/Documents/side_projects/scoping_review_invekos/Eigene Arbeit_2/countries_excluded_MW.csv', sep=';', encoding="UTF-8")
#exluded_countries <- na.omit(exluded_countries)
exluded_countries <- exluded_countries[which(exluded_countries$exclusion.yes.no=='yes'),]
excl_list <- which(paper$authors %in% exluded_countries$authors)
paper[excl_list,"exclusion.yes.no"] <- "no"

# 129 (after charting: 132) (after checking exclusion reasons 128)
################################################################################
paper %>% group_by(exclusion.yes.no) %>% summarise(n = n())
paper <- paper[which(paper$exclusion.yes.no=='yes'),]
################################################################################
# Manual reclassification of exlusion reasons before grouping
unique(paper$exclusion..reason)
paper[which(paper$exclusion..reason=="double" ), 'exclusion..reason'] <- 'duplicate'
paper[which(paper$exclusion..reason=="country, data"), 'exclusion..reason'] <- 'country'
paper[which(paper$exclusion..reason=="data, country"), 'exclusion..reason'] <- 'country'
paper[which(paper$exclusion..reason=="country (auf basis des titels, volltext nicht greifbar)"), 'exclusion..reason'] <- 'country'
paper[which(paper$exclusion..reason=="wrong country"), 'exclusion..reason'] <- 'country'
paper[which(paper$exclusion..reason=="Abstract in english - Paper in German"), 'exclusion..reason'] <- 'language'
paper[which(paper$exclusion..reason=="dataset, country"), 'exclusion..reason'] <- 'country'
paper[which(paper$exclusion..reason=="Study area is in Hungary"), 'exclusion..reason'] <- 'country'
paper[which(paper$exclusion..reason=="Study area is in Spain"), 'exclusion..reason'] <- 'country'
paper[which(paper$exclusion..reason=="Study area is in Finland"), 'exclusion..reason'] <- 'country'
paper[which(paper$exclusion..reason=="text is in Portuguese; study area in Brazil"), 'exclusion..reason'] <- 'country'
paper[which(paper$exclusion..reason=="study area is in Spain"), 'exclusion..reason'] <- 'country'
paper[which(paper$exclusion..reason=="study area is in Eastern Poland"), 'exclusion..reason'] <- 'country'
paper[which(paper$exclusion..reason=="study area is in Scotland"), 'exclusion..reason'] <- 'country'

paper[which(paper$exclusion..reason=="no IACS data used" ), 'exclusion..reason'] <- "dataset"
paper[which(paper$exclusion..reason=="no usage of IACS only use of real LPIS"), 'exclusion..reason'] <- "dataset"
paper[which(paper$exclusion..reason=="scientific report; only one reference to IACS"), 'exclusion..reason'] <- "dataset"
paper[which(paper$exclusion..reason=="IACS data only used marginally" ), 'exclusion..reason'] <- "dataset"
paper[which(paper$exclusion..reason=="IACS data probably not used" ), 'exclusion..reason'] <- "dataset"
paper[which(paper$exclusion..reason=="no IACS data used, IAC's stand for Industries Assistance Comission's"), 'exclusion..reason'] <- "dataset"
paper[which(paper$exclusion..reason=="data" ), 'exclusion..reason'] <- "dataset"
paper[which(paper$exclusion..reason=="it is a review paper; they don't use LPIS" ), 'exclusion..reason'] <- "dataset"
paper[which(paper$exclusion..reason=="They just mention IACS in the conclusion"), 'exclusion..reason'] <- "dataset"
paper[which(paper$exclusion..reason=="no usage of IACS" ), 'exclusion..reason'] <- "dataset"

paper[which(paper$exclusion..reason=="no plot-level data is used"), 'exclusion..reason'] <- "plot-level"
paper[which(paper$exclusion..reason=="not plot"), 'exclusion..reason'] <- "plot-level"
paper[which(paper$exclusion..reason=="not retrievable; conference proceeding?"), 'exclusion..reason'] <- "not retrievable"

paper[which(paper$exclusion..reason=="? They collect many data sets from all over the world and combine them into one croland product"),'exclusion..reason'] <- "dataset"

paper[which(paper$exclusion..reason==""), 'exclusion..reason'] <- "dataset"
paper[which(paper$exclusion..reason=="discipline"), 'exclusion..reason'] <- "dataset"

paper[which(paper$exclusion..reason==""), 'doi.url']


################################################################################
# plot and final table about exclusion reasons
grouped <- paper %>% group_by(exclusion..reason) %>% summarise(n = n())
ordered <- grouped[order(grouped$n, decreasing = T),] 
ordered$exclusion..reason <- factor(ordered$exclusion..reason, levels=ordered$exclusion..reason)
ordered %>% ggplot(., aes(y=n, x=exclusion..reason)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,70,5)) +
  theme(axis.title.x=element_blank())

paper$exclusion..reason <- str_to_title(paper$exclusion..reason)
temp <- paper %>% group_by(exclusion..reason) %>% summarise(Count=n()) %>% arrange(desc(Count)) 
rbind(temp, data.frame(exclusion..reason='yes', Count=sum(temp$Count)))
rbind(temp, data.frame(exclusion..reason='no', Count=279))
sum(temp$Count)
paper %>% group_by(exclusion..reason) %>% summarise(Count=n()) %>% arrange(desc(Count)) %>%
  rename(., 'Exclusion reason' = 'exclusion..reason') %>% 
  kbl() %>%  
  #group_rows(group_label = c('labelo'), start_row = c(1), end_row = c(4)) %>%
  #group_rows(group_label = c('labeloss'), start_row = c(5), end_row = c(6)) %>%
  kable_classic_2(full_width=FALSE) %>% 
  save_kable("output/tables_allMeta/exclusion_r_table.png", zoom = 5)


################################################################################
# find out which 2 Papers are missing in test_corrected_Heidi_included_utf8.txt
paper2 = read.csv('input/Meta_all/test_corrected_Heidi_included_utf8.txt', sep=';', encoding="UTF-8")
paper2 <- paper2[,c(6)]
sum(duplicated(paper2$doi.url))
#colnames(paper2)[1] <- "X"



paper = read.csv('input/Meta_all/papers_and_exlusions.csv', sep=',', encoding="UTF-8")
paper <- paper[which(paper$exclusion.yes.no=='no'),]
paper <- paper[duplicated(paper$doi.url),]

#paper <- paper[,c(6)]
# 1 Duplictae 
sum(duplicated(paper$doi.url))

setdiff <- setdiff(paper, paper2)
paper[which(paper$doi.url %in% setdiff),]

paper_merged = c(paper, paper2)
sum(!duplicated(paper_merged))
paper_merged[!duplicated(paper_merged),]
test <- paper_merged[!duplicated(paper_merged['title']),]

################################################################################
paper = read.csv('input/Meta_all/test_corrected_Heidi_included_utf8.txt', sep=';', encoding="UTF-8")
paper$exclusion..reason
set.seed(73829)
paper[which(paper$Journal=="Remote Sensing"),"categories_scimagojr"]
paper[which(paper$Journal=="Remote Sensing"),"categories_scimagojr_corrected"]
paper$Journal <- tolower(paper$Journal)
paper$Journal <- str_to_title(paper$Journal)

####################################
#include these papers again that had no exclusion reason and were checked
# again at 15.12.22
# "https://doi.org/10.1016/j.ecolecon.2021.107044"    
# "http://dx.doi.org/10.17221/143/2014-SWR"   
# "https://doi.org/10.1016/j.agee.2016.09.025"       
# "http://dx.doi.org/10.3390/rs11020118"         

paper_all = read.csv('input/Meta_all/papers_and_exlusions.csv', sep=',', encoding="UTF-8")
include <- paper_all[which(paper_all$doi.url == "https://doi.org/10.1016/j.ecolecon.2021.107044" ),]
include <- rbind(include, paper_all[which(paper_all$doi.url == "http://dx.doi.org/10.17221/143/2014-SWR"  ),])
include <- rbind(include, paper_all[which(paper_all$doi.url == "https://doi.org/10.1016/j.agee.2016.09.025" ),])
include <- rbind(include, paper_all[which(paper_all$doi.url == "http://dx.doi.org/10.3390/rs11020118"),])
include$authors_corrected <- c("['Karner, Katrin'; 'Schmid, Erwin'; 'Schneider, Uwe A'; 'Mitter, Hermine']",
                               "['Podhrázská, J'; 'Kučera, J'; 'Karásek, P'; 'Konečná, J']",
                               "['Lundin, Ola'; 'Rundlöf, Maj'; 'Smith, Henrik G'; 'Bommarco, Riccardo']",
                               "['Demarez, Valérie'; 'Florian, Helen'; 'Marais-Sicre, Claire'; 'Baup, Frédéric']")
include$categories_scimagojr <- c("Economics and Econometrics (Q1); Environmental Science (miscellaneous) (Q1)",
                                  "Aquatic Science (Q2); Soil Science (Q2)",
                                  "Agronomy and Crop Science (Q1); Animal Science and Zoology (Q1); Ecology (Q1)",
                                  "Earth and Planetary Sciences (miscellaneous) (Q1)")
include$categories_scimagojr_corrected <- c("['Economics and Econometrics', ' Environmental Science (miscellaneous)']",
                                            "['Aquatic Science', ' Soil Science']",
                                            "['Agronomy and Crop Science', ' Animal Science and Zoology', ' Ecology']",
                                            "['Earth and Planetary Sciences (miscellaneous)']")
colnames(include) <- colnames(paper)
include$exclusion.yes.no <- 'no'

paper <- rbind(paper, include)
####################################
# remove papers that have been removed during the Charting process of the 60 Papers
# 1 of Stefan, 1 of Andreas, 1 of Franziska
paper <- paper[-which(paper$authors=="['Quendler, Erika']" | 
                        paper$authors=="['Lawson, Gerry', 'Dupraz, Christian', 'Watté, Jeroen']" |
                        paper$authors=="['Schmitzberger, I', 'Wrbka, Th.', 'Steurer, B', 'Aschenbrenner, G', 'Peterseil, J', 'Zechmeister, H G']"),]

#paper <- paper %>% sample_n(65)
####################################
# remove papers from sample that have already been charted
"""
unique(paper$authors)
paper <- paper[-which(paper$authors=="['Kirchweger, Stefan', 'Clough, Yann', 'Kapfer, Martin', 'Steffan-Dewenter, Ingolf', 'Kantelhardt, Jochen']" |
        paper$authors == "['Leonhardt, Heidi', 'Penker, Marianne', 'Salhofer, Klaus']" |
        paper$authors == "['Wolff, Saskia', 'Huttel, Silke', 'Nendel, Claas', 'Lakes, Tobia']" |
        paper$authors == "['Sklenicka, Petr', 'Salek, Miroslav']"  |
          paper$authors == "['Kyere, Isaac']" ),]
"""
#paper <- paper[which(paper$year==2011 |paper$year==2016 | paper$year==2021),]
################################################################################
# split up the category string
categories = c()
years = c()
for(row in 1:nrow(paper)){
  cat = paper[row, "categories_scimagojr"]
  year = paper[row, "year"]
  splitted = str_split(cat, ';')
  for(index in 1:length(splitted[[1]])) {
    #print(string)
    cleaned = str_replace_all(splitted[[1]][index], "[[:punct:]]", "")
    #cleaned = str_replace_all(cleaned, " *", "")
    cleaned = str_squish(cleaned)
    cleaned = substr(cleaned,1,nchar(cleaned)-2)
    years = c(years, year)
    categories = c(categories, cleaned)
    #print(cleaned)
    if(cleaned == "and Optics"){
      print(paste(index, splitted))
      print(cat)
    }
  }
}

cat_years <- data.frame('year' = years, 'cat' = categories)
grouped <- cat_years %>% group_by(cat, year) %>% summarise(n = n())


grouped <- cat_years %>% group_by(cat) %>% summarise(n = n())
ordered <- grouped[order(grouped$n, decreasing = T)[1:32],] 
ordered <- ordered[-which(ordered$cat=='no mat'),]
ordered$cat <- factor(ordered$cat, levels=ordered$cat)


jpeg("output/figures_plots_allMeta/categories.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

ordered %>% ggplot(., aes(y=n, x=cat)) + geom_boxplot() + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,70,5)) + ylab('count') + 
  theme(axis.title.x=element_blank())

dev.off()


###############
# most frequent category per year
years = c()
categories_ <- c()
n <- c()
for(year in unique(grouped$year)){
  sub = grouped[which(grouped$year == year),]
  sub <- sub[which(sub$n == max(sub$n)), ]
  for(row in nrow(sub)) {
    categories_ <- c(categories_, sub[row,"cat"])
    years <- c(years, year)
    n <- c(n, max(sub$n))
  }
}
most_freq_cat_year <- data.frame("year"=years, "n"=n, "cat"=categories_)

###############


grouped %>% filter(cat=="Ecology " | cat=="Nature and Landscape Conservation " | 
                     cat == "Geography Planning and Development " | 
                     cat=="Agronomy and Crop Science ") %>% ggplot(., aes(x=year, y=n, col=cat)) + geom_line()


grouped <- cat_years %>% group_by(cat) %>% summarise(n = n())
ordered <- grouped[order(grouped$n, decreasing = T)[1:30],] 
ordered$cat <- factor(ordered$cat, levels=ordered$cat)
ordered %>% ggplot(., aes(y=n, x=cat)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,70,5)) +
  theme(axis.title.x=element_blank())


ggplot(grouped, aes(y=n, x=cat)) + geom_boxplot() + facet_wrap(~year) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,70,5)) +
  theme(axis.title.x=element_blank())

cts <- data.frame(table(as.factor(categories)))



################## 
# most frequent journals

cts <- data.frame(table(as.factor(paper$Journal)))


cat_years <- data.frame('year' = paper$year, 'cat' = paper$Journal)
grouped <- cat_years %>% group_by(cat, year) %>% summarise(n = n())
grouped <- cat_years %>% group_by(cat) %>% summarise(n = n())
grouped[grouped$cat == 'International Journal Of Applied Earth Observation And Geoinformation', "cat"] = "Int. J. Appl. Earth Obs. Geoinf."
grouped[grouped$cat == 'Environmental Monitoring And Assessment', "cat"] = "Environmental Monitoring & Assessment"

ordered <- grouped[order(grouped$n, decreasing = T),]
ordered <- ordered[which(ordered$n > 2),]
ordered$cat <- factor(ordered$cat, levels=ordered$cat)

jpeg("output/figures_plots_allMeta/most_freq_journals_ct_bigger_2.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

ordered %>% ggplot(., aes(y=n, x=cat)) + geom_boxplot() + #theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(axis.title.x=element_blank()) + ylab('count')

dev.off()

###############

# TODO FInd the most frequent category per year
# TODO Do the same for the Journal
################################################################################
# split up the author string
# FIrst do some corrections to the papers where the corrected author field is empty
paper[paper$authors==
        "['Latruffe, Laure', 'Piet, Laure']","authors_corrected"] <- "['Latruffe, L'; 'Piet, L']"

paper[paper$authors== "['Delattre, Laurence', 'Debolini, Marta', 'Paoli, Jean Christophe', 'Napoleone, Claude', 'Moulery, Michel', 'Leonelli, Lara', 'Santucci, Pierre']", "authors_corrected"] <-
  "['Delattre, L'; 'Debolini, M'; 'Paoli, J C'; 'Napoleone, C'; 'Moulery, M'; 'Leonelli, L'; 'Santucci, P']"

paper[paper$authors== "['MURGUE  Romain ; Vavasseur, Maroussia ; Burger-Leenhardt, Delphine ; Therond, Olivier, Clément ; Lardy']", "authors_corrected"] <-
  "['Murgue, Clément'; 'Lardy, Romain';'Vavasseur, Maroussia'; 'Burger-Leenhardt, Delphine D.'; 'Therond, Olivier']"


paper[paper$authors== "['Rivers-Moore, Justine', 'Andrieu, Emilie', 'Vialatte, Aude', 'Ouin, Annie']", "authors_corrected"] <-
  "['Rivers-Moore, Justine'; 'Andrieu, Emilie'; 'Vialatte, Aude'; 'Ouin, Annie']"


paper[paper$authors=="['Vincente-Vincente, Jose Luis', 'Sanz-Sanz, Esther', 'Napoléone, Claude', 'Moulery, Michel', 'Piorr, Annette']", "authors_corrected"] <-
  "['Vincente-Vincente, Jose Luis'; 'Sanz-Sanz, Esther'; 'Napoléone, Claude'; 'Moulery, Michel'; 'Piorr, Annette']"

################################################################################
authors = c()
years = c()
for(row in 1:nrow(paper)){
  
  cat = paper[row, "authors_corrected"]
  #print(cat)
  year = paper[row, "year"]
  splitted = str_split(cat, ',')
  #print(splitted)
  for(index in 1:length(splitted[[1]])) {
    #print(string)
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

jpeg("output/figures_plots_allMeta/authors_count_29.jpeg", 
     
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



################################################################################
# Years of publication
jpeg("output/figures_plots_allMeta/year_pub.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

ggplot(paper, aes(x=year)) + geom_histogram(bins=39) +
  scale_x_continuous(breaks=2002:2021) + theme_bw()

dev.off()

#paper$year <- as.factor(paper$year)
table(paper$year)

#paper$country <- as.factor(paper$country)
c <- data.frame(table(paper$country))
table(paper$country) %>% sort()


paper[which(paper$country != 'DE' & paper$country != 'FR' & paper$country != 'SE' & paper$country != 'CZ' & paper$country != 'AT'),"country"] = 'other'

counted_yr_country <- paper %>% group_by(year, country) %>% summarise(ct = n())


jpeg("output/figures_plots_allMeta/year_pub_country.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

counted_yr_country$country <- factor(counted_yr_country$country, 
                                     levels = c("AT", "CZ", "DE", "FR", "SE", "other"))

ggplot(counted_yr_country, aes(fill=country, y=ct, x=year)) + 
  scale_x_continuous(breaks=2002:2021) +
  geom_bar(position="stack", stat="identity") + 
  geom_col(position = "stack") +
  ylab("count") + theme_bw()

dev.off()
################################################################################
# Counts per country

paper %>% group_by(country) %>% summarise(count=n()) %>% arrange(desc(count)) %>%
  kbl() %>% kable_classic_2(full_width=FALSE) %>% 
  save_kable("output/tables_allMeta/country_cts.png", zoom = 5)

