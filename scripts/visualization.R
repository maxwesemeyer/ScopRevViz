################################################################################
library(ggplot2)
library(dplyr)
library(stringr)#
library(kableExtra)

extraction_all <- read.csv('input/all_extracted_Heidis_Nacharbeit_.csv', sep=',', encoding = 'UTF-8')
#indicators_all <- read.csv('input/indicators_all.csv', sep=';', encoding = 'UTF-8')
#datasets_all <- read.csv('input/datasets_all.csv', sep=';', encoding = 'UTF-8')
country_c <- paper[paper$doi.url %in% extraction_all$doi, "country"]
# two papers not in doi; add countries manually
country_c <- c(country_c, "DE")
country_c <- c(country_c, "FR")
table(country_c)
extraction_all[!(extraction_all$doi %in% paper$doi.url), 3]


correct_meta_sample <- paper[paper$doi.url %in% extraction_all$doi,]
correct_meta_sample <- rbind(correct_meta_sample, paper[grep(paper$title, pattern="Spatial configuration and landscape context"),])
correct_meta_sample <- rbind(correct_meta_sample, paper[grep(paper$title, pattern="A Two-Branch CNN Architecture for Land Cover "),])

####################################
# find water and medical papers

water_sub <- extraction_all[str_detect(extraction_all$Title, 'ater'),]
health_sub <- extraction_all[str_detect(extraction_all$Abstract, 'ealth'),]
health_sub <- extraction_all[str_detect(extraction_all$Abstract, 'edicine'),]


################################################################################
# some preprocessing of the Dataset
# Thematic Information extracted
thematic_information <- c()


for(i in 1:nrow(extraction_all)) {
  x <- extraction_all[i, "Raw.info.used..thematic..other..crop.type..land.use.type..LSE..organic..AES.....separate.multiple.entries.by...."]
  matched <- FALSE
  if(grepl(',', x) == TRUE) {
    splitted <- strsplit(x, ', ')
    matched <- TRUE
  }
  if(grepl(';', x) == TRUE) {
    splitted <- strsplit(x, '; ')
    matched <- TRUE
  }
  else if(matched == FALSE) {
    splitted <- x
  }
  print(splitted)
  print(length(splitted[[1]]))
  for(len in 1:length(splitted[[1]])) {
    thematic_information <- c(thematic_information, splitted[[1]][len])
  }
}
thematic_information[thematic_information == 'unclear'] = 'None'
thematic_information[thematic_information == ''] = 'None'
thematic_information[thematic_information == 'irrigation (not clear if from LPIS)']= 'irrigation*'




table(thematic_information) %>% 
  as.data.frame() %>% rename(., 'count'= "Freq", 
                             'thematic information' = 'thematic_information') %>%
  arrange(desc(count)) %>%
  kbl() %>% kable_classic_2(full_width=FALSE) %>% 
  footnote("* not clear if from LPIS") %>%
  save_kable("output/tables/thematic_inf.png", zoom = 5)

################################################################################
# Spatial.unit.s..of.analysis string split by ;
spatial_unit_analysis <- c()


for(i in 1:nrow(extraction_all)) {
  x <- extraction_all[i, "Spatial.unit.s..of.analysis"]
  matched <- FALSE
  if(grepl(',', x) == TRUE) {
    splitted <- strsplit(x, ', ')
    matched <- TRUE
  }
  if(grepl(';', x) == TRUE) {
    splitted <- strsplit(x, '; ')
    matched <- TRUE
  }
  else if(matched == FALSE) {
    splitted <- x
  }
  print(splitted)
  print(length(splitted[[1]]))
  for(len in 1:length(splitted[[1]])) {
    spatial_unit_analysis <- c(spatial_unit_analysis, splitted[[1]][len])
  }
}

spatial_unit_analysis
spatial_unit_analysis[spatial_unit_analysis == ''] = 'NA'
spatial_unit_analysis[spatial_unit_analysis == 'sub-catchment'] = 'catchment/watershed'
spatial_unit_analysis[spatial_unit_analysis == 'watershed areas']  = 'catchment/watershed'
spatial_unit_analysis[spatial_unit_analysis == 'river catchment']  = 'catchment/watershed'
spatial_unit_analysis[spatial_unit_analysis == 'kettle hole catchment']  = 'catchment/watershed'
spatial_unit_analysis[spatial_unit_analysis == 'study region']  = 'region'
spatial_unit_analysis[spatial_unit_analysis == 'network of plots']  = 'network of plots/farms'
spatial_unit_analysis[spatial_unit_analysis == 'network of farms']  = 'network of plots/farms'
spatial_unit_analysis[spatial_unit_analysis == 'habitat and land-use polygons']  = 'habitat'
spatial_unit_analysis[spatial_unit_analysis == 'study sites']  = 'landscape'
spatial_unit_analysis[which(spatial_unit_analysis=="plot/block")] = 'parcel'
table(spatial_unit_analysis)
table(spatial_unit_analysis) %>% 
  as.data.frame() %>% rename(., 'count'= "Freq", 
                             'spatial unit of analysis' = 'spatial_unit_analysis') %>%
  arrange(desc(count)) %>%
  kbl() %>% kable_classic_2(full_width=FALSE) %>% 
  #footnote("* not clear if from LPIS") %>%
  save_kable("output/tables/sp_unit_inf.png", zoom = 5)

table(spatial_unit_analysis)
################################################################################
# Raw Information geometry split string by ;
raw_inf <- c()
# geometries == alles
extraction_all[which(extraction_all$Raw.information.from.IACS.used..geometrical.info..plot.shape..size..edges.....separate.multiple.entries.by....=='geometries'),"Raw.information.from.IACS.used..geometrical.info..plot.shape..size..edges.....separate.multiple.entries.by...."] = 'edges; plot shape; size'

# Thematic Information extracted
for(i in 1:nrow(extraction_all)) {
  x <- extraction_all[i, "Raw.information.from.IACS.used..geometrical.info..plot.shape..size..edges.....separate.multiple.entries.by...."]
  matched <- FALSE
  if(grepl(',', x) == TRUE) {
    splitted <- strsplit(x, ', ')
    matched <- TRUE
  }
  if(grepl(';', x) == TRUE) {
    splitted <- strsplit(x, '; ')
    matched <- TRUE
  }
  else if(matched == FALSE) {
    splitted <- x
  }
  print(splitted)
  print(length(splitted[[1]]))
  for(len in 1:length(splitted[[1]])) {
    raw_inf <- c(raw_inf, splitted[[1]][len])
  }
}

raw_inf[raw_inf == 'no'] = 'none'
raw_inf[raw_inf == ''] = 'none'
table(raw_inf)

table(raw_inf) %>% 
  as.data.frame() %>% rename(., 'count'= "Freq", 
                             'geometrical information' = 'raw_inf') %>%
  arrange(desc(count)) %>%
  kbl() %>% kable_classic_2(full_width=FALSE) %>% 
  #footnote("* not clear if from LPIS") %>%
  save_kable("output/tables/geo_inf.png", zoom = 5)
################################################################################
# Year information extracted
year_data_use <- c()

for(i in 1:nrow(extraction_all)) {
  x <- extraction_all[i, "year.s..of.data.use"]  
  matched <- FALSE
  if(grepl('-', x) == TRUE) {
    splitted <- strsplit(x, '-')
    print('strich')
    
    splitted <- list(splitted[[1]][1]:splitted[[1]][2])
    matched <- TRUE
  }
  
  else if(grepl(',', x) == TRUE) {
    splitted <- strsplit(x, ', ')
    matched <- TRUE
  }
  else if(grepl(';', x) == TRUE) {
    splitted <- strsplit(x, '; ')
    matched <- TRUE
  }
  else if( matched == FALSE ){
    splitted <- x
  }
  print(splitted)
  print(length(splitted[[1]]))
  for(len in 1:length(splitted[[1]])) {
    year_data_use <- c(year_data_use, splitted[[1]][len])
  }
}
# number of papers that don't state the year of data use
year_data_use %>% is.na() %>% sum()

# counts per year and histogram of data use
table(year_data_use)
hist(as.integer(year_data_use))
df_year = data.frame('year_data_use' = as.integer(year_data_use))

jpeg("output/figures_plots/year_data_use.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")


ggplot(data = df_year, aes(x=year_data_use)) + 
  #geom_histogram(bins = 70) + 
  geom_bar() +
  xlab('year of data use') + 
  scale_x_continuous(breaks=seq(1998, 2019, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) 

dev.off()
################################################################################
# year of paper
correct_meta_sample[which(correct_meta_sample$country != 'DE' & 
                            correct_meta_sample$country != 'FR' & 
                            correct_meta_sample$country != 'SE' & 
                            correct_meta_sample$country != 'CZ' & 
                            correct_meta_sample$country != 'AT'),"country"] = 'other'

counted_yr_country <- correct_meta_sample %>% group_by(year, country) %>% summarise(ct = n())
counted_yr_country$country <- factor(counted_yr_country$country, 
                                     levels = c("AT", "CZ", "DE", "FR", "SE", "other"))

jpeg("output/figures_plots/year_pub_country.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

ggplot(counted_yr_country, aes( x=year, y=ct, fill=country)) + 
  #geom_col(position = "stack") +
  geom_col(position = "stack") +
  
  ylab("count") + 
  scale_x_continuous(breaks=seq(2002, 2021, 1))+
  scale_y_continuous(breaks=seq(0, 10, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  scale_fill_brewer(palette="Set2")

dev.off()

extraction_all$Year %>% table()

extraction_all[which(extraction_all$Authors == "['Ronnenberg, Katrin', 'Ronnenberg  Egbert ; Siebert, Ursula, Katrin ; StrauÃŸ', 'Ronnenberg, Katrin']" ), "Discussion.of.IACS.dataset..verbatim"]
extraction_all$Discussion.of.IACS.dataset..verbatim
str_detect(extraction_all$Discussion.of.IACS.dataset..verbatim, pattern = 'level') %>% sum(., na.rm = TRUE)
extraction_all[str_detect(extraction_all$Discussion.of.IACS.dataset..verbatim, pattern = 'level'), ]
################################################################################
# Sample: collect count/shares information about y/n variables & importance variable; 
# create table or graph, descriptive text
# Get numbers for all counables from the Dataset


extraction_all %>% group_by(Dataset..likely..used) %>% summarise(n = n())

extraction_all %>% group_by(Importance.of.IACS.data.use) %>% summarise(n = n())

#extraction_all %>% group_by(Is.this.a.conceptual.paper...e.g...discussion.creation.of.IACS.data..use.of.IACS.data..etc.......if.exclusively.yes..other.variables.may.not.apply..leave.those.empty.) %>% 
#  summarise(n = n())

extraction_all %>% group_by(Combination.with.other.datasets.) %>% 
  summarise(n = n())

extraction_all %>% group_by(Indicator.s..generated.) %>% 
  summarise(n = n())

extraction_all %>% group_by(Raw.info.used..location) %>% 
  summarise(n = n())

extraction_all %>% group_by(Raw.info.used..time) %>% 
  summarise(n = n())


################################################################################
# Authors

authors = c()
years = c()

for(row in 1:nrow(correct_meta_sample)){
  
  cat = correct_meta_sample[row, "authors_corrected"]
  #print(cat)
  year = correct_meta_sample[row, "year"]
  splitted = str_split(cat, ',')
  #print(splitted)f
  for(index in 1:length(splitted[[1]])) {
    #print(string)
    cleaned = str_replace_all(splitted[[1]][index], "[[:punct:]]", "")
    #cleaned = str_replace_all(cleaned, " *", "")
    cleaned = str_squish(cleaned)
    #print(cleaned)
    if(cleaned=="Schmid E") {
      print(correct_meta_sample[row,])
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
ordered <- grouped[order(grouped$n, decreasing = T)[1:21],] 
ordered$cat <- factor(ordered$cat, levels=ordered$cat)

jpeg("output/figures_plots/authors_count_21.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

ordered %>% ggplot(., aes(y=n, x=cat)) + geom_boxplot() + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,10,1)) + ylab("count") + 
  theme(axis.title.x=element_blank()) 

dev.off()


################################################################################
# split up the category string
categories = c()
years = c()
for(row in 1:nrow(correct_meta_sample)){
  cat = correct_meta_sample[row, "categories_scimagojr"]
  year = correct_meta_sample[row, "year"]
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
ordered <- grouped[order(grouped$n, decreasing = T)[1:26],] 
#ordered <- ordered[-which(ordered$cat=='no mat'),]
ordered$cat <- factor(ordered$cat, levels=ordered$cat)


jpeg("output/figures_plots/categories.jpeg", 
     
     width = 20, height = 15, quality = 100, units = "cm",res= 300,
     
     type = "cairo")

ordered %>% ggplot(., aes(y=n, x=cat)) + geom_boxplot() + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks=seq(1,10,1)) + ylab('count') + 
  theme(axis.title.x=element_blank())

dev.off()
