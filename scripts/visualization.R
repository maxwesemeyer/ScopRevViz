# TODO
# Study region Paper nochmal ansehen; habitat and land-use polygons ansehen DONE
# -> Sihe Notes
# Difference spatial unit of analys farm (6) und thematic info used farm (3) DONE
# -> plots description

# Eventuell neue Plots mit Kategorien pro Jahr 


#Datasets combined zusammengefasst; 2 keine Kategorie weil unclear

# Finale Tabelle rausschreiben. welches skript? 
# Welche Tabellen sollen geteilt werden? Metadaten mit Exlusions?

# numbers for FLowchart; Zahlen anpassen
# Done Zahlen angepasst 

# papers wieder includieren und aus dem exlusion datensatz nehmen 
# DONE

# TODO Datasets kategorisieren dokumentieren Text; 
# Doppelte Klassifizierung wird eventuell überschrieben aktuell
# Was tun wenn Datensatz nicht angegeben ist ? z.B. land cover data? 

# TODO Tabelle wie im Protocol Paper zu yes/no und thematic usw. Großbuchstabe 
# am Anfang
# str_to_title() -> erster Buchstabe zu Großbuchstabe

# Projekt teilen via Gitbucket -> Github
# DONE

# TODO Fließtext

# 16.1
# Done! flow chart update 
# Done! MEtadaten Abstract Wordclouds mehr Wörter; Sample Abstract Wordcloud mehr Wörter
# TODO NAs nachsehen im Paper -> both unclear -> Remove 
# TODO Paper zusammen kopieren 
# TODO + Flowchart; Details zur Suchstrategie und Ergebnissen 
# Nach EAAE:
# TODO Indicatoren aufräumen

################################################################################
library(ggplot2)
library(dplyr)


extraction_all <- read.csv('input/Extraction_sheet_all.csv', sep=';', encoding = 'UTF-8')
indicators_all <- read.csv('input/indicators_all.csv', sep=';', encoding = 'UTF-8')
datasets_all <- read.csv('input/datasets_all.csv', sep=';', encoding = 'UTF-8')
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
  geom_histogram(bins = 70) + xlab('year of data use')

dev.off()

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

