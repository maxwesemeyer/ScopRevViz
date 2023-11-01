library(dplyr)
library(stringr)
path <- 'C:/Users/Max/Documents/side_projects/scoping_review_invekos/FAIR/FAIR_full.csv'
extraction_all <- read.csv('C:/Users/Max/Documents/side_projects/scoping_review_invekos/ScopRevViz/input/all_extracted_Heidis_Nacharbeit_.csv', sep=',', encoding = 'UTF-8')

df <- read.csv(path)
df <- df[-which(is.na(df$Information.on.data.provision.and.access..Yes..no..partly.)),]
df <- df[-which(!(df$Authors %in% extraction_all$Authors)),]

df$Description.of.dataset..Yes..no..partly. <- tolower(df$Description.of.dataset..Yes..no..partly.)
df$Provision.of.source.code..Yes..no. <- tolower(df$Provision.of.source.code..Yes..no.)
df$Description.of.data.cleaning.and.pre.processing..Yes..no..partly..reference.to.other.publication. <- tolower(df$Description.of.data.cleaning.and.pre.processing..Yes..no..partly..reference.to.other.publication.)
df$Information.on.data.provision.and.access..text <- tolower(df$Information.on.data.provision.and.access..text)

merged <- merge(df, extraction_all[,c("Title", "year.s..of.data.use", "Dataset..likely..used",
                                      "Raw.information.from.IACS.used..geometrical.info..plot.shape..size..edges.....separate.multiple.entries.by....",
                                      "Raw.info.used..thematic..other..crop.type..land.use.type..LSE..organic..AES.....separate.multiple.entries.by....")], by.x='Title', by.y='Title')

merged$Dataset..likely..used
################################################################################
# correction of data description column and summary
merged$condition_ds_1 <- ifelse(merged[,"Dataset..likely..used"] != 'unclear', 1, 0)
merged$condition_ds_2 <- ifelse(is.na(merged[,"year.s..of.data.use"]), 0, 1)
merged$conditions_sum <- merged$condition_ds_1 + merged$condition_ds_2
merged$Description.of.dataset..Yes..no..partly._corrected <- "yes"
merged[which(merged$conditions_sum==1),"Description.of.dataset..Yes..no..partly._corrected"] <- 'partly'
merged[which(merged$conditions_sum==0),"Description.of.dataset..Yes..no..partly._corrected"] <- 'no'

ct <- merged %>% group_by(Description.of.dataset..Yes..no..partly._corrected) %>%
  summarise(n = n())
ct


merged %>% filter(Description.of.dataset..Yes..no..partly._corrected == 'yes') %>% 
  group_by(Dataset..likely..used) %>% 
  summarise(n=n())

# not mentioning year but dataset
merged %>% filter(Description.of.dataset..Yes..no..partly._corrected == 'partly', 
                  is.na(year.s..of.data.use))

# not mentioning dataset but year
merged %>% filter(Description.of.dataset..Yes..no..partly._corrected == 'partly', 
                  Dataset..likely..used == 'unclear')
##############
# data provision summary
ct <- df %>% group_by(Information.on.data.provision.and.access..Yes..no..partly.) %>%
  summarise(n = n())
ct

##############
# source code
ct <- df %>% group_by(Provision.of.source.code..Yes..no.) %>%
  summarise(n = n())
ct
df[str_starts(df$Provision.of.source.code..Yes..no., 'y'), "Authors"]
##############
# preprocessing
ct <- df %>% group_by(Description.of.data.cleaning.and.pre.processing..Yes..no..partly..reference.to.other.publication.) %>%
  summarise(n = n())
ct
df[str_starts(df$Description.of.data.cleaning.and.pre.processing..Yes..no..partly..reference.to.other.publication., 'ref'), ] %>%
  nrow()

str_detect(df$Information.on.data.provision.and.access..text, 'thank') %>% sum()
