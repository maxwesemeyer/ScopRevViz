################################################################################
# Offensichtliche Dinge zusammenfassen; Neue spalte am besten
library(ggplot2)
library(dplyr)

datasets_all <- read.csv('input/datasets_all.csv', sep=';', encoding = 'UTF-8')
unique(datasets_all$Dataset.combined.with.IACS..text.) %>% length()
unique(datasets_all$Dataset.combined.with.IACS..text.) 
# create a new column with thematically summarized Dataset categories
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('sat', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('aer', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('sensing', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('modis', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('sentinel', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('ortho', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('topo', ignore_case = TRUE)), "Dataset_comb_agg"] <- "topographic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('elev', ignore_case = TRUE)), "Dataset_comb_agg"] <- "topographic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('DEM', ignore_case = TRUE)), "Dataset_comb_agg"] <- "topographic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('terrain', ignore_case = TRUE)), "Dataset_comb_agg"] <- "topographic data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('soil', ignore_case = TRUE)), "Dataset_comb_agg"] <- "soil data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('eros', ignore_case = TRUE)), "Dataset_comb_agg"] <- "soil data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('peat', ignore_case = TRUE)), "Dataset_comb_agg"] <- "soil data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('runoff', ignore_case = TRUE)), "Dataset_comb_agg"] <- "soil data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('clima', ignore_case = TRUE)), "Dataset_comb_agg"] <- "climate data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('preci', ignore_case = TRUE)), "Dataset_comb_agg"] <- "climate data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('rainfall', ignore_case = TRUE)), "Dataset_comb_agg"] <- "climate data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('meteo', ignore_case = TRUE)), "Dataset_comb_agg"] <- "climate data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('habitat', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biodiversity data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('wild', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biodiversity data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('bird', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biodiversity data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('plant', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biodiversity data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('pollinator', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biodiversity data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('parasit', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biodiversity data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('bee', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biodiversity data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('clover', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biodiversity data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('biodiv', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biodiversity data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('ecosys', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biodiversity data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('land cover', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('land use', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('corine', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('field block', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('grassland classification', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('reference map', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"
# todo cadastral data
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('ATK', ignore_case = TRUE)), "Dataset_comb_agg"] <- "cadastral data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('cadas', ignore_case = TRUE)), "Dataset_comb_agg"] <- "cadastral data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('regis', ignore_case = TRUE)), "Dataset_comb_agg"] <- "cadastral data"


datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('price', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('economic', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('account', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('agron', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('cost', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('yield', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('harv', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('marg', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('manage', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('LFA', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('water', ignore_case = TRUE)), "Dataset_comb_agg"] <- "hydrological data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('irrig', ignore_case = TRUE)), "Dataset_comb_agg"] <- "hydrological data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('hydro', ignore_case = TRUE)), "Dataset_comb_agg"] <- "hydrological data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('inter', ignore_case = TRUE)), "Dataset_comb_agg"] <- "qualitative data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('quest', ignore_case = TRUE)), "Dataset_comb_agg"] <- "qualitative data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('qualitative', ignore_case = TRUE)), "Dataset_comb_agg"] <- "qualitative data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('statis', ignore_case = TRUE)), "Dataset_comb_agg"] <- "statistical data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('cens', ignore_case = TRUE)), "Dataset_comb_agg"] <- "statistical data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('milk quot', ignore_case = TRUE)), "Dataset_comb_agg"] <- "statistical data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('energy atlas', ignore_case = TRUE)), "Dataset_comb_agg"] <- "statistical data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('ALFIS', ignore_case = TRUE)), "Dataset_comb_agg"] <- "statistical data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('growth stages', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biological data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('crop coefficients', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biological data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('rooting', ignore_case = TRUE)), "Dataset_comb_agg"] <- "biological data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('mapy', ignore_case = TRUE)), "Dataset_comb_agg"] <- "map data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('Open Street Map', ignore_case = TRUE)), "Dataset_comb_agg"] <- "map data"


datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('growth ', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 

# All datsets without category yet
datasets_all[is.na(datasets_all$Dataset_comb_agg),"Dataset.combined.with.IACS..text."]
datasets_all$Dataset_comb_agg %>% is.na() %>% sum()
# TODO GPS nachschauen im Paper, andere 3 rauswerfen mapy und OSM zusammenfassen


################################################################################
datasets_all %>% group_by(Dataset_comb_agg) %>% summarise(count=n()) %>% arrange(desc(count)) %>%
  rename(., 'dataset combined' = 'Dataset_comb_agg') %>% kbl() %>%
  kable_classic_2(full_width=FALSE) #%>% 
  save_kable("output/tables_allMeta/exclusion_r_table.png", zoom = 5)



################################################################################