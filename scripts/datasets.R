################################################################################
#
library(ggplot2)
library(dplyr)
library(stringr)
library(kableExtra)
library(tidyr)
datasets_all <- read.csv('input/datasets_list_HL_2.csv', sep=',', encoding = 'UTF-8')
datasets_all[is.na(datasets_all$linked.by),"linked.by"] <- 'other'
datasets_all[str_detect(datasets_all$linked.by, regex('spatial match', ignore_case = TRUE)), "linked.by"] <- 'spatial match'
datasets_all[str_detect(datasets_all$linked.by, regex('farm', ignore_case = TRUE)), "linked.by"] <- 'farm'
datasets_all[str_detect(datasets_all$Dataset.group, regex('LULC', ignore_case = TRUE)), "Dataset.group"] <- 'LULC data'
datasets_all[str_detect(datasets_all$Dataset.group, regex('hydro', ignore_case = TRUE)), "Dataset.group"] <- 'hydrological data'
datasets_all[str_detect(datasets_all$Dataset.group, regex('statistical', ignore_case = TRUE)), "Dataset.group"] <- 'statistical and economic data'
datasets_all[str_detect(datasets_all$Dataset.group, regex('ecological data', ignore_case = TRUE)), "Dataset.group"] <- 'ecological data'
datasets_all[str_detect(datasets_all$Dataset.group, regex('climate', ignore_case = TRUE)), "Dataset.group"] <- 'climate and weather data'
datasets_all[str_detect(datasets_all$Dataset.group, regex('other', ignore_case = TRUE)), "Dataset.group"] <- 'other data'
datasets_all[str_detect(datasets_all$Dataset.group, regex('cadastral', ignore_case = TRUE)), "Dataset.group"] <- 'cadastral data'

#datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('GPS ', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'GPS points of Land use land cover ground truth information '
################################################################################
# correct the dataset combined column 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('land-use map derived from', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "land-use map"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('ATKIS', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "ATKIS"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('corine', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "CORINE"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('Urban Atlas', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "Urban Atlas"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('interview', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'farmer interview data'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('questionnaire', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'questionnaire data'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('qualitative', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'qualitative data'

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('Copernicus', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 


datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('formo', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "Formosat-2"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('RapidEye ', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "RapidEye"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('TerraSar-X , Radarsat-2 and Alos', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "TerraSar-X , Radarsat-2 and Alos"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('satellite', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "Satellite data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('MOD13Q1', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "MODIS"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('modis', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "MODIS"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('aerial', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "aerial photo"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('sentinel', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "Sentinel"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('landsat', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "Landsat"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('ortho', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "ortho photo"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('BD Topo', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "BD Topo, IGN"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('topog', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "topographic data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('elev', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "Digital Elevation Model (DEM)"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('DEM', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "Digital Elevation Model (DEM)"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('terrain', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "Digital Elevation Model (DEM)"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('soil map', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'Soil map'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('soil type', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'Soil type'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('soil ', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('eros', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('peat', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('runoff', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('clima', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'Climate data'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('precipitation', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('rainfall', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('meteorological', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'Meteorological data'

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('wildlife', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'wildlife monitoring data'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('habitat', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('bird', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'bird monitoring data'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('plant', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('pollinator', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('parasit', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('bee ', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('beet', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'Ground beetle sampling'

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('clover', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('biodiv', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('land cover', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'land cover data'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('Land Use/Cover', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "Land Use/Cover data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('field block', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('grassland classification', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('reference map', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('cadas', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('regis', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]


datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('price', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('economic', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('Farm Accountancy Data', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'FADN'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('agron', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('cost', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('yield', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- "yield data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('harv', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]  <- "yield data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('margin', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('LFA', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('water', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('status of the water bodies', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'Governmental monitoring of water bodies'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('hydro', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 


datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('statis', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('cens', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('milk quot', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('energy atlas', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('ALFIS', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('growth stages', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'plant growth stages'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('crop coefficients', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] <- 'crop coefficients'
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('rooting', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('mapy', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."]
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('Open Street Map', ignore_case = TRUE)), "Dataset.combined.with.IACS..text."] 



unique(datasets_all$Dataset.combined.with.IACS..text.) %>% length()
unique(datasets_all$Dataset.combined.with.IACS..text.) 

# create a new column with thematically summarized Dataset categories
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('sat', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('Rapid', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('aerial', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('sensing', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('modis', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('sentinel', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('landsat', ignore_case = TRUE)), "Dataset_comb_agg"] <- "remote sensing data"
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
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('precipitation', ignore_case = TRUE)), "Dataset_comb_agg"] <- "climate data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('rainfall', ignore_case = TRUE)), "Dataset_comb_agg"] <- "climate data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('meteorological', ignore_case = TRUE)), "Dataset_comb_agg"] <- "climate data"

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
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('land-use', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('Urban Atlas', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('corine', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('field block', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('grassland classification', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('land cover', ignore_case = TRUE)), "Dataset_comb_agg"] <- "LULC data"
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
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('margin', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('manage', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('LFA', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('FADN', ignore_case = TRUE)), "Dataset_comb_agg"] <- "economic/agronomic data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('water', ignore_case = TRUE)), "Dataset_comb_agg"] <- "hydrological data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('irrig', ignore_case = TRUE)), "Dataset_comb_agg"] <- "hydrological data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('hydro', ignore_case = TRUE)), "Dataset_comb_agg"] <- "hydrological data"

datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('interview', ignore_case = TRUE)), "Dataset_comb_agg"] <- "qualitative data"
datasets_all[str_detect(datasets_all$Dataset.combined.with.IACS..text., regex('questionnaire', ignore_case = TRUE)), "Dataset_comb_agg"] <- "qualitative data"
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
datasets_all <- drop_na(datasets_all)
# All datsets without category yet
datasets_all[is.na(datasets_all$Dataset_comb_agg),"Dataset.combined.with.IACS..text."]
datasets_all$Dataset_comb_agg %>% is.na() %>% sum()
# TODO GPS nachschauen im Paper # DONE # , andere 2 rauswerfen mapy und OSM zusammenfassen

datasets_all$Dataset_comb_agg <- str_to_title(datasets_all$Dataset_comb_agg)

write.csv(datasets_all, 'datasets_out.csv')
################################################################################
datasets_all %>% group_by(Dataset_comb_agg) %>% summarise(Count=n()) %>% arrange(desc(Count)) %>%
  rename(., 'Dataset combined' = 'Dataset_comb_agg') %>% kbl() %>%
  kable_classic_2(full_width=FALSE) %>% 
  save_kable("output/tables/Datasets_combined_categories.png", zoom = 5)

datasets_all %>% group_by(Dataset.group) %>% summarise(Count=n()) %>% arrange(desc(Count)) %>%
  rename(., 'Dataset combined' = 'Dataset.group') %>% kbl() %>%
  kable_classic_2(full_width=FALSE) %>% 
  save_kable("output/tables/Datasets_combined_categories.png", zoom = 5)


datasets_all %>% group_by(linked.by) %>% summarise(Count=n()) %>% 
  arrange(desc(Count)) %>% rename(., 'linked by' = 'linked.by') %>% kbl() %>%
  kable_classic_2(full_width=FALSE) %>% 
  save_kable("output/tables/linkedby.png", zoom = 5)
################################################################################
# reclassify Link for combination

datasets_all[str_detect(datasets_all$link.for.combination..e.g...spatial.match..farm.ID., regex('unclear', ignore_case = TRUE)), "link.for.combination..e.g...spatial.match..farm.ID."] <- "unclear"
datasets_all[datasets_all$link.for.combination..e.g...spatial.match..farm.ID.=='', "link.for.combination..e.g...spatial.match..farm.ID."] <- "unclear"

datasets_all[str_detect(datasets_all$link.for.combination..e.g...spatial.match..farm.ID., regex('spatial match', ignore_case = TRUE)), "link.for.combination..e.g...spatial.match..farm.ID."] <- "spatial match"

datasets_all[str_detect(datasets_all$link.for.combination..e.g...spatial.match..farm.ID., regex('farm ID?', ignore_case = TRUE)), "link.for.combination..e.g...spatial.match..farm.ID."] <- "farm ID"





datasets_all %>% group_by(link.for.combination..e.g...spatial.match..farm.ID.) %>% summarise(Count=n()) %>% arrange(desc(Count)) %>%
  rename(., 'Link' = 'link.for.combination..e.g...spatial.match..farm.ID.') %>% kbl() %>%
  kable_classic_2(full_width=FALSE)  %>% 
  save_kable("output/tables/Link_f_combination.png", zoom = 5)



################################################################################
datasets_all %>% group_by(Dataset_comb_agg, link.for.combination..e.g...spatial.match..farm.ID.) %>% summarise(Count=n()) %>% #arrange(desc(Count)) %>%
  rename(., 'Dataset combined' = 'Dataset_comb_agg', 'Link for combination' = 'link.for.combination..e.g...spatial.match..farm.ID.') %>%
  kbl() %>%
  kable_classic_2(full_width=FALSE) %>% 
  save_kable("output/tables/Link_f_comb_dataset.png",  zoom = 2)






3* (902.93) + 3* (878.26) + 6* (851.48)

46.98*3+28.19+15.13+28.35+45.77*2

9*50 + 52
