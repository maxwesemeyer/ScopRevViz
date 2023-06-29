library(ggplot2)
library(ggalluvial)
library(data.table)
library(dplyr)
library(stringr)
# load data and prepare


indicators_all <- read.csv('input/indicators_list_HL_2.csv', sep=',', encoding = 'UTF-8')
indicators_all$Indicandum..text.
indicators_all <- na.omit(indicators_all)
################################################################################
# reclassify indicators in new column 
"""
indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('proport', ignore_case = TRUE)), "Indicator_recl"] <- "Proportion of ..."
indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('share', ignore_case = TRUE)), "Indicator_recl"] <- "Proportion of ..."
indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('amount', ignore_case = TRUE)), "Indicator_recl"] <- "Proportion of ..."

indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('number', ignore_case = TRUE)), "Indicator_recl"] <- "Number of ..."
indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('simp', ignore_case = TRUE)), "Indicator_recl"] <- "Simpsons Index"
indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('Shann', ignore_case = TRUE)), "Indicator_recl"] <- "Shannon Index"
indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('contag', ignore_case = TRUE)), "Indicator_recl"] <- "Contagion Index"
indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('sequ', ignore_case = TRUE)), "Indicator_recl"] <- "Crop sequences"

indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('area', ignore_case = TRUE)), "Indicator_recl"] <- "length of features"
indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('length', ignore_case = TRUE)), "Indicator_recl"] <- "length of features"

indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('size', ignore_case = TRUE)), "Indicator_recl"] <- "size of ..."
indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('type', ignore_case = TRUE)), "Indicator_recl"] <- "type of ..."

indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('ratio', ignore_case = TRUE)), "Indicator_recl"] <- "ratio of ..."

indicators_all[str_detect(indicators_all$Indicator.Name..text., regex('ratio', ignore_case = TRUE)), "Indicator.Name..text."] 

################################################################################
# reclassify Indicanda in new Column
indicators_all[str_detect(indicators_all$Indicandum..text., regex('inten', ignore_case = TRUE)), "Indicandum_recl"] <- "land use intensity"

indicators_all[str_detect(indicators_all$Indicandum..text., regex('crop div', ignore_case = TRUE)), "Indicandum_recl"] <- "crop diversity"
indicators_all[str_detect(indicators_all$Indicandum..text., regex('crops', ignore_case = TRUE)), "Indicandum_recl"] <- "crop diversity"

indicators_all[str_detect(indicators_all$Indicandum..text., regex('complexity', ignore_case = TRUE)), "Indicandum_recl"] <- "Landscape complexity"
indicators_all[str_detect(indicators_all$Indicandum..text., regex('heterogeneity', ignore_case = TRUE)), "Indicandum_recl"] <- "Landscape complexity"

indicators_all[str_detect(indicators_all$Indicandum..text., regex('rota', ignore_case = TRUE)), "Indicandum_recl"] <- "crop rotation"
indicators_all[str_detect(indicators_all$Indicandum..text., regex('cropping', ignore_case = TRUE)), "Indicandum_recl"] <- "crop rotation"

indicators_all[str_detect(indicators_all$Indicandum..text., regex('structural ', ignore_case = TRUE)), "Indicandum_recl"] <- "risk to the spread desease"

indicators_all[str_detect(indicators_all$Indicandum..text., regex('ecosystem', ignore_case = TRUE)), "Indicandum_recl"] <- "Ecosystem Service"

indicators_all[str_detect(indicators_all$Indicandum..text., regex('contam', ignore_case = TRUE)), "Indicandum_recl"] <- "Pollution"
indicators_all[str_detect(indicators_all$Indicandum..text., regex('pollu', ignore_case = TRUE)), "Indicandum_recl"] <- "Pollution"
indicators_all[str_detect(indicators_all$Indicandum..text., regex('anthropogenic factors', ignore_case = TRUE)), "Indicandum_recl"] <- "Pollution"

indicators_all[str_detect(indicators_all$Indicandum..text., regex('soil erodibility', ignore_case = TRUE)), "Indicandum_recl"] <- "soil erodibility"


indicators_all[str_detect(indicators_all$Indicandum..text., regex('crops', ignore_case = TRUE)), "Indicandum..text."] 

"""
################################################################################


indicators <- indicators_all[,c("Indicator..short.", "Indicandum.group")]
colnames(indicators) <- c("Indicator.group", "Indicandum.group")
ind.df <- data.table(table(indicators))


ggplot(ind.df[N > 0],
       aes(y = N, axis1 = Indicator.group, axis2 = Indicandum.group)) +
  geom_alluvium(aes(fill = Indicandum.group), width = 1/6, alpha = .75, curve_type = "cubic") +
  geom_stratum(width = 1/4, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  scale_x_discrete(limits = c("Indicators", "Indicanda"), expand = c(.05, .05)) +
  #scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Indicators and indicanda in the sample papers") +
  scale_fill_viridis_d(option = "plasma", alpha = 1) +
  # scale_fill_paletteer_d("colorBlindness::paletteMartin") +
  # theme_void() +
  theme_classic() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Indicanda"))

