# Author Network plot

library(networkDynamic)
library(ndtv)
library(igraph)
library(statnet)
library(intergraph)
library(visNetwork)
library(stringr)
library(dplyr)
library(tidyr)
library(manipulate)
library(openxlsx)

# Papers-Quantity pro Autor and Countries as categorie
authors_title_country <- paper[, c("authors_corrected", "title", "country")]
authors_counter <- separate_rows(authors_title_country, authors_corrected, sep = ",") ##ACA CADA AUTOR QUEDA CON SUS PUBLICACIONES Y PAISES DE C/PUBLICAC.

authors_counter$authors_corrected <- str_replace_all(authors_counter$authors_corrected, "[[:punct:]]", "") ##PUNTUACION DE AUTORES WEG
authors_counter$authors_corrected <- apply(authors_counter["authors_corrected"], 1, function(x) gsub("^\\s+", "", x)) ##ELIMINA ESPACIO VACIO AL INICIO

# authors with various countries
df_countries_group <- aggregate(country ~ authors_corrected, data = authors_counter, FUN = function(x) paste(unique(x), collapse = ", ")) ##A C/AUTOR SE LE ASIGNAN SUS PAISES
df_countries_group2 <- df_countries_group[grepl(",", df_countries_group$country), ] ##SOLO AUTORES CON VARIOS PAISES
rownames(df_countries_group2) <- NULL

###
#most common country
get_most_common_country <- function(countries) {
  countries <- str_split(countries, ",")[[1]]
  most_common_country <- names(which.max(table(trimws(countries))))
  return(most_common_country)
}

df_countries_group$country <- sapply(df_countries_group$country, get_most_common_country)
table(df_countries_group$country)
###

# Assign "other" to countries that contain [?]|none|BE|CA|DK|EU, or that have zero length
df_countries_group$country <- ifelse(grepl("[?]|none|BE|CA|DK|EU", df_countries_group$country) | nchar(df_countries_group$country) == 0, "other", df_countries_group$country)

paper_count <- authors_counter %>%  ##SE TOTALIZAN LOS PAPERS POR AUTOR
  group_by(authors_corrected) %>%
  summarise(Papers = n()) %>%
  left_join(authors_counter %>% select(authors_corrected, country), by = "authors_corrected") %>%
  distinct(authors_corrected, .keep_all = TRUE)

paper_count$country <- df_countries_group$country ##ASIGNAR LA CLASIFICACION 5 PAISES + other

table(paper_count$country)

#------------------------------
# CO-AUTHOR NETWORK
paper.coauthor <- paper

replacer <- function(x) {
  return(str_replace_all(x, "[[:punct:]]", ""))
}

# 1. NODES (AUTHORS)

# split strings containing multiple authors for each publication into individual entries as a first step in creating a set of nodes
paper.coauthor <- sapply(as.character(paper$authors_corrected), strsplit, ", ")
paper.coauthor[[10]]
paper.coauthor <- lapply(paper.coauthor, replacer) # replacer function: Semikolon und ] sind weg
paper.coauthor[[10]]

# create the node set (alphabetized)
paper.coauthor.unique <- unique(unlist(paper.coauthor))[order(unique(unlist(paper.coauthor)))]

# 2. EDGES (CO-AUTHORSHIPS)
paper.bipartite.edges <- lapply(paper.coauthor, function(x) {paper.coauthor.unique  %in% x})
paper.bipartite.edges <- do.call("cbind", paper.bipartite.edges) # dimension is number of authors x number of papers
rownames(paper.bipartite.edges) <- paper.coauthor.unique

paper.mat <- paper.bipartite.edges %*% t(paper.bipartite.edges) #adjacency matrix 
mat <- paper.mat[order(rownames(paper.mat)), order(rownames(paper.mat))]

# 3. NETWORK
paper.statnet <- as.network(paper.mat, directed = FALSE, names.eval = "edge.lwd", ignore.eval = FALSE)
paper.statnet  # view network summary
paper.statnet %e% "edge.lwd"

# STATIC VISUALIZATION
plot.network(paper.statnet, edge.col <- "gray", edge.lwd = "edge.lwd", label = "vertex.names", label.cex = .5, label.pad = 0, label.pos = 1)

# INTERACTIVE VISUALIZATION
edges <- data.frame(from = data.frame(as.edgelist(paper.statnet))$X1, 
                    to = data.frame(as.edgelist(paper.statnet))$X2, 
                    value = (paper.statnet %e% "edge.lwd"))

nodes <- data.frame(id = 1:length(paper.statnet%v%"vertex.names"),
                    label = paper.statnet%v%"vertex.names", ## NAMEN ÜBERPRÜFEN HIER
                    title = paper_count$Papers,
                    country = paper_count$country) 


# Interactive Network
countries_colors <- c("#8dd3c7", "#FFFF4F", "#bebada", "#80b1d3", "#fdb462", "#b3de69")

nodes$color <- ifelse(nodes$country == "AT", countries_colors[1],
                      ifelse(nodes$country == "CZ", countries_colors[2],
                             ifelse(nodes$country == "DE", countries_colors[3],
                                    ifelse(nodes$country == "FR", countries_colors[4],
                                           ifelse(nodes$country == "SE", countries_colors[5],
                                                  countries_colors[6]
                                           )))))

plot_interactive <- visNetwork(nodes = nodes, edges = edges, main = "Co-Author Network", width = 800, height = 700) %>% 
  visPhysics(solver = "barnesHut") %>%
  visNodes(color = list(highlight = "red")) %>% 
  visLegend(stepY = 70, addNodes = list(
    list(label = "Austria", shape = "icon",
         icon = list(code ="25CF", size = 25, color = "#8dd3c7")),
    list(label = "Czech Republic", shape = "icon",
         icon = list(code ="25CF", size = 25, color = "#FFFF4F")),
    list(label = "Germany", shape = "icon",
         icon = list(code ="25CF", size = 25, color = "#bebada")),
    list(label = "France", shape = "icon",
         icon = list(code ="25CF", size = 25, color = "#80b1d3")),
    list(label = "Sweden", shape = "icon",
         icon = list(code ="25CF", size = 25, color = "#fdb462")),
    list(label = "other", shape = "icon",
         icon = list(code ="25CF", size = 25, color = "#b3de69"))), 
    useGroups = FALSE) %>% 
  visEdges(selectionWidth = 10, color = list(highlight = "red"), smooth = FALSE) %>% #smooth = FALSE --> improve the performance
  visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled  = TRUE, main = "Select by Author", style = "width:200px"),
             selectedBy = list(variable = "title", main = "Select by Number of Papers", style = "width:200px")) %>%
  visInteraction(hover = TRUE, tooltipDelay = 10)

plot_interactive 

visSave(plot_interactive, "output/coauthor_network.html", selfcontained = T)

#------------------------------
# earliest paper for each country
df_jahr <- paper %>%
  group_by(country) %>%
  summarize(year = min(year)) %>%
  left_join(paper, by = c("country", "year")) %>%
  select(year, country, authors_corrected, doi.url)

write.xlsx(df_jahr, file = here::here("output", "df_jahr.xlsx"))

#------------------------------
# country NULL or ?
country_check <- paper %>%
  filter(country %in% c(NULL, "?", "none", "other")) %>%
  select(year, country, authors_corrected, doi.url)