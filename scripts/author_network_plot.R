# Author Network plot

#library(scholar) # Provides functions to extract citation data from Google Scholar
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

################################################################################
setwd("D:/ForLand")
getwd()

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

paper_all = read.csv('input/papers_and_exlusions.csv', sep=',', encoding="UTF-8")
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
#replacer <- function(x) {
#  return(str_replace_all(x, "[[:punct:]]", ""))
#}

paper <- paper[-which(paper$authors=="['Quendler, Erika']" | 
                        paper$authors=="['Lawson, Gerry', 'Dupraz, Christian', 'Watté, Jeroen']" |
                        paper$authors=="['Schmitzberger, I', 'Wrbka, Th.', 'Steurer, B', 'Aschenbrenner, G', 'Peterseil, J', 'Zechmeister, H G']"),]

replacer <- function(x) {
  return(str_replace_all(x, "[[:punct:]]", ""))
}

#------------------------------- BIS HIER PAPER

# zuerst "authors_corrected.R" ausführen

# Spalte "authors_corrected" ersetzen
paper$authors_corrected <- aut_check$authors_corrected

#------------------------------
# Papers-Anzahl pro Autor

authors_title <- paper[, c("authors_corrected", "title")]
authors_counter <- separate_rows(authors_title, authors_corrected, sep = ",")
authors_counter$authors_corrected <- str_replace_all(authors_counter$authors_corrected, "[[:punct:]]", "")
authors_counter$authors_corrected <- apply(authors_counter["authors_corrected"], 1, function(x) gsub("^\\s+", "", x))

paper_count <- authors_counter %>%
  group_by(authors_corrected) %>%
  summarise(Papers = n())

#names(paper_count)[names(paper_count) == "authors_corrected"] <- "label"  # Spalte Name verändern

#rm(authors_title, authors_counter)

#------------------------------
# CO-AUTHOR NETWORK
paper.coauthor <- paper

# 1. NODES (AUTHORS)

# split strings containing multiple authors for each publication into individual entries as a first step in creating a set of nodes
paper.coauthor <- sapply(as.character(paper$authors_corrected), strsplit, ", ")
paper.coauthor[[10]]
paper.coauthor <- lapply(paper.coauthor, replacer) # replacer function: Semikolon und ] sind weg
paper.coauthor[[10]]

# create the node set (alphabetized)
paper.coauthor.unique <- unique(unlist(paper.coauthor))[order(unique(unlist(paper.coauthor)))] # unique() delete the duplicate values
# unlist() takes a list as an argument and returns a vector (elements of the same data type)

# 2. EDGES (CO-AUTHORSHIPS)
paper.bipartite.edges <- lapply(paper.coauthor, function(x) {paper.coauthor.unique  %in% x})
paper.bipartite.edges <- do.call("cbind", paper.bipartite.edges) # dimension is number of authors x number of papers
rownames(paper.bipartite.edges) <- paper.coauthor.unique

paper.mat <- paper.bipartite.edges %*% t(paper.bipartite.edges) #bipartite to unimode ### t() to calculate transpose of a matrix or Data Frame
mat <- paper.mat[order(rownames(paper.mat)), order(rownames(paper.mat))]

# 3. NETWORK
paper.statnet <- as.network(paper.mat, directed = FALSE, names.eval = "edge.lwd", ignore.eval = FALSE)
paper.statnet  # view network summary
paper.statnet %e% "edge.lwd"

# STATIC VISUALIZATION
plot.network(paper.statnet, edge.col <- "gray", edge.lwd = "edge.lwd", label = "vertex.names", label.cex = .5, label.pad = 0, label.pos = 1)

# INTERACTIVE VISUALIZATION
edges <- data.frame(from=data.frame(as.edgelist(paper.statnet))$X1, 
                    to=data.frame(as.edgelist(paper.statnet))$X2, 
                    value=(paper.statnet %e% "edge.lwd"))

nodes <- data.frame(id = 1:length(paper.statnet%v%"vertex.names"),
                    label = paper.statnet%v%"vertex.names",
                    title = paper_count$Papers) ## NAMEN ÜBERPRÜFEN HIER

# Interactive Network

# Nodes-Size-Skalierung
#nodes$size_log <- 20 + 40 * (log(nodes$Number_Papers) - min(log(nodes$Number_Papers))) / (max(log(nodes$Number_Papers)) - min(log(nodes$Number_Papers)))

plot_interactive <- visNetwork(nodes = nodes, edges = edges, main = "Co-Author Network", width = 900, height = 900) %>% 
  visPhysics(solver = "barnesHut") %>%  #barnesHut, forceAtlas2Based --> AUCH PASST
  #visIgraphLayout(layout = "layout_nicely", type = "full") %>%
  #visNodes(size = "size_log", color = list(highlight = "red")) %>% 
  visNodes(color = list(highlight = "red")) %>% 
  visEdges(selectionWidth = 10, color = list(highlight = "#2B7CE9"), smooth = FALSE) %>% #smooth = FALSE --> improve the performance
  visOptions(nodesIdSelection = list(enabled  = TRUE, main = "Select by Author", style = "width:200px"),
             selectedBy = list(variable = "title", main = "Select by Number of Papers", style = "width:200px")) %>%
  visInteraction(hover = TRUE, tooltipDelay = 10)

plot_interactive

# save as html
visSave(plot_interactive, "coauthor_network.html", selfcontained = T)

# igraph instead of statnet package
#paper.igraph <- intergraph::asIgraph(paper.statnet, vnames="vertex.names", vertex.size = 30)
#visIgraph(paper.igraph)




