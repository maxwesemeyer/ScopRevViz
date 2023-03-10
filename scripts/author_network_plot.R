# Author Network plot

library(networkDynamic)
library(ndtv)
library(igraph)
library(statnet)
library(intergraph)
library(visNetwork)
library(stringr)

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
####################################
# remove papers that have been removed during the Charting process of the 60 Papers
# 1 of Stefan, 1 of Andreas, 1 of Franziska
replacer <- function(x) {
  return(str_replace_all(x, "[[:punct:]]", ""))
}
paper <- paper[-which(paper$authors=="['Quendler, Erika']" | 
                        paper$authors=="['Lawson, Gerry', 'Dupraz, Christian', 'Watté, Jeroen']" |
                        paper$authors=="['Schmitzberger, I', 'Wrbka, Th.', 'Steurer, B', 'Aschenbrenner, G', 'Peterseil, J', 'Zechmeister, H G']"),]
paper.coauthor <- paper
paper.coauthor[grepl(x =paper.coauthor, pattern="L"), "authors_corrected"]
paper.coauthor
paper.coauthor.unique[grepl(pattern="Hubert", x=paper.coauthor.unique)]
#paper$authors_corrected <- replacer(paper$authors_corrected)
paper.coauthor = sapply(as.character(paper$authors_corrected), strsplit, ", ")
paper.coauthor[[10]]
paper.coauthor = lapply(paper.coauthor, replacer)
paper.coauthor[[10]]
#paper.coauthor = lapply(paper.coauthor, trimws)


paper.coauthor.unique = unique(unlist(paper.coauthor))[order(unique(unlist(paper.coauthor)))]


paper.bipartite.edges = lapply(paper.coauthor, function(x) {paper.coauthor.unique  %in% x})
paper.bipartite.edges = do.call("cbind", paper.bipartite.edges) # dimension is number of authors x number of papers
rownames(paper.bipartite.edges) = paper.coauthor.unique

paper.mat = paper.bipartite.edges %*% t(paper.bipartite.edges) #bipartite to unimode
mat = paper.mat[order(rownames(paper.mat)), order(rownames(paper.mat))]

paper.statnet = as.network(paper.mat, directed = FALSE, 
                           names.eval = "edge.lwd", ignore.eval = FALSE)
paper.statnet %e% "edge.lwd"

# view network summary

plot.network(paper.statnet, edge.col = "gray", edge.lwd = "edge.lwd", label = "vertex.names", label.cex = .5, label.pad = 0, label.pos = 1)

plot.network(paper.statnet, edge.col = "gray", edge.lwd = paper.statnet %e% "edge.lwd" , label = "vertex.names", label.cex = .5, label.pad = 0, label.pos = 1)



paper.igraph = intergraph::asIgraph(paper.statnet, vnames="vertex.names", vertex.size = 30)

visIgraph(paper.igraph)

# TODO CORRECT AUTHOR NAMES (Paula?)

kellogg.nodes <- data.frame(id = 1:length(paper.statnet%v%"vertex.names"),
                            label = paper.statnet%v%"vertex.names",
                            title = paper.statnet%v%"vertex.names"))

kellogg.edges <- data.frame(from=data.frame(as.edgelist(paper.statnet))$X1, 
                            to=data.frame(as.edgelist(paper.statnet))$X2, 
                            value=(paper.statnet %e% "edge.lwd"))

visNetwork(kellogg.nodes, kellogg.edges, main = "Co-Author Network", width = 900, height = 500) %>% 
  visIgraphLayout(layout = "layout_nicely", type = "full")
