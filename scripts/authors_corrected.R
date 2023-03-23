paper[,"authors"]
aut_check <- paper[, c("authors", "authors_corrected")]

#-----------
#install.packages("xlsx")
#library(xlsx)
#write.xlsx(aut_check, "aut_check.xlsx")
#-----------

aut_check[aut_check$authors == "['Latruffe, Laure', 'Piet, Laure']", "authors_corrected"] <- "['Latruffe; L', 'Piet; L']"
aut_check$authors_corrected[5]

aut_check[aut_check$authors == "['Laggner  Natascha, Birgit ; Orthen']", "authors_corrected"] <- "['Orthen; N', 'Laggner; B']"
aut_check$authors_corrected[6]

aut_check[aut_check$authors == "['Ronnenberg, Katrin', 'Ronnenberg  Egbert ; Siebert, Ursula, Katrin ; Strauß', 'Ronnenberg, Katrin']", "authors_corrected"] <- 
  "['Ronnenberg; K', 'Strauß; E', 'Siebert; U']" 
# Ronnenberg, Katrin; Strauß, Egbert; Siebert, Ursula
aut_check$authors_corrected[7]

aut_check[aut_check$authors_corrected == "['Fortems-Cheiney; A', 'Dufour; G', 'Dufossé; K', 'Couvidat; F', 'Gilliot; J M', 'Siour; G', 'Beekmann; M', 'Foret; G', 'Meleux; F', 'Clarisse; L', 'Coheur; P F', 'Van\\xa0Damme; M', 'Clerbaux; C', 'Génermont; S']", "authors_corrected"] <- 
  "['Fortems-Cheiney; A', 'Dufour; G', 'Dufossé; K', 'Couvidat; F', 'Gilliot; J M', 'Siour; G', 'Beekmann; M', 'Foret; G', 'Meleux; F', 'Clarisse; L', 'Coheur; P F', 'Van Damme; M', 'Clerbaux; C', 'Génermont; S']" 
# Martin Van Damme
aut_check$authors_corrected[18]

aut_check[aut_check$authors == "['Sagris, Valentina', 'Sagris  Piotr ; Milenov, Pavel ; Devos, Wim, Valentina ; Wojda', 'Sagris, Valentina', 'Sagris  Piotr ; Milenov, Pavel ; Devos, Wim, Valentina ; Wojda']", "authors_corrected"] <- 
  "['Sagris; V', 'Wojda; P', 'Milenov; P', 'Devos; W']"
# Valentina Sagris, Piotr Wojda, Pavel Milenov, Wim Devos
aut_check$authors_corrected[23]

aut_check[aut_check$authors == "['Maší?ek, Tomáš', 'Dufková, Jana Kozlovsky', 'Záme?ník, Tomáš']", "authors_corrected"] <- "['Mašíček; T', 'Dufková; J K', 'Zámečník; T']"
# Tomáš Mašíček,Jana Kozlovsky Dufková and Tomáš Zámečník
aut_check$authors_corrected[27]

aut_check[aut_check$authors == "['Hubert-Moy  Jeanne ; Fabre, Elodie ; Rozo, Clémence ; Arvor, Damien ; Corpetti, Thomas ; Rapinel, Sébastien, Laurence ; Thibault', 'Rapinel, Laurence Hubert-Moy ; Jeanne Thibault ; Elodie Fabre ; Clémence Rozo ; Damien Arvor ; Thomas Corpetti ; Sébastien']", "authors_corrected"] <- 
  "['Hubert-Moy; L', 'Thibault; J', 'Fabre; E', 'Rozo; C', 'Arvor; D', 'Corpetti; T', 'Rapinel; S']"
# Laurence Hubert-Moy, Jeanne Thibault, Elodie Fabre, Clémence Rozo, Damien Arvor, Thomas Corpetti, Sébastien Rapinel
aut_check$authors_corrected[28]

aut_check[aut_check$authors == "['Gregar, Jan', 'Petr?, Jan', 'Novotná, Jana']", "authors_corrected"] <- "['Gregar; J', 'Petrů; J', 'Novotná; J']"
# Jan Gregar, Jan Petrů, Jana Novotná
aut_check$authors_corrected[36]

aut_check[aut_check$authors == "['Martin, Philippe', 'Ronfort, Céline', 'Laroutis, Dimirti', 'Souchere, Veronique', 'Sebillotte, Clementina']", "authors_corrected"] <- 
  "['Martin; P', 'Ronfort; C', 'Laroutis; D', 'Souchère; V', 'Sebillotte; C']"
aut_check$authors_corrected[60]

aut_check[aut_check$authors == "['Ronnenberg, Katrin', 'Strauß ,Egbert', 'Siebert, Ursula']", "authors_corrected"] <- "['Ronnenberg; K', 'Strauß; E', 'Siebert; U']"
aut_check$authors_corrected[68]

aut_check[aut_check$authors == "['Vincente-Vincente, Jose Luis', 'Sanz-Sanz, Esther', 'Napoléone, Claude', 'Moulery, Michel', 'Piorr, Annette']", "authors_corrected"] <- 
  "['Vincente-Vincente; J L', 'Sanz-Sanz; E', 'Napoléone; C', 'Moulery; M', 'Piorr; A']"
aut_check$authors_corrected[74]

aut_check[aut_check$authors == "['Battude, Marjorie', 'Al Bitar, Ahmad', 'Morin, David', 'Cros, Jérôme', 'Huc, Mireille', 'Marais Sicre, Claire', 'Le Dantec, Valérie', 'Demarez, Valérie']", "authors_corrected"] <- 
  "['Battude; M', 'Al Bitar; A', 'Morin; D', 'Cros; J', 'Huc; M', 'Marais-Sicre; C', 'Le Dantec; V', 'Demarez; V']"
#Marais-Sicre, Claire
aut_check$authors_corrected[82]

aut_check[aut_check$authors == "['Sklenicka, Petr', 'Salek, Miroslav']", "authors_corrected"] <- "['Sklenicka; P', 'Salek; M']"
aut_check$authors_corrected[85]

aut_check[aut_check$authors == "['Abdi, Abdulhakim M', 'Carrié, Romain', 'Sidemo-Holm, William', 'Cai, Zhanzhang', 'Boke-Olén, Niklas', 'Smith, Henrik G', 'Eklundh, Lars', 'Ekroos, Johan']", "authors_corrected"] <- 
  "['Abdi; A M', 'Carrié; R', 'Sidemo-Holm; W', 'Cai; Z', 'Boke Olén; N', 'Smith; H G', 'Eklundh; L', 'Ekroos; J']"
aut_check$authors_corrected[91]

aut_check[aut_check$authors == "['Sklenicka, Petr', 'Janovska, Vratislava', 'Salek, Miroslav', 'Vlasak, Josef', 'Molnarova, Kristina']", "authors_corrected"] <- 
  "['Sklenicka; P', 'Janovska; V', 'Salek; M', 'Vlasak; J', 'Molnarova; K J']"
aut_check$authors_corrected[97]

aut_check[aut_check$authors == "['Hlavsa, Tomas', 'Spicka, Jindrich', 'Stolbova, Marie', 'Hlouskova, Zuzana']", "authors_corrected"] <- "['Hlavsa; T', 'Spicka; J', 'Štolbová; M', 'Hlouskova; Z']"
aut_check$authors_corrected[98]

aut_check[aut_check$authors == "['Bougnères, Pierre', 'Porcher, Raphael', 'Esterle, Laure', 'Baker, David', 'Vaissière, Adrien de la', 'Meurisse, Sofia', 'Valtat, Sophie', 'Castell, Anne-Laure', 'Mouriquand, Pierre', 'Valleron, Alain-Jacques', 'Bougnères  Raphael ; Esterle, Laure ; Baker, David ; de la Vaissière, Adrien ; Meurisse, Sofia ; Valtat, Sophie ; Castell, Anne-Laure ; Mouriquand, Pierre ; Valleron, Alain-Jacques, Pierre ; Porcher']", "authors_corrected"] <- 
  "['Bougnères; P', 'Porcher; R', 'Esterle; L', 'Baker; D', 'Vaissière; A', 'Meurisse; S', 'Valtat; S', 'Castell; A L', 'Mouriquand; P', 'Valleron; A J', 'Bougnères; R', 'Esterle; L']"
aut_check$authors_corrected[105]

aut_check[aut_check$authors == "['Pavlík, Jan', 'Hrn?írová, Markéta', 'Sto?es, Michal', 'Masner, Jan', 'Van?k, Ji?í']", "authors_corrected"] <- "['Pavlík; J', 'Hrnčírová; M', 'Stočes; M', 'Masner; J', 'Vaněk; J']"
# Jan Pavlík, Markéta Hrnčírová, Michal Stočes, Jan Masner, Jiří Vaněk 
aut_check$authors_corrected[107]

aut_check[aut_check$authors == "['Štolbová, M', 'Mí?ová, M']", "authors_corrected"] <- "['Štolbová; M', 'Míčová; M']"
# Michala Míčová
aut_check$authors_corrected[122]

aut_check[aut_check$authors == "['Andrade, Camila', 'Villers, Alexandre', 'Balent, Gérard', 'Bar?Hen, Avner', 'Chadoeuf, Joël', 'Cylly, Daniel', 'Cluzeau, Daniel', 'Fried, Guillaume', 'Guillocheau, Sarah', 'Pillon, Olivier', 'Porcher, Emmanuelle', 'Tressou, Jessica', 'Yamada, Ohri', 'Lenne, Nicolas', 'Jullien, Jérôme', 'Monestiez, Pascal']", "authors_corrected"] <- 
  "['Andrade; C', 'Villers; A', 'Balent; G', 'Bar-Hen; A', 'Chadoeuf; J', 'Cylly; D', 'Cluzeau; D', 'Fried; G', 'Guillocheau; S', 'Pillon; O', 'Porcher; E', 'Tressou; J', 'Yamada; O', 'Lenne; N', 'Jullien; J', 'Monestiez; P']"
# Avner Bar-Hen
aut_check$authors_corrected[124]

aut_check[aut_check$authors == "['Kloucek, Tomas', 'Moravec, David', 'Komarek, Jan', 'Lagner, Ondrej', 'Stych, Premysl']", "authors_corrected"] <- 
  "['Klouček; T', 'Moravec; D', 'Komárek; J', 'Lagner; O', 'Štych; P']"
aut_check$authors_corrected[130]

aut_check[aut_check$authors == "['Jost, Elisabeth', 'Schönhart, Martin', 'Skalský, Rastislav', 'Balkovi?, Juraj', 'Schmid, Erwin', 'Mitter, Hermine']", "authors_corrected"] <- 
  "['Jost; E', 'Schönhart; M', 'Skalský; R', 'Balkovič; J', 'Schmid; E', 'Mitter; H']"
# Juraj Balkovič 
aut_check$authors_corrected[133]

aut_check[aut_check$authors == "['Vaudour, Emmanuelle', 'Gilliot, Jean-Marc', 'Bel, Liliane', 'Lefebvre, J', 'Chehdi, K']", "authors_corrected"] <- 
  "['Vaudour; E', 'Gilliot; J M', 'Bel; L', 'Lefevre; J', 'Chehdi; K']"
#J. Lefevre
aut_check$authors_corrected[139]

aut_check[aut_check$authors == "['Tournayre, Orianne', 'Leuchtmann, Maxime', 'Galan, Maxime', 'Trillat, Marine', 'Piry, Sylvain', 'Pinaud, David', 'Filippi?Codaccioni, Ondine', 'Pontier, Dominique', 'Charbonnel, Nathalie']", "authors_corrected"] <- 
  "['Tournayre; O', 'Leuchtmann; M', 'Galan; M', 'Trillat; M', 'Piry; S', 'Pinaud; D', 'Filippi-Codaccioni; O', 'Pontier; D', 'Charbonnel; N']"
# Ondine Filippi-Codaccioni
aut_check$authors_corrected[142]

aut_check[aut_check$authors == "['Ženka, Jan', 'Slach, Ond?ej', 'Krti?ka, Lud?k', 'Žufan, Petr']", "authors_corrected"] <- 
  "['Ženka; J', 'Slach; O', 'Krtička; L', 'Žufan; P']"
# Jan Ženka, Ondřej Slach, Luděk Krtička, Petr Žufan
aut_check$authors_corrected[143]

aut_check[aut_check$authors == "['Bauer, Miroslav', 'Dostal, Tomas', 'Krasa, Josef', 'Jachymova, Barbora', 'Vaclav, David', 'Devaty, Jan', 'Strouhal, Ludek', 'Rosendorf, Pavel']", "authors_corrected"] <- 
  "['Bauer; M', 'Dostál; T', 'Krása; J', 'Jachymova; B', 'Vaclav; D', 'Devaty; J', 'Strouhal; L', 'Rosendorf; P']"
aut_check$authors_corrected[145]

aut_check[aut_check$authors == "['Fucík, Petr', 'Novák, Pavel', 'ízala, Daniel']", "authors_corrected"] <- "['Fučík; P', 'Novák; P', 'Žížala; D']"
#Daniel Žížala 
aut_check$authors_corrected[174]

aut_check[aut_check$authors == "['Bedná?, Marek', 'Šarapatka, Bo?ivoj']", "authors_corrected"] <- "['Bednář; M', 'Šarapatka; B']"
# Marek Bednář, Bořivoj Šarapatka
aut_check$authors_corrected[188]

aut_check[aut_check$authors == "['Vincikova, Hana', 'Prochazka, Jan', 'Brom, Jakub']", "authors_corrected"] <- "['Vinciková; H', 'Procházka; J', 'Brom; J']"
aut_check$authors_corrected[190]

aut_check[aut_check$authors == "['Kone?ná, Jana', 'Karásek, Petr', 'Fu?ík, Petr', 'Podhrázská, Jana', 'Pochop, Michal', 'Ryšavý, Stanislav', 'Hanák, Roman']", "authors_corrected"] <- 
  "['Konečná; J', 'Karásek; P', 'Fučík; P', 'Podhrázská; J', 'Pochop; M', 'Ryšavý; S', 'Hanák; R']"
# Jana Konečná, Petr Karásek, Petr Fučík, Jana Podhrázská, Michal Pochop, Stanislav Ryšavý und Roman Hanák
aut_check$authors_corrected[191]

aut_check[aut_check$authors == "['Sliwinski, Katharina', 'Ronnenberg, Katrin', 'Jung, Klaus', 'Strauss, Egbert', 'Siebert, Ursula']", "authors_corrected"] <- 
  "['Sliwinski; K', 'Ronnenberg; K', 'Jung; K', 'Strauß; E', 'Siebert; U']"
aut_check$authors_corrected[199]

aut_check[aut_check$authors == "['Klou_ek  David ; Komárek, Jan ; Lagner, Ond_ej ; _tych, P_emysl, Tomá_ ; Moravec', '_tych, Tomá_ Klou_ek ; David Moravec ; Jan Komárek ; Ond_ej Lagner ; P_emysl', 'Klou_ek  David ; Komárek, Jan ; Lagner, Ond_ej ; _tych, P_emysl, Tomá_ ; Moravec', 'Klou?ek, Tomáš', 'Moravec, David', 'Komárek, Jan', 'Lagner, Ond?ej', 'Štych, P?emysl']", "authors_corrected"] <-
  "['Klouček; T', 'Moravec; D', 'Komárek; J', 'Lagner; O', 'Štych; P']"
# Tomáš Klouček, David Moravec, Jan Komárek, Ondřej Lagner, Přemysl Štych
aut_check$authors_corrected[201]

aut_check[aut_check$authors == "['Kandziora, Marion', 'Burkhard, Benjamin', 'Mueller, Felix']", "authors_corrected"] <- "['Kandziora; M', 'Burkhard; B', 'Müller; F']"
aut_check$authors_corrected[203]

aut_check[aut_check$authors == "['Delattre, Laurence', 'Debolini, Marta', 'Paoli, Jean Christophe', 'Napoleone, Claude', 'Moulery, Michel', 'Leonelli, Lara', 'Santucci, Pierre']", "authors_corrected"] <-
  "['Delattre; L', 'Debolini; M', 'Paoli; J C', 'Napoléone; C', 'Moulery; M', 'Leonelli; L', 'Santucci; P']"
aut_check$authors_corrected[205]

aut_check[aut_check$authors == "['Lomba  Michael ; Jerrentrup, Sabrina ; Dauber, Jens ; Klimek, Sebastian ; McCracken, David I., Angela ; Strohbach']", "authors_corrected"] <-
  "['Lomba; A', 'Strohbach; M W', 'Jerrentrup; J S', 'Dauber; J', 'Klimek; S', 'McCracken; D I']"
# Angela Lomba, Michael Strohbach, J. Sabrina Jerrentrup, Jens Dauber, Sebastian Klimek , David I. McCracken
aut_check$authors_corrected[213]

aut_check[aut_check$authors == "['Marais Sicre, C', 'Fieuzal, R', 'Baup, F']", "authors_corrected"] <- "['Marais-Sicre; C', 'Fieuzal; R', 'Baup; F']"
aut_check$authors_corrected[217]

aut_check[aut_check$authors == "['MURGUE  Romain ; Vavasseur, Maroussia ; Burger-Leenhardt, Delphine ; Therond, Olivier, Clément ; Lardy']", "authors_corrected"] <-
  "['Murgue; C', 'Lardy; R', 'Vavasseur; M', 'Burger-Leenhardt; D D', 'Therond; O']"
# Clément Murgue, Romain Lardy, Maroussia Vavasseur, Delphine D. Burger-Leenhardt, Olivier Therond
aut_check$authors_corrected[221]

aut_check[aut_check$authors == "['Rivers-Moore, Justine', 'Andrieu, Emilie', 'Vialatte, Aude', 'Ouin, Annie']", "authors_corrected"] <-
  "['Rivers-Moore; J', 'Andrieu; E', 'Vialatte; A', 'Ouin; A']"
aut_check$authors_corrected[227]

aut_check[aut_check$authors == "['Holá, Michaela', 'Zíka, Tomás', 'Sálek, Miroslav', 'Hanzal, Vladimír', 'Kusta, Tomás', 'Jezek, Milos', 'Hart, Vlastimil']", "authors_corrected"] <-
  "['Holá; M', 'Zíka; T', 'Salek; M', 'Hanzal; V', 'Kusta; T', 'Jezek; M', 'Hart; V']"
aut_check$authors_corrected[228]

aut_check[aut_check$authors == "['Sicre, Claire Marais', 'Tallec, Tiphaine']", "authors_corrected"] <- "['Marais-Sicre; C', 'Tallec; T']"
aut_check$authors_corrected[229]

aut_check[aut_check$authors == "['Schoenhart, Martin', 'Schauppenlehner, Thomas', 'Schmid, Erwin', 'Muhar, Andreas']", "authors_corrected"] <- "['Schönhart; M', 'Schauppenlehner; T', 'Schmid; E', 'Muhar; A']"
aut_check$authors_corrected[231]

aut_check[aut_check$authors == "['Bohan, David A', 'Schmucki, Reto', 'Abay, Abrha T', 'Termansen, Mette', 'Bane, Miranda', 'Charalabidis, Alice', 'Cong, Rong-Gang', 'Derocles, Stephane A P', 'Dorner, Zita', 'Forster, Matthieu', 'Gibert, Caroline', 'Harrower, Colin', 'Oudoire, Geoffroy', 'Therond, Olivier', 'Young, Juliette', 'Zalai, Mihály', 'Pocock, Michael J O B T - Advances in Ecological Research']", "authors_corrected"] <- 
  "['Bohan; D A', 'Schmucki; R', 'Abay; A T', 'Termansen; M', 'Bane; M', 'Charalabidis; A', 'Cong; R G', 'Dorner; Z', 'Derocles; S A P', 'Förster; M', 'Gibert; C', 'Harrower; C', 'Oudoire; G', 'Therond; O', 'Young; J', 'Zalai; M', 'Pocock; M J O']"
#David A. Bohan, Reto Schmucki, Abrha T. Abay, Mette Termansen, Miranda Bane, Alice Charalabidis, Rong-Gang Cong, Stephane A.P. Derocles, Zita Dorner, Matthieu Forster, Caroline Gibert, Colin Harrower, Geoffroy Oudoire, Olivier Therond, Juliette Young, Mihály Zalai, Michael J.O. Pocock
aut_check$authors_corrected[235]

aut_check[aut_check$authors == "['Brychta, Ji?í', 'Jane?ek, Miloslav', 'Walmsley, Alena']", "authors_corrected"] <-
  "['Brychta; J', 'Janeček; M', 'Walmsley; A']"
# Jiri Brychta, Miloslav Janeček, Alena Walmsley Roubíčková
aut_check$authors_corrected[242]

aut_check[aut_check$authors == "['Jachymova, Barbora', 'Krasa, Josef']", "authors_corrected"] <- "['Jachymova; B', 'Krása; J']"
aut_check$authors_corrected[253]

aut_check[aut_check$authors == "['Matutini, Florence', 'Baudry, Jacques', 'Pain, Guillaume', 'Sineau, Morgane', 'Pithon, Joséphine']", "authors_corrected"] <- 
  "['Matutini; F', 'Baudry; J', 'Pain; G', 'Sineau; M', 'Pithon; J A']"
aut_check$authors_corrected[264]

aut_check[aut_check$authors == "['Pfingstmann, Alexandra', 'Paredes, Daniel', 'Buchholz, Jacob', 'Querner, Pascal', 'Bauer, Thomas', 'Strauss, Peter', 'Kratschmer, Sophie', 'Winter, Silvia', 'Zaller, Johann']", "authors_corrected"] <-
  "['Pfingstmann; A', 'Paredes; D', 'Buchholz; J', 'Querner; P', 'Bauer; T', 'Strauss; P', 'Kratschmer; S', 'Winter; S', 'Zaller; J G']"
aut_check$authors_corrected[269]

aut_check[aut_check$authors == "['Bogaerts, Theo', 'Williamson, Ian P', 'Fendel, Elfriede M']", "authors_corrected"] <-
  "['Bogaerts; T', 'Williamson; I P', 'Fendel; E M']"
# Theo Bogaerts, Ian P. Williamson, Elfriede M. Fendel
aut_check$authors_corrected[272]

aut_check[aut_check$authors == "['Inan, Hall Ibrahim', 'Sagris, Valentina', 'Devos, Wim', 'Milenov, Pavel', 'van Oosterom, Peter', 'Zevenbergen, Jaap']", "authors_corrected"] <-
  "['Inan; H I', 'Sagris; V', 'Devos; W', 'Milenov; P', 'van Oosterom; P', 'Zevenbergen; J']"
aut_check$authors_corrected[273]

aut_check[aut_check$authors == "['Neumann, Barbara', 'Lütz, Michael', 'Schüpbach, Beatrice', 'Szerencsits, Erich']", "authors_corrected"] <-
  "['Neumann; B', 'Lütz; M', 'Schüpbach; B', 'Szerencsits; E']"
aut_check$authors_corrected[275]

aut_check[aut_check$authors == "['Karner, Katrin', 'Schmid, Erwin', 'Schneider, Uwe A', 'Mitter, Hermine']", "authors_corrected"] <-
  "['Karner; K', 'Schmid; E', 'Schneider; U A', 'Mitter; H']"
aut_check$authors_corrected[276]

aut_check[aut_check$authors == "['Podhrázská, J', 'Kučera, J', 'Karásek, P', 'Konečná, J']", "authors_corrected"] <-
  "['Podhrázská; J', 'Kučera; J', 'Karásek; P', 'Konečná; J']"
aut_check$authors_corrected[277]

aut_check[aut_check$authors == "['Lundin, Ola', 'Rundlöf, Maj', 'Smith, Henrik G', 'Bommarco, Riccardo']", "authors_corrected"] <-
  "['Lundin; O', 'Rundlöf; M', 'Smith; H G', 'Bommarco; R']"
aut_check$authors_corrected[278]

aut_check[aut_check$authors == "['Demarez, Valérie', 'Florian, Helen', 'Marais-Sicre, Claire', 'Baup, Frédéric']", "authors_corrected"] <-
  "['Demarez; V', 'Florian; H', 'Marais-Sicre; C', 'Baup; F']"
aut_check$authors_corrected[279]