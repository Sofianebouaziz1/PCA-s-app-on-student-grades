library("readxl")
data = read_excel("NotesPVS1-S2.xlsx", sheet=2) # 2 Represente le numÃ©ro de la feuille du fichier excel
library(dplyr)

data = select(data,-starts_with("...")) # supprimer les colonnes qui commencent par ...
data = select(data, -c('Matricule', `Nom & Prénom`, 'Groupe', `Ne S1`, `Rang S1`))


data = plyr::rename(data, c(`SYS1 * x 5` = 'SYS1', `RES1 * x 4` = 'RES1', `ANUM * x 4` = 'ANUM',  `RO * x 3` = 'RO', `ORG * x 3` = 'ORG', `IGL * x 5` = 'IGL', `THP * x 4` = 'THP', `LANG1 * x 2` = 'LANG', `Moy S1` = 'MoyS1'))


data$SYS1 = gsub("[A-Z0-9]+: ([0-9\\.]+) < [0-9\\.]+", "\\1", data$SYS1)
data$RES1 = gsub("[A-Z0-9]+: ([0-9\\.]+) < [0-9\\.]+", "\\1", data$RES1)   
data$ANUM = gsub("[A-Z0-9]+: ([0-9\\.]+) < [0-9\\.]+", "\\1", data$ANUM) 
data$RO = gsub("[A-Z0-9]+: ([0-9\\.]+) < [0-9\\.]+", "\\1", data$RO) 
data$ORG = gsub("[A-Z0-9]+: ([0-9\\.]+) < [0-9\\.]+", "\\1", data$ORG) 
data$IGL= gsub("[A-Z0-9]+: ([0-9\\.]+) < [0-9\\.]+", "\\1", data$IGL) 
data$THP= gsub("[A-Z0-9]+: ([0-9\\.]+) < [0-9\\.]+", "\\1", data$THP) 
data$LANG= gsub("[A-Z0-9]+: ([0-9\\.]+) < [0-9\\.]+", "\\1", data$LANG)  

data = na.omit(data) #supprimer les donnÃ©es manquantes
data = as.data.frame(apply(data, 2, as.numeric)) #conversion des type des colonnes en numeric

summary(data) #Avoir des statistique concernant le tableau de données


library(FactoMineR)
acp = PCA(data[-9]) #Toutes les colonnes à part la derniere qui represente MoyS1
summary(acp) 

install.packages("factoextra")
library("factoextra")
fviz_eig(acp)#Representation de l'eboulis des valeurs propores

#cos2 : represente la qualité de representation des individus sur le plan factoriel engendré par l'axe 1 et 2
fviz_pca_ind(acp, col.ind = "cos2",  gradient.cols = c("#00AFBB", "blue", "red")) 
