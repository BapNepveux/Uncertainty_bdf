library(archive)
library(rvest)
library(tidyverse)




import_un_mois <- function(nom_du_fichier){
  tf <- tempfile() ; td <- tempdir()
  website <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&downfile=comext%2FCOMEXT_DATA%2FPRODUCTS%2F"
  file.path <- paste(website,nom_du_fichier,sep = "")
  download.file( file.path , tf , mode = "wb" )
  pre_data <- archive_extract(tf)
  data <- read.table(pre_data, sep = ",", header = T)
  return(data)
}


string_tous_mois <- function(){
  code_html_page<-read_html(url("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&dir=comext%2FCOMEXT_DATA%2FPRODUCTS"))
  string_1 <- code_html_page %>% html_nodes("a") %>%html_text()
  string_1 <- string_1[grepl("full",string_1)]
  string_1 <- string_1[!grepl("fullxixu",string_1)]
  string_1 <- string_1[!grepl("52.7z",string_1)]
  return(string_1)
}
string_2017_onward <- function(){
  alpha <- string_tous_mois()
  depart <- grepl("2017",alpha)
  indice <- which(depart)
  indice_depart <- indice[1]
  return(alpha[indice_depart:length(alpha)])
}
string_dernier_mois <- function(){
  string_1 <- string_2017_onward()
  return(string_1[length(string_1)])
}



importe_data <- function(month){
  # month can be either "all" or "last"
  if(month=="all"){
    data<- import_un_mois(string_2017_onward()[1])
    for (x in string_2017_onward()[2:length(string_2017_onward())]){
      solo <- import_un_mois(x)
      data <- rbind(data,solo)
    }
    data_uncertainty <<- data
  } else if(month=="last"){
    data_uncertainty <<- import_un_mois(string_2017_onward()[length(string_2017_onward())])
  } else {
    print("Error: the month parameter should be either 'all' or 'last'")
  }
    
}

#answer <- c( content, wrapper) permet de append
#Roxygen package pour la doc des fonctions
#https://stackoverflow.com/questions/7187442/filter-a-vector-of-strings-based-on-string-matching
importe_data(month = "last")
table(data_uncertainty$PERIOD)
# TESTS :
tf <- tempfile() ; td <- tempdir()
file.path <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&downfile=comext%2FCOMEXT_DATA%2FPRODUCTS%2Ffull200301.7z"
download.file( file.path , tf , mode = "wb" )
data <- archive_extract(tf)
data2 <- read.table(data, sep = ",", header = T)

data2 <- import_un_mois("fullxixu202108.7z")

alpha <- recupere_noms_mois()
depart <- grepl("2017",alpha)
indice <- which(depart)
indice_depart <- indice[1]
alpha[indice_depart:length(alpha)]
string_2017_onward()
final<- import_un_mois(string_2017_onward()[1])
 for (x in string_2017_onward()[2:3]){
  solo <- import_un_mois(x)
  final <- rbind(final,solo)
}

importe_data("all")
df1 <- import_un_mois("full201703.7z")
df2 <- import_un_mois("full201702.7z")
df_test <- rbind(df1,df2)
table(df_test$PERIOD)

