library(archive)
library(rvest)
library(tidyverse)



tf <- tempfile() ; td <- tempdir()
file.path <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&downfile=comext%2FCOMEXT_DATA%2FPRODUCTS%2Ffull200301.7z"
download.file( file.path , tf , mode = "wb" )
data <- archive_extract(tf)
data2 <- read.table(data, sep = ",", header = T)

import_un_mois <- function(nom_du_fichier){
  tf <- tempfile() ; td <- tempdir()
  website <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&downfile=comext%2FCOMEXT_DATA%2FPRODUCTS%2F"
  file.path <- paste(website,nom_du_fichier,sep = "")
  download.file( file.path , tf , mode = "wb" )
  pre_data <- archive_extract(tf)
  data <- read.table(pre_data, sep = ",", header = T)
  return(data)
}
data2 <- import_un_mois("fullxixu202108.7z")
recupere_noms_mois <- function(){
  code_html_page<-read_html(url("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&dir=comext%2FCOMEXT_DATA%2FPRODUCTS"))
  string_1 <-pipe_et_jambe_de_bois %>% html_nodes("a") %>%html_text()
  string_2<- string_1[grepl("full",string_1)]
  return(string_2)
}
recupere_noms_mois()
verifie_validite_du_nom <- function(){
  
}

importe_data <- function(month ="all"){
  # file can be either "all" or "last
}

#answer <- c( content, wrapper) permet de append
#Roxygen package pour la doc des fonctions
#https://stackoverflow.com/questions/7187442/filter-a-vector-of-strings-based-on-string-matching

