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
    data_uncertainty <- data
    return(data_uncertainty)
  } 
  else if(month=="last"){
    data_uncertainty <- import_un_mois(string_2017_onward()[length(string_2017_onward())])
    return(data_uncertainty)
  } 
  else {
    print("Error: the month parameter should be either 'all' or 'last'")
  }
    
}

gere_iso <- function(data){
  data <- data[data$DECLARANT_ISO !="QQ"&data$DECLARANT_ISO !="QU"&
               data$DECLARANT_ISO !="QV"&data$DECLARANT_ISO !="QW"&
               data$DECLARANT_ISO !="TP"&data$DECLARANT_ISO !="XA"&
               data$DECLARANT_ISO !="XC"&data$DECLARANT_ISO !="XE"&
               data$DECLARANT_ISO !="XF"&data$DECLARANT_ISO !="XG"&
               data$DECLARANT_ISO !="XH"&data$DECLARANT_ISO !="XI"&
               data$DECLARANT_ISO !="XK"&data$DECLARANT_ISO !="XL"&
               data$DECLARANT_ISO !="XM"&data$DECLARANT_ISO !="XO"&
               data$DECLARANT_ISO !="XP"&data$DECLARANT_ISO !="XR"&
               data$DECLARANT_ISO !="XS"&data$DECLARANT_ISO !="FR"&
               data$DECLARANT_ISO !="EU"&data$DECLARANT_ISO !="GL"&
               data$DECLARANT_ISO !="PA"&data$DECLARANT_ISO !="XX",]
  
  data <- data[data$PARTNER_ISO !="QQ"&data$PARTNER_ISO !="QU"&
               data$PARTNER_ISO !="QV"&data$PARTNER_ISO !="QW"&
               data$PARTNER_ISO !="TP"&data$PARTNER_ISO !="XA"&
               data$PARTNER_ISO !="XC"&data$PARTNER_ISO !="XE"&
               data$PARTNER_ISO !="XF"&data$PARTNER_ISO !="XG"&
               data$PARTNER_ISO !="XH"&data$PARTNER_ISO !="XI"&
               data$PARTNER_ISO !="XK"&data$PARTNER_ISO !="XL"&
               data$PARTNER_ISO !="XM"&data$PARTNER_ISO !="XO"&
               data$PARTNER_ISO !="XP"&data$PARTNER_ISO !="XR"&
               data$PARTNER_ISO !="XS"&data$PARTNER_ISO !="FR"&
               data$PARTNER_ISO !="EU"&data$PARTNER_ISO !="GL"&
               data$PARTNER_ISO !="PA"&data$PARTNER_ISO !="XX",]
  
  return(data)
}

gere_nc <- function(data){
  num_colonne<-which(colnames(data)=="PRODUCT_NC")
  for(i in length(data[,num_colonne])){
    if (length(data[i,num_colonne])==1){
      data[i,num_colonne]<-paste(data[i,num_colonne],"0000000",sep = "")
    }
    else if (length(data[i,num_colonne])==2){
      data[i,num_colonne]<-paste(data[i,num_colonne],"000000",sep = "")
    }
    else if (length(data[i,num_colonne])==3){
      data[i,num_colonne]<-paste(data[i,num_colonne],"00000",sep = "")
    }
    else if (length(data[i,num_colonne])==4){
      data[i,num_colonne]<-paste(data[i,num_colonne],"0000",sep = "")
    }
    else if (length(data[i,num_colonne])==5){
      data[i,num_colonne]<-paste(data[i,num_colonne],"000",sep = "")
    }
    else if (length(data[i,num_colonne])==6){
      data[i,num_colonne]<-paste(data[i,num_colonne],"00",sep = "")
    }
    else if (length(data[i,num_colonne])==7){
      data[i,num_colonne]<-paste(data[i,num_colonne],"0",sep = "")
    }
  }
  data <- data[data$PRODUCT_NC !="99500000"&data$PRODUCT_NC !="98807300"&
               data$PRODUCT_NC !="98808400"&data$PRODUCT_NC !="98809900"&
               data$PRODUCT_NC !="98808500"&data$PRODUCT_NC !="99699999"&
               data$PRODUCT_NC !="99050000"&data$PRODUCT_NC !="99190000"&
               data$PRODUCT_NC !="99300000"&data$PRODUCT_NC !="99310000"&
               data$PRODUCT_NC !="0000000",]
  return(data)
}

gere_flows <- function(data){
  data <- data[!(data$TRADE_TYPE=="I"&data$QUANTITY_IN_KG==0),]
  return(data)
}

conversion_nc <- function(data){
  data$PRODUCT_NC <- substring(data$PRODUCT_NC,1,6)
  return(data)
}

importe_et_nettoie <- function(month){
  data <- importe_data(month = month)
  data <- gere_iso(data)
  data <- gere_nc(data)
  data <- gere_flows(data)
  data <- conversion_nc(data)
  data_uncertainty <- data
  return(data_uncertainty)
}

importe_et_nettoie_test <- function(data){
  data <- gere_iso(data)
  data <- gere_nc(data)
  data <- gere_flows(data)
  data <- conversion_nc(data)
  return(data)
}

somme_month_month <- function(data){
  fabrice <- as.integer(levels(as.factor(data$PERIOD)))
  somme <- sum(data$VALUE_IN_EUROS[data$PERIOD==fabrice[1]&data$TRADE_TYPE!="I"])
  for(mois in fabrice[2:length(fabrice)]){
  somme <- cbind(somme,sum(data$VALUE_IN_EUROS[data$PERIOD==mois&data$TRADE_TYPE!="I"]))
  }
  return(somme)
}

growth_month_month <- function(data){
  data_bis <- somme_month_month(data)
  longueur <- seq(3, length(data_bis),1)
  growth <- (data_bis[1,2]-data_bis[1,1])/data_bis[1,1]
  for(i in longueur){
    growth <- cbind(growth,(data_bis[1,i]-data_bis[1,i-1])/data_bis[1,i-1])
  }
  colnames(growth)<-levels(as.factor(data$PERIOD))[2:4]
  return(growth)
}

growth_month_year_normal <- function(data){
  somme_par_mois <- somme_month_month(data)
  longueur <- seq(2, length(somme_par_mois),1)
  essai <- try((somme_par_mois[1,13]-somme_par_mois[1,1])/somme_par_mois[1,1],silent = T)
  if(class(essai)=="numeric"){ 
    growth <- (somme_par_mois[1,13]-somme_par_mois[1,1])/somme_par_mois[1,1]
    for(i in longueur-12){
      growth <- cbind(growth,(somme_par_mois[1,i+12]-somme_par_mois[1,i])/somme_par_mois[1,i])
      colnames(growth)[i] <- paste(as.integer(levels(as.factor(data$PERIOD)))[i],as.integer(levels(as.factor(data$PERIOD)))[i+12],sep = "/")
    }
  return(growth)
  }
  else{
  return("La base de données est plus courte qu'une année, nous ne pouvons donc pas étudier les variations
         des échanges d'une année sur l'autre")
  }
}

growth_month_year_midpoint <- function(data){
  somme_par_mois <- somme_month_month(data)
  longueur <- seq(2, length(somme_par_mois),1)
  essai <- try((somme_par_mois[1,13]-somme_par_mois[1,1])/mean(somme_par_mois[1,1],somme_par_mois[1,13]),silent = T)
  if(class(essai)=="numeric"){ 
    growth <- (somme_par_mois[1,13]-somme_par_mois[1,1])/mean(somme_par_mois[1,1],somme_par_mois[1,13])
    for(i in longueur[1:length(longueur)-12]){
      growth <- cbind(growth,(somme_par_mois[1,i+12]-somme_par_mois[1,i])/mean(somme_par_mois[1,i],somme_par_mois[1,i+12]))
      colnames(growth)[i] <- paste(as.integer(levels(as.factor(data$PERIOD)))[i],as.integer(levels(as.factor(data$PERIOD)))[i+12],sep = "/")
    }
    return(growth)
  }
  else{
    return("La base de données est plus courte qu'une année, nous ne pouvons donc pas étudier les variations
         des échanges d'une année sur l'autre")
  }
}

growth_month_year_log <- function(data){
  somme_par_mois <- somme_month_month(data)
  longueur <- seq(2, length(somme_par_mois), 1)
  essai <- try(log(somme_par_mois[1,13])-log(somme_par_mois[1,1]),silent = T)
  if(class(essai)=="numeric"){ 
    growth <- (log(somme_par_mois[1,13])-log(somme_par_mois[1,1]))
    for(i in longueur[1:length(longueur)-12]){
      growth <- cbind(growth,(log(somme_par_mois[1,i+12])-log(somme_par_mois[1,i])))
      colnames(growth)[i] <- paste(as.integer(levels(as.factor(data$PERIOD)))[i],as.integer(levels(as.factor(data$PERIOD)))[i+12],sep = "/")
    }
    return(growth)
  }
  else{
    return("La base de données est plus courte qu'une année, nous ne pouvons donc pas étudier les variations
         des échanges d'une année sur l'autre")
  }
}

compute_growth_rate <- function(data,growth="normal"){
  # growth can be either "month","normal","midpoint" or "delta_log"
  if(growth=="month"){
    return(growth_month_month(data))
  } 
  else if(growth=="normal"){
    return(growth_month_year_normal(data))
  }
  else if(growth=="midpoint"){
    return(growth_month_year_midpoint(data))
  }
  else if(growth=="delta_log"){
    return(growth_month_year_log(data))
  } 
  else {
    print("Error: growth can be either 'month','normal','midpoint' or 'delta_log'")
  }
}