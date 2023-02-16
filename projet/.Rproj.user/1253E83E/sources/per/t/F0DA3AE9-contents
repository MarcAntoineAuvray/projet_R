# arguments : un d²
tri_colonnes_noms<-function(df, n_levels=50){
  booleen = c()
  numerique_continu = c()
  numerique_discret = c()
  caractere_continu = c()
  caractere_discret = c()
  
  for (nom_colonne in colnames(df)) {
    colonne = df[, nom_colonne]
    if (is.logical(colonne)) {
      booleen = c(booleen, nom_colonne)
    }
    
    if (is.character(colonne)) {
      if (length(unique(colonne))<n_levels){
        caractere_discret = c(caractere_discret, nom_colonne)
      } else{
        caractere_continu = c(caractere_continu, nom_colonne)
      }
    }
    
    if (is.numeric(colonne)) {
      if (length(unique(colonne))<n_levels){
        numerique_discret = c(numerique_discret, nom_colonne)
      } else{
        numerique_continu = c(numerique_continu, nom_colonne)
      }
    }
  }
  
  liste = list("booleen"=booleen,
               "numerique_continu"=numerique_continu,
               "numerique_discret"=numerique_discret,
               "caractere_continu"=caractere_continu,
               "caractere_discret"=caractere_discret)
  return(liste)
}


library(caret)
library(dplyr)


split_<-function(df, nombre_tests){
  suppressMessages(df_test <- df %>% sample_n(nombre_tests))
  suppressMessages(df_train <- df %>% anti_join(df_test))
  
  return(list("df_train"=df_train,
              "df_test"=df_test))
}

modele_lineaire <- function(df_train, prix=prix) {
  modele_fit <- train(prix ~ ., data=df_train, method="lm")
  return(modele_fit)
}

table_prediction<-function(df, nombre_tests, prix=prix, nom_model){
  liste_train_test = split_(df=df, nombre_tests=nombre_tests)
  
  df_train = liste_train_test$df_train
  df_test = liste_train_test$df_test
  
  if (nom_model=="modele_lineaire") {
    modele_fit = modele_lineaire(df=df_train, prix=prix)
  }
  colnames(df_test)[colnames(df_test) == as.character(substitute(prix))] <- "y_observés"
  tablo =  data.frame("observations_y"=df_test$"y_observés",
                      "predictions_y"=predict(modele_fit, df_test))
  return(tablo)
}

# donnees pop :
# https://www.observatoire-des-territoires.gouv.fr/population-au-dernier-recensement
# il est difficile de trouver un jeu de donnees complet sur les populations municipales 
# on va se contenter du maximum :
load_pop_municip<-function(file="insee_rp_hist_1968.xlsx", file_codes="laposte_hexasmal.csv"){
  library("readxl")
  pop_municip = data.frame(read_excel("insee_rp_hist_1968.xlsx", skip=4))
  colnames(pop_municip) = c("codgeo", "nom_ville","annee", "nombre_habitants") # "codgeo" "libgeo" "an"     "p_pop" 
  pop_municip$annee = as.numeric(pop_municip$annee)
  pop_municip$nombre_habitants = as.numeric(pop_municip$nombre_habitants)
  Encoding(pop_municip$nom_ville) <- "UTF-8"
  
  codes_cp_commun = read.csv(file_codes, sep=";")[, c("Code_commune_INSEE", "Code_postal")]
  colnames(codes_cp_commun) = c("codgeo", "cp")
  
  pop_municip = merge(pop_municip, codes_cp_commun)
  
  df_max_year <- aggregate(pop_municip$annee, by=list(pop_municip$cp), max)
  colnames(df_max_year) = c("cp", "annee")
  df_merged <- merge(pop_municip, df_max_year)
  
  pop_municip = aggregate(pop_municip$nombre_habitants, by=list(pop_municip$cp), max )
  colnames(pop_municip) = c("cp", "nombre_habitants")
  return(pop_municip)
}

add_pop<-function(df, file="insee_rp_hist_1968.xlsx", file_codes="laposte_hexasmal.csv"){
  df_pop_municip = load_pop_municip(file="insee_rp_hist_1968.xlsx", file_codes="laposte_hexasmal.csv")[, c("cp","nombre_habitants")]
  
  merge_ = merge(df, df_pop_municip, all.x = TRUE)
  
  return(merge_)
}


load_rafineries<-function(){
  nom = c("Raffinerie de Normandie" ,
           "Raffinerie de Donges",
           "Raffinerie de Port-Jérôme-Gravenchon",
           "Raffinerie de Lavéra",
           "Plateforme de la Mède ",
           "Raffinerie de Feyzin",
           "Raffinerie de Fos",
           "Raffinerie de Grandpuits")
  
  latitude = c(49.4752211,47.3135413,49.4752211,43.4074,43.39819,45.6657132,43.45065,48.5922882)
  longitude = c(0.5528246,-2.0651766,0.5528246,5.0553,5.11637,4.8409521,4.9239384,2.9486286)
  
  df_raffineries = data.frame(nom ,latitude,longitude)
  return(df_raffineries)
}

