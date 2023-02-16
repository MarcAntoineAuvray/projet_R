# package pour lancer une appli shiny
library(shiny)
# package pour faire des graphiques
library(ggplot2)
# package pour importer des fichier xml
library(xml2)

# effacer les variables de l'environnement :
rm(list=ls())

# lancer le code
source("fonctions.R")

# recuperer les donnees presentes dans chaque fichier (principales, population, raffineries)
df = readRDS("dataframe.rds")
df_pop = readRDS("dataframe_pop.rds")
df_raffineries = load_rafineries()

# nom du Rmd où le dataframe est construit
nom_rmd = "auvray_marcantoine_projetR"

# pour recharger toutes les donnees (temps d execution long)
recharger = FALSE
if (recharger) {
  rmarkdown::render(input = paste0(nom_rmd, ".Rmd"), 
                    output_file = sprintf(paste0(nom_rmd, ".html")))
  df = readRDS("dataframe.rds")
  df_pop = add_pop(df, file="insee_rp_hist_1968.xlsx", file_codes="laposte_hexasmal.csv")
  saveRDS(df_pop, "dataframe_pop.rds")
}

# recuperer le knit html du Rmarkdown sur la construction des données
texte_presentation = HTML(as.character(read_html(paste0(nom_rmd, ".html"))))

# relancer le code du Rmarkown sur les cartes
rmarkdown::render(input = "cartes.Rmd", 
                  output_file = sprintf("cartes.html"))

# recuperer le contenu html du knit html du Rmarkdown sur les cartes
texte_carte = HTML(as.character(read_html("cartes.html")))

# liste des onglets
ui <- fluidPage(tabsetPanel(tabPanel("Presentation",
                                     texte_presentation),
                            
                            tabPanel("Analyse unidimensionnelle",
                                     selectInput("variable", 
                                                 "Choisir une variable :", 
                                                 names(df)),
                                     radioButtons("plot_type", 
                                                  "Choississez le type de graphique",
                                                  c("Histogramme"="hist",
                                                    "Boites a moustaches"="boxplot",
                                                    "Diagramme de densite"="density",
                                                    "Camembert"="pie",
                                                    "Texte"="texte"),
                                                  "hist"),
                                     plotOutput("graph_uni")),
                            
                            tabPanel("Analyse bidimensionnelle",
                                     selectInput("xcol", "Colonne pour l'axe x", names(df)),
                                     selectInput("ycol", "Colonne pour l'axe y", names(df)),
                                     radioButtons("plot_type_2", 
                                                  "Choississez le type de graphique",
                                                  c("Histogramme"="hist",
                                                    "Camembert"="pie",
                                                    "Diagramme de densité"="density", 
                                                    "Nuage de points"="scatter"),
                                                  "hist"),
                                     plotOutput("graph_bi")),
                            
                            tabPanel("Cartographie",
                                     texte_carte),
                            
                            
                            tabPanel("Régression linéaire", 
                                     selectInput("nombre_tests", "Nombre d'individus à tester", c(1:10)),
                                     radioButtons("plot_type_3", 
                                                  "Choississez ce que vous voulez afficher",
                                                  c("Tableau de comparaison"="texte_2",
                                                    "Nuage de points"="scatter"),
                                                  "texte_2"),
                                     plotOutput("graph_machine_learning"))))

# liste des sorties de l'appli shiny
server <- function(input, output, session) {
  
  liste_tri_colonnes_noms = tri_colonnes_noms(df)
  booleen = unlist(liste_tri_colonnes_noms$booleen)
  numerique_continu = unlist(liste_tri_colonnes_noms$numerique_continu)
  numerique_discret = unlist(liste_tri_colonnes_noms$numerique_discret)
  caractere_continu = unlist(liste_tri_colonnes_noms$caractere_continu)
  caractere_discret = unlist(liste_tri_colonnes_noms$caractere_discret)
  
  output$graph_uni <- renderPlot({
    
    exception<-function(input_variable, input_plot_type){
      return(text(x = 0.5, y = 0.5,
                  paste0("Pas d'affichage possible pour représenter la variable :\n", input_variable,
                         ",\nde type :\n", typeof(df[ ,input_variable]),
                         ",\nà travers un graphique de type :\n", input_plot_type, "."),
                  cex = 1.6, col = "black", adj = 0.5, font = 2))}
    
    if (input$plot_type == "hist") {
      if (input$variable %in% c(numerique_continu, numerique_discret)) {
        ggplot(df, aes_string(x = input$variable)) + geom_histogram(bins = 30)
      } else { exception(input_variable=input$variable, input_plot_type=input$plot_type) }
      
    } else if (input$plot_type == "boxplot") {
      if (input$variable %in% c(numerique_continu, numerique_discret)) {
        ggplot(df, aes_string(x = "1", y = input$variable)) + geom_boxplot()
      } else { exception(input_variable=input$variable, input_plot_type=input$plot_type) }
      
    } else if (input$plot_type == "density") {
      if (input$variable %in% c(numerique_continu, numerique_discret)) {
        ggplot(df, aes_string(x = input$variable)) + geom_density()
      } else { exception(input_variable=input$variable, input_plot_type=input$plot_type) }
      
    } else if (input$plot_type == "pie") {
      if (input$variable %in% c(booleen, caractere_discret, numerique_discret)) {
        
        if (input$variable %in% c(caractere_discret, numerique_discret)){
          df_character = data.frame(table(df[,input$variable]))
          colnames(df_character) = c("Groupes", "Nombres")
          ggplot(df_character, aes(x="", y=Nombres, fill=Groupes))+geom_bar(width=1, stat="identity")+coord_polar("y", start=0)+labs(title="Camembert", x="", y="")+theme_classic()
          
        } else if (input$variable %in% booleen){
          df_logical = data.frame(Groupes = c("Nombre de TRUEs", "Nombre de FALSEs"),
                                  Nombres = c(sum(df[,input$variable]), length(df[,input$variable])-sum(df[,input$variable])))
          ggplot(df_logical, aes(x="", y=Nombres, fill=Groupes))+geom_bar(width=1, stat="identity")+coord_polar("y", start=0)+labs(title="Camembert", x="", y="")+theme_classic()
          
        } 
      } else { exception(input_variable=input$variable, input_plot_type=input$plot_type) }
      
    } else if (input$plot_type == "texte") {
      if(input$variable %in%  c(caractere_continu)){
        text(x=0.5,
             y=0.5,
             paste0("Nombre de valeurs différentes: ", 
                    length(unique(df[,input$variable])), 
                    "\nLongueur du dataframe: ", 
                    nrow(df)),
             cex=1.6,
             col="black")
      } else {
        text(x=0.5,
             y=0.5,
             paste0("Résumé de ", input$variable," : \n",
                    paste(capture.output(summary(df[,input$variable])), collapse="\n")),
             cex=1.6,
             col="black")
      }
    }
  })
  
  output$graph_bi <- renderPlot({
    # req(input$xcol, input$ycol)
    
    exception_2<-function(input_xcol, input_ycol, input_plot_type){
      return(text(x = 0.5, y = 0.5,
                  paste0("Pas d'affichage possible pour représenter la variable x :\n", input_xcol,
                         ",\nde type :\n", typeof(df[ ,input_xcol]),
                         ",\nen fonction de la variable y :\n", input_ycol,
                         ",\nde type :\n", typeof(df[ ,input_ycol]),
                         ",\nà travers un graphique de type :\n", input_plot_type, "."),
                  cex = 1.6, col = "black", adj = 0.5, font = 2))}
    
    
    if(input$plot_type_2=="hist"){
      if (input$xcol %in% numerique_continu) {
        if (input$ycol %in% numerique_discret) {
          ggplot(df, aes(x = input$xcol, fill = input$ycol)) + geom_bar()
        } else {
          exception_2(input_xcol=input$xcol, input_ycol=input$ycol, input_plot_type=input$plot_type_2)
        }
      } else {
        exception_2(input_xcol=input$xcol, input_ycol=input$ycol, input_plot_type=input$plot_type_2)
      }
      
    }else if(input$plot_type_2=="pie"){
      if (input$xcol %in% numerique_discret) {
        ggplot(df, aes(x = input$xcol)) + geom_bar(aes(fill = input$ycol), width = 1, stat = "count") + coord_polar("y", start = 0)
      } else {
        exception_2(input_xcol=input$xcol, input_ycol=input$ycol, input_plot_type=input$plot_type_2)
      }
      
    }else if(input$plot_type_2=="density"){
      if (input$xcol %in% numerique_continu) {
        ggplot(df, aes(x = input$xcol, color = input$ycol)) + geom_density()
      } else {
        exception_2(input_xcol=input$xcol, input_ycol=input$ycol, input_plot_type=input$plot_type_2)
      }
      
    }else if(input$plot_type_2 == "scatter") {
      if(input$xcol %in% c(numerique_discret, numerique_continu) && input$ycol %in% c(numerique_discret, numerique_continu)) {
        ggplot(df, aes_string(x=input$xcol, y=input$ycol))+geom_point()
      } else {
        text(x = 0.5, y = 0.5,
             paste0("Pas d'affichage possible pour représenter la variable x :\n", input$xcol,
                    ",\nde type :\n", typeof(df[,input$xcol]),
                    ",\nen fonction de la variable y :\n", input$ycol,
                    ",\nde type :\n", typeof(df[,input$ycol]),
                    ",\nà travers un graphique de type :\n Scatter."),
             cex = 1.6, col = "black", adj = 0.5, font = 2)
      }
    }
  })
  
  
  output$graph_machine_learning <- renderPlot({ 
    variables_explicatives = df[, c(numerique_continu, numerique_discret)]
    tablo_<-function(df, nombre_tests, prix, nom_model) {
      tablo = table_prediction(df=df, nombre_tests=nombre_tests, prix=prix, nom_model=nom_model)
      return(tablo)
      }
    
    if (input$plot_type_3 == "texte_2") {
      tablo = tablo_(df=variables_explicatives, nombre_tests=input$nombre_tests, prix=prix, nom_model="modele_lineaire")
      text(x = 0.5, y = 0.5,
           paste(capture.output(tablo),
                 collapse="\n"),
           cex = 1.6, col = "black", adj = 0.5, font = 1)
      
    } else if (input$plot_type_3 == "scatter") {
      tablo = tablo_(df=variables_explicatives, nombre_tests=input$nombre_tests, prix=prix, nom_model="modele_lineaire")
      ggplot(tablo, aes(x=observations_y , y=predictions_y))+geom_point()
    }
    })
}

# lancement de l appli
shinyApp(ui, 
         server, 
         options = list(launch.browser = TRUE))
