# Charger les bibliothèques nécessaires
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(lubridate)
library(hms)
library(leaflet)
library(sf)
library(readxl)
library(shinythemes)


# chargement des données:
conso_sup36= read_excel("conso-sup36-30juin region.xlsx")
# Supprimer les valeurs manquantes:
conso_sup36= conso_sup36[complete.cases(conso_sup36), ]

# Transformation du dataset pour conso36:

# df <- conso_sup36 %>%
#   mutate(
#     Horodate_clean = sub("\\+.*$", "", Horodate),  # Supprimer le fuseau horaire
#     Date = as.Date(sub("T.*$", "", Horodate_clean)),  # Extraire la date
#     Heure = format(ymd_hms(Horodate_clean), "%H:%M:%S"),  # Extraire l'heure
#     `Total énergie soutirée (Wh)` = as.numeric(`Total énergie soutirée (Wh)`),
#     `Courbe Moyenne n°1 (Wh)` = as.numeric(`Courbe Moyenne n°1 (Wh)`),
#     `Courbe Moyenne n°2 (Wh)` = as.numeric(`Courbe Moyenne n°2 (Wh)`)
#   ) %>%
#   select(c("Semaine max du mois (0/1)", "Jour max du mois (0/1)", Heure, Date, everything()))



# df<-readRDS("Prod_enrgies.rds")
# 
# df <- df %>%
#   mutate(
#     Horodate_clean = sub("\\+.*$", "", Horodate),
#     # Extraire la date
#     Date = as.Date(sub("T.*$", "", Horodate_clean)),
#     # Extraire l'heure
#     Heure = as_hms(sub("^.*T", "", Horodate_clean)),
#     Total.énergie.injectée..Wh.=as.numeric(Total.énergie.injectée..Wh.),
#     Courbe.Moyenne.n.1..Wh.=as.numeric(Courbe.Moyenne.n.1..Wh.),
#     Courbe.Moyenne.n.2..Wh.=as.numeric(Courbe.Moyenne.n.2..Wh.)
#   ) %>%
#   select(c(16:18,2:15))

# Charger les frontières des régions françaises depuis le fichier GeoJSON téléchargé
# Remarque : Assurez-vous que le fichier "regions.geojson" est dans votre répertoire de travail
regions_geo <- st_read("regions.geojson")  # Si le fichier est dans le même dossier que le script
regions_geo$code<-as.numeric(regions_geo$code)


# Fonction pour créer les graphiques des courbes de densité
courbe_moyenne_plots <- function() {
  # Créer les données pour les différentes distributions
  x_normal <- seq(-4, 4, length.out = 100)
  y_normal <- dnorm(x_normal)
  
  x_expo <- seq(0, 10, length.out = 100)
  y_expo <- dexp(x_expo)
  
  # Créer les courbes de densité
  p_normal <- ggplot(data.frame(x = x_normal, y = y_normal), aes(x, y)) +
    geom_line(color = "blue") +
    ggtitle("Distribution Normale") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_expo <- ggplot(data.frame(x = x_expo, y = y_expo), aes(x, y)) +
    geom_line(color = "red") +
    ggtitle("Distribution Exponentielle") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  dd<-df %>%
    filter(Filière.de.production == "F0 : Total toutes filières",
           Plage.de.puissance.injection == "P0 : Total toutes puissances")%>%
    mutate(Total.énergie=Total.énergie.injectée..Wh./Nb.points.injection)%>%
    group_by(Date) %>%
    summarise(Total_energie = sum(Total.énergie, na.rm = TRUE),Courbe.Moyenne1=sum(Courbe.Moyenne.n.1..Wh., na.rm = TRUE),Courbe.Moyenne2=sum(Courbe.Moyenne.n.2..Wh., na.rm = TRUE))
    
  production <- ggplot(dd, aes(x = Date)) +
    geom_line(aes(y = Total_energie, color = "Energie Moyenne injectées")) +
    geom_line(aes(y = Courbe.Moyenne1, color = "Courbe Moyenne 1"), linetype = "dotted", size = 1) +
    geom_line(aes(y = Courbe.Moyenne2, color = "Courbe Moyenne 2"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Energie Moyenne injectées" = "green", "Courbe Moyenne 1" = "red", "Courbe Moyenne 2" = "yellow")) +
    labs(title = "Moyenne Totale d'énergie injectée",
         x = "Date",
         y = "Consommation",
         color = "Courbe Moyenne -Production-") +
    theme_minimal()
  
  # Retourner les graphiques sous forme de liste
  list(p_normal, p_expo, production)
}

# Interface utilisateur (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Analyse de la Production & la consomation d'Energie",titleWidth = 500),
  dashboardSidebar(disable = TRUE),  # Désactiver la barre latérale
  dashboardBody(
    # Boutons en haut
    fluidRow(
      column(12, align = "center",
             actionButton("tab_36k", "Conso > 36k", class = "btn-primary"),
             actionButton("tab_36k_inf", "Conso < 36k", class = "btn-success"),
             actionButton("tab_prod", "Production", class = "btn-warning")
      )
    ),
    br(),  # Espacement entre les boutons et le contenu
    # Zone pour afficher les courbes ou le contenu en fonction du choix
    uiOutput("main_content")
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Afficher les courbes de densité ou le contenu dynamique
  plots <- courbe_moyenne_plots()
  
  # Contenu par défaut : courbes de densité
  output$main_content <- renderUI({
    fluidRow(
      column(4, plotOutput("normal_density")),
      column(4, plotOutput("expo_density")),
      column(4, plotOutput("Prod_Energie"))
    )
  })
  
  # Afficher les courbes de densité
  output$normal_density <- renderPlot({ plots[[1]] })
  output$expo_density <- renderPlot({ plots[[2]] })
  output$Prod_Energie <- renderPlot({ plots[[3]] })
  
  # Mettre à jour le contenu lorsque l'utilisateur clique sur un bouton
  observeEvent(input$tab_36k, {
    output$main_content <- renderUI({
      tagList(h2("Analyse de la Consommation supérieur à 36k "),
              radioButtons(
                inputId = "choix_pas",          
                label = "Choisissez le pas de temps :", 
                choices = c("Demi-horaire" = "demi_horaire", 
                            "Quotidien" = "quotidien"),
                selected = "demi_horaire",
                inline = TRUE      
              ),
              fluidRow(
                
                column(3,
                       selectInput("region", "Sélectionner les régions", 
                                   choices = unique(df$Région), 
                                   selected = unique(df$Région)[1], 
                                   multiple = TRUE)),
                # Sélecteur de secteur_activite
                column(2,
                       selectInput("secteur_activite", "Sélectionner le Secteur d'activité", 
                                   choices = unique(df$`Secteur activité`), 
                                   selected = unique(df$`Secteur activité`)[1],multiple = TRUE 
                       )),
                
                column(2,
                       selectInput("plage_puissance", "la plage de puissance", 
                                   choices = unique(df$`Plage de puissance souscrite`), 
                                   selected = unique(df$`Plage de puissance souscrite`)[1], 
                                   multiple = TRUE)),
                column(2,
                       uiOutput("date_selector")),
                
                column(1, 
                       conditionalPanel(
                         condition = "input.choix_pas == 'demi_horaire'", 
                         selectInput(
                           inputId = "heure_debut",
                           label = "Heure Début", 
                           choices = format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                                                to = as.POSIXct("23:00:00", format = "%H:%M:%S"), 
                                                by = "30 mins"), 
                                            "%H:%M"),
                           selected = as.POSIXct("00:00:00", format = "%H:%M")
                         ))),
                
                column(1, 
                       conditionalPanel(
                         condition = "input.choix_pas == 'demi_horaire'", 
                         selectInput(
                           inputId = "heure_fin",
                           label = "Heure Fin", 
                           choices = format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                                                to = as.POSIXct("23:30:00", format = "%H:%M:%S"), 
                                                by = "30 mins"), 
                                            "%H:%M"),
                           selected = format(as.POSIXct("23:30:00", format = "%H:%M:%S"),"%H:%M")
                         ))),
                column(2,
                       downloadButton("download_data", "Télécharger les données", 
                                      class = "btn-success"), offset = 10)
              ),
              
              fluidRow(
                valueBoxOutput("total_energie"),
                valueBoxOutput("total_points"),
                valueBoxOutput("puissance_max")
              ),
              
              # Affichage du graphique interactif:
              
              fluidRow(
                column(8,plotlyOutput("energy_plot")),)
      )
      
    })
    
    output$date_selector <- renderUI({
      if (input$choix_pas == "demi_horaire") {
        dateInput("date", "Date", value = "2024-06-30")
      } else {
        dateRangeInput("date_range", "Plage de Dates", start = as.Date("2024-06-30") - 3, end = as.Date("2024-06-30"), width =1000)
      }
    })
    
    
    # Filtrage des données en fonction des choix de l'utilisateur:
    
    filtered_data <- reactive({
      if (input$choix_pas == "demi_horaire") {
        
        # Filtrage des données selon les intervalles de temps:
        
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin,":00"))
        
        df_filtered <- df %>%
          filter(Région %in% input$region,
                 `Plage de puissance souscrite` %in% input$plage_puissance,
                 Date %in% input$date,
                 `Secteur activité` == input$secteur_activite,
                 Heure >= debut & Heure <= fin)
      } else {
        df_filtered <- df %>%
          filter(Région %in% input$region,
                 `Plage de puissance souscrite` %in% input$plage_puissance,
                 Date >= input$date_range[1] & Date <= input$date_range[2],
                 `Secteur activité` == input$secteur_activite) %>%
          group_by(Date,Région,`Plage de puissance souscrite`) %>%
          summarise(Total_energie = sum(`Total énergie soutirée (Wh)`, na.rm = TRUE),Courbe.Moyenne1=sum(`Courbe Moyenne n°1 (Wh)`, na.rm = TRUE),Courbe.Moyenne2=sum(`Courbe Moyenne n°2 (Wh)`, na.rm = TRUE),
                    Nb_points = unique(`Nb points soutirage`))
      }
      return(df_filtered)
    })
    
    
    # Valuebox : l'énergie Total:
    
    output$total_energie <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        total_energie <- sum(data$`Total énergie soutirée (Wh)`, na.rm = TRUE)
        valueBox(format(total_energie, big.mark = ","), "Total Energie Injectée", icon = icon("bolt"), color = "light-blue") 
      } else {
        total_energie <- sum(data$Total_energie, na.rm = TRUE)
        valueBox(format(total_energie, big.mark = ","), "Total Energie Injectée", icon = icon("bolt"), color = "light-blue")
      }
    })
    
    # Valuebox : Nombre de points:
    
    output$total_points <- renderValueBox({
      df <- filtered_data()
      total_points <- length(unique(df$`Nb points soutirage`))  
      valueBox(format(total_points, big.mark = ","), "Nombre de Points", icon = icon("point"), color = "green")
    })
    # Valuebox : Puissance maximal:
    
    output$puissance_max <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        puissance_max <- max(data$`Total énergie soutirée (Wh)` , na.rm = TRUE)
        horodate_max <- data$Horodate_clean[which.max(data$`Total énergie soutirée (Wh)`)]
        valueBox(format(puissance_max, big.mark = ","), paste("Puissance Max. à", horodate_max), icon = icon("chart-line"), color = "yellow")
      } else {
        puissance_max <- max(data$Total_energie, na.rm = TRUE)
        horodate_max <- data$Date[which.max(data$Total_energie)]
        valueBox(format(puissance_max, big.mark = ","), paste("Puissance Max. à", horodate_max), icon = icon("chart-line"), color = "yellow")
      }
    })
    
    
    output$energy_plot <- renderPlotly({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        fig <- plot_ly(data, 
                       x = ~ as.POSIXct(paste(input$date, Heure), format = "%Y-%m-%d %H:%M:%S"),
                       y = ~ `Total énergie soutirée (Wh)`/10^5, 
                       color = ~ paste(data$`Plage de puissance souscrite`, data$Région, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~Région) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, Heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ `Courbe Moyenne n°1 (Wh)`,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$`Plage de puissance souscrite`, data$Région, sep = "_"))
          ) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, Heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ `Courbe Moyenne n°2 (Wh)`,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$`Plage de puissance souscrite`, data$Région, sep = "_"))
          ) %>%
          layout(
            title = "Totale d'énergie injéctées",
            xaxis = list(title = "Plage horaire"),
            yaxis = list(title = "Consommation (100 kWh)")
          )
      }else {
        fig <- plot_ly(data, 
                       x = ~ data$Date,
                       y = ~ Total_energie/10^5, 
                       color = ~ paste(data$`Plage de puissance souscrite`, data$Région, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~Région) %>%
          add_trace(
            x = ~ data$Date,
            y = ~ Courbe.Moyenne1,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$`Plage de puissance souscrite`, data$Région, sep = "_"))
          ) %>%
          add_trace(
            x = ~ data$Date,
            y = ~ Courbe.Moyenne2,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$`Plage de puissance souscrite`, data$Région, sep = "_"))
          ) %>%
          layout(
            title = "Totale d'énergie injéctées",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Consommation (100 kWh)")
          )
      }
      fig
      
    })
    
    
    output$download_data <- downloadHandler(
      filename = function() { 
        paste("prod_energies",input$region,input$fliere ,input$date, ".csv", sep = "") 
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
  })
  
  observeEvent(input$tab_36k_inf, {
    output$main_content <- renderUI({
      ("Consommation inférieure à 36k")
    })
  })
  
  observeEvent(input$tab_prod, {
    output$main_content <- renderUI({
      tagList(h2("Analyse de la Production d'Energie"),
        radioButtons(
          inputId = "choix_pas",
          label = "Choisissez le pas de temps :",
          choices = c("Demi-horaire" = "demi_horaire", "Quotidien" = "quotidien"),
          selected = "demi_horaire",
          inline = TRUE
        ),
        fluidRow(
          column(3,
                 selectInput("region", "Sélectionner les régions", 
                             choices = unique(df$Région), 
                             selected = unique(df$Région)[1], 
                             multiple = TRUE)),
          column(2,
                 selectInput("fliere", "Sélectionner la filère", 
                             choices = unique(df$Filière.de.production), 
                             selected = unique(df$Filière.de.production)[1]
                 )),
          column(2,
                 selectInput("plage_puissance", "La plage de puissance", 
                             choices = unique(df$Plage.de.puissance.injection), 
                             selected = unique(df$Plage.de.puissance.injection)[1], 
                             multiple = TRUE)),
          column(2,
                 uiOutput("date_selector")),
          column(1, 
                 conditionalPanel(
                   condition = "input.choix_pas == 'demi_horaire'",
                   selectInput(
                     inputId = "heure_debut",
                     label = "Heure Début", 
                     choices = format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                                          to = as.POSIXct("23:00:00", format = "%H:%M:%S"), 
                                          by = "30 mins"), 
                                      "%H:%M"),
                     selected = as.POSIXct("00:00:00", format = "%H:%M")
                   )
                 )),
          column(1, 
                 conditionalPanel(
                   condition = "input.choix_pas == 'demi_horaire'", 
                   selectInput(
                     inputId = "heure_fin",
                     label = "Heure Fin", 
                     choices = format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                                          to = as.POSIXct("23:30:00", format = "%H:%M:%S"), 
                                          by = "30 mins"), 
                                      "%H:%M"),
                     selected = format(as.POSIXct("23:30:00", format = "%H:%M:%S"),"%H:%M")
                   )
                 )),
          column(2,
                 downloadButton("download_data", "Télécharger les données", 
                                class = "btn-success"), offset = 10)
        ),
        fluidRow(
          valueBoxOutput("total_energie"),
          valueBoxOutput("puissance_moyenne"),
          valueBoxOutput("puissance_max")
        ),
        fluidRow(
          column(8, plotlyOutput("energy_plot")),
          column(4, leafletOutput("map"))
        )
      )
    })
    output$date_selector <- renderUI({
      if (input$choix_pas == "demi_horaire") {
        dateInput("date", "Date", value = "2024-06-30")
      } else {
        dateRangeInput("date_range", "Plage de Dates", 
                       start = as.Date("2024-06-30") - 3, 
                       end = as.Date("2024-06-30"), 
                       width = 1000)
      }
    })
    filtered_data <- reactive({
      if (input$choix_pas == "demi_horaire") {
        # Filtrer les données selon les intervalles de temps
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin,":00"))
        
        df_filtered <- df %>%
          filter(Région %in% input$region,
                 Plage.de.puissance.injection %in% input$plage_puissance,
                 Date %in% input$date,
                 Filière.de.production == input$fliere,
                 Heure >= debut & Heure <= fin)
      } else {
        if(input$fliere != "F0 : Total toutes filières"){
          df_filtered <- df %>%
          filter(Région %in% input$region,
                 Plage.de.puissance.injection %in% input$plage_puissance,
                 Date >= input$date_range[1] & Date <= input$date_range[2],
                 Filière.de.production == input$fliere) %>%
          group_by(Date,Région,Plage.de.puissance.injection) %>%
          summarise(Total_energie = sum(Total.énergie.injectée..Wh., na.rm = TRUE),Courbe.Moyenne1=sum(Courbe.Moyenne.n.1..Wh., na.rm = TRUE),Courbe.Moyenne2=sum(Courbe.Moyenne.n.2..Wh., na.rm = TRUE))
        }else{
          df_filtered <- df %>%
            filter(Région %in% input$region,
                   Plage.de.puissance.injection %in% input$plage_puissance,
                   Date >= input$date_range[1] & Date <= input$date_range[2],
                   Filière.de.production == "F0 : Total toutes filières") %>%
            group_by(Date,Région,Plage.de.puissance.injection) %>%
            summarise(Total_energie = sum(Total.énergie.injectée..Wh., na.rm = TRUE),Courbe.Moyenne1=sum(Courbe.Moyenne.n.1..Wh., na.rm = TRUE),Courbe.Moyenne2=sum(Courbe.Moyenne.n.2..Wh., na.rm = TRUE))
        }
      }
      return(df_filtered)
    })
    
    output$total_energie <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(Filière.de.production == "F0 : Total toutes filières")}
        else{data%>% filter(Filière.de.production != "F0 : Total toutes filières")}
        total_energie <- sum(data$Total.énergie.injectée..Wh., na.rm = TRUE)
        valueBox(format(total_energie, big.mark = ","), "Total Energie Injectée", icon = icon("bolt"), color = "light-blue") 
      } else {
        total_energie <- sum(data$Total_energie, na.rm = TRUE)
        valueBox(format(total_energie, big.mark = ","), "Total Energie Injectée", icon = icon("bolt"), color = "light-blue")
      }
    })
    
    output$puissance_moyenne <- renderValueBox({
      data <- filtered_data()
      
      if (input$choix_pas == "demi_horaire") {
      
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(Filière.de.production == "F0 : Total toutes filières")}       
        else{data%>% filter(Filière.de.production != "F0 : Total toutes filières")}
          puissance_moyenne <- mean(data$Total.énergie.injectée..Wh., na.rm = TRUE)
        valueBox(format(puissance_moyenne, big.mark = ","), "Puissance Moyenne", icon = icon("tachometer-alt"), color = "green",
                 width = NULL)
      }else {
        puissance_moyenne <- mean(data$Total_energie, na.rm = TRUE)
        valueBox(format(puissance_moyenne, big.mark = ","), "Puissance Moyenne", icon = icon("tachometer-alt"), color = "green",
                 width = NULL)
      }
    })
    
    output$puissance_max <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(Filière.de.production == "F0 : Total toutes filières")}
        else{data%>% filter(Filière.de.production != "F0 : Total toutes filières")}
        puissance_max <- max(data$Total.énergie.injectée..Wh. , na.rm = TRUE)
        horodate_max <- data$Horodate_clean[which.max(data$Total.énergie.injectée..Wh.)]
        valueBox(format(puissance_max, big.mark = ","), paste("Puissance Max. à", horodate_max), icon = icon("chart-line"), color = "yellow")
      } else {
        puissance_max <- max(data$Total_energie, na.rm = TRUE)
        horodate_max <- data$Date[which.max(data$Total_energie)]
        valueBox(format(puissance_max, big.mark = ","), paste("Puissance Max. à", horodate_max), icon = icon("chart-line"), color = "yellow")
      }
    })
    
    # Graphique interactif
    output$energy_plot <- renderPlotly({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(Filière.de.production == "F0 : Total toutes filières")}       
        else{data%>% filter(Filière.de.production != "F0 : Total toutes filières")}
          
        fig <- plot_ly(data, 
                       x = ~ as.POSIXct(paste(input$date, Heure), format = "%Y-%m-%d %H:%M:%S"),
                       y = ~ Total.énergie.injectée..Wh./10^5, 
                       color = ~ paste(data$Plage.de.puissance.injection, data$Région, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~Région) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, Heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ Courbe.Moyenne.n.1..Wh.,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$Plage.de.puissance.injection, data$Région, sep = "_"))
          ) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, Heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ Courbe.Moyenne.n.2..Wh.,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$Plage.de.puissance.injection, data$Région, sep = "_"))
          ) %>%
          layout(
            title = "Totale d'énergie injéctées",
            xaxis = list(title = "Plage horaire"),
            yaxis = list(title = "Consommation (100 kWh)")
          )
      }else {
        fig <- plot_ly(data, 
                       x = ~ data$Date,
                       y = ~ Total_energie/10^5, 
                       color = ~ paste(data$Plage.de.puissance.injection, data$Région, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~Région) %>%
          add_trace(
            x = ~ data$Date,
            y = ~ Courbe.Moyenne1,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$Plage.de.puissance.injection, data$Région, sep = "_"))
          ) %>%
          add_trace(
            x = ~ data$Date,
            y = ~ Courbe.Moyenne2,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$Plage.de.puissance.injection, data$Région, sep = "_"))
          ) %>%
          layout(
            title = "Totale d'énergie injéctées",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Consommation (100 kWh)")
          )
      }
      
      # Afficher le graphique
      fig
      
    })
    output$map <- renderLeaflet({
      if (input$choix_pas == "demi_horaire") {
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin,":00"))
        
        df_aggregated <- df %>%
          filter(Plage.de.puissance.injection %in% input$plage_puissance,
                 Date %in% input$date,
                 Filière.de.production == input$fliere,
                 Heure >= debut & Heure <= fin)%>%
          group_by(Code.région,Nb.points.injection,Filière.de.production) %>%
          summarise(Total_energie = sum(as.numeric(Total.énergie.injectée..Wh.), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(df_aggregated, by = c("code" = "Code.région"))
        
        # Créer la carte avec leaflet
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("Région: ", nom, ",", "\nTotal énergie injectée: ", Total_energie,"\nNb de points d'injection : ",unique(Nb.points.injection)),
            label = ~paste(nom, ",", "Total énergie injectée: ", round(Total_energie/10^6,3),"Mkh"),  # Labels sur chaque région
            labelOptions = labelOptions(
              direction = "center",  # Positionner les labels au centre des régions
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black")  # Personnalisation du style des labels
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),  # Palette de couleurs et nombre de catégories
            values = regions_geo$Total_energie,
            title = "Total énergie injectée",
            opacity = 0.5
          )
      }else {
        df_aggregated <- df %>%
          filter(Plage.de.puissance.injection %in% input$plage_puissance,
                 Filière.de.production == input$fliere,
                 Date >= input$date_range[1] & Date <= input$date_range[2])%>%
          group_by(Code.région) %>%
          summarise(Total_energie = sum(as.numeric(Total.énergie.injectée..Wh.), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(df_aggregated, by = c("code" = "Code.région"))
        # Créer la carte avec leaflet
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("Région: ", nom, ",", "\nTotal énergie injectée: ", Total_energie),
            label = ~paste(nom, ",", "Total énergie injectée: ", round(Total_energie/10^6,3),"Mkh"),  # Labels sur chaque région
            labelOptions = labelOptions(
              direction = "center",  # Positionner les labels au centre des régions
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black")  # Personnalisation du style des labels
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),  # Palette de couleurs et nombre de catégories
            values = regions_geo$Total_energie,
            title = "Total énergie injectée",
            opacity = 0.5
          )
        
      }
    })
    output$download_data <- downloadHandler(
      filename = function() { 
        paste("prod_energies",input$region,input$fliere ,input$date, ".csv", sep = "") 
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
  
}

# Lancer l'application
shinyApp(ui, server)
