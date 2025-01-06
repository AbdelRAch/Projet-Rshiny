#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(readxl)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(dplyr) 
library(shinydashboard)

# Charger les données
conso <- read_excel("D:/Nouveau dossier/MASTER 2/RAVANCE/Conso/conso-inf36-region (1).xlsx")


# Interface utilisateur
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("Consommation demi-horaire"),
  
  sidebarLayout(
    sidebarPanel(
      # Sélection d'une plage d'horodate
      dateRangeInput("Horodate", "Sélectionner une plage de dates :", 
                     start = min(conso$Horodate), 
                     end = max(conso$Horodate)),
      
      # Sélection d'une région
      selectInput("Région", "Sélectionner une Région :", 
                  choices = unique(conso$Région), 
                  selected = unique(conso$Région)[1]),
      
      # Sélection dynamique d'un Profil
      uiOutput("Profil"), 
      
      # Sélection dynamique d'une plage de puissance
      uiOutput("Plage_puissance"),
      
      # Case à cocher pour choisir le niveau d'agrégation
      checkboxInput("aggregate_daily", "Afficher les données par jour", value = FALSE),
      
      # Bouton pour télécharger les données affichées
      downloadButton("download_data", "Télécharger les données")
    ),
    mainPanel(
      plotlyOutput("plot1"),  # Graphique interactif
      fluidRow(
        valueBoxOutput("total_conso"),       # Consommation totale
        valueBoxOutput("avg_power"),        # Puissance moyenne
        valueBoxOutput("max_power_info")    # Puissance max et date/heure associée
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Mettre à jour le champ Profil en fonction de la Région sélectionnée
  output$Profil <- renderUI({
    req(input$Région)  
    pickerInput(
      "Profil", "Sélectionner un Profil :", 
      choices = unique(conso %>% filter(Région == input$Région) %>% pull(Profil)),
      options = pickerOptions(
        actionsBox = TRUE, 
        size = 10,
        selectedTextFormat = "count >= 1"
      ),
      multiple = TRUE,
      selected = unique(conso$Profil)[1]
    )
  })
  
  # Mettre à jour le champ Plage_puissance en fonction de la Région sélectionnée
  output$Plage_puissance <- renderUI({
    req(input$Région)  
    pickerInput(
      "Plage_puissance", "Sélectionner une plage de puissance :", 
      choices = unique(conso %>% filter(Région == input$Région) %>% pull(`Plage de puissance souscrite`)),
      options = pickerOptions(
        actionsBox = TRUE, 
        size = 10,
        selectedTextFormat = "count >= 1"
      ),
      multiple = TRUE,
      selected = unique(conso$`Plage de puissance souscrite`)[1]
    )
  })
  
  # Filtrer et agréguer les données en fonction des sélections utilisateur
  filtered_data <- reactive({
    req(input$Région, input$Profil, input$Plage_puissance, input$Horodate)
    data <- conso %>%
      filter(
        Région == input$Région,
        Profil %in% input$Profil,
        `Plage de puissance souscrite` %in% input$Plage_puissance,
        Horodate >= input$Horodate[1],
        Horodate <= input$Horodate[2]
      )
    
    if (input$aggregate_daily) {
      # Agrégation quotidienne
      data <- data %>%
        mutate(Date = as.Date(Horodate)) %>%
        group_by(Date, Région, Profil) %>%
        summarise(`Total énergie soutirée (Wh)` = sum(`Total énergie soutirée (Wh)`, na.rm = TRUE),
                  Max_Power = max(`Total énergie soutirée (Wh)`, na.rm = TRUE),
                  Avg_Power = mean(`Total énergie soutirée (Wh)`, na.rm = TRUE),
                  Max_Horodate = first(Date), # Utiliser la date pour le pas quotidien
                  .groups = "drop")
    } else {
      # Pas demi-horaire (pas d'agrégation)
      data <- data %>%
        mutate(Max_Horodate = Horodate)
    }
    
    data
  })
  
  # Affichage du graphique interactif
  output$plot1 <- renderPlotly({
    data <- filtered_data()
    
    plot_ly(
      data,
      x = if (input$aggregate_daily) ~Date else ~Horodate,  # Axe X : Date ou Horodate
      y = ~`Total énergie soutirée (Wh)`,  # Axe Y : énergie
      color = ~Profil,  # Couleur par Profil
      linetype = ~Région,  # Différencier par Région
      type = 'scatter',
      mode = 'lines+markers',
      hoverinfo = "text",
      text = ~paste(
        "Région :", Région,
        "<br>Profil :", Profil,
        "<br>Énergie :", round(`Total énergie soutirée (Wh)`, 2),
        if (input$aggregate_daily) paste("<br>Date :", Date) else paste("<br>Heure :", Horodate)
      )
    ) %>%
      layout(
        title = if (input$aggregate_daily) "Consommation quotidienne" else "Consommation demi-horaire",
        xaxis = list(title = if (input$aggregate_daily) "Date" else "Heure"),
        yaxis = list(title = "Total énergie soutirée (Wh)"),
        legend = list(title = list(text = "<b>Profil & Région</b>"))
      )
  })
  
  # Affichage de la consommation totale dans une valueBox
  output$total_conso <- renderValueBox({
    total <- sum(filtered_data()$`Total énergie soutirée (Wh)`, na.rm = TRUE)
    valueBox(
      value = round(total, 2), 
      subtitle = if (input$aggregate_daily) "Consommation totale par jour (Wh)" else "Consommation totale (Wh)",
      icon = icon("chart-bar"),
      color = "blue"
    )
  })
  
  # Affichage de la puissance moyenne dans une valueBox
  output$avg_power <- renderValueBox({
    avg_power <- mean(filtered_data()$`Total énergie soutirée (Wh)`, na.rm = TRUE)
    valueBox(
      value = round(avg_power, 2), 
      subtitle = "Puissance moyenne (Wh)",
      icon = icon("bolt"),
      color = "green"
    )
  })
  
  # Affichage de la puissance maximale et horodate associée dans une valueBox
  output$max_power_info <- renderValueBox({
    data <- filtered_data()
    max_power <- max(data$`Total énergie soutirée (Wh)`, na.rm = TRUE)
    max_time <- data$Max_Horodate[which.max(data$`Total énergie soutirée (Wh)`)]
    valueBox(
      value = round(max_power, 2), 
      subtitle = paste("Puissance max (Wh) atteinte le :", max_time),
      icon = icon("fire"),
      color = "red"
    )
  })
  
  # Télécharger les données affichées
  output$download_data <- downloadHandler(
    filename = function() {
      paste("consommation_donnees", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Lancer l'application
shinyApp(ui = ui, server = server)