# Charger les librairies nécessaires
library(shiny)
library(DT)
library(readxl)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(viridis)
library(shinydashboard)

# Charger les données
conso <- read_excel("/Users/gertrudenyamassoule/Projet-Rshiny/conso-inf36-region (1).xlsx")

# Interface utilisateur
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("Consommation demi-horaire"),
  
  sidebarLayout(
    sidebarPanel(
      
      dateRangeInput("Horodate", "Sélectionner une plage de dates :", 
                     start = min(conso$Horodate), 
                     end = max(conso$Horodate)),
      
      
      selectInput("Région", "Sélectionner une Région :", 
                  choices = unique(conso$Région), 
                  selected = unique(conso$Région)[1]),
      
      
      uiOutput("Profil"), 
      
      
      uiOutput("Plage_puissance"),
      
     
      checkboxInput("aggregate_daily", "Afficher les données par jour", value = FALSE),
      
      
      selectInput("graph_type", "Choisir le graphique à afficher", 
                  choices = c("Total énergie soutirée", "Courbe Moyenne"), 
                  selected = "Total énergie soutirée"),
      
      
      downloadButton("download_data", "Télécharger les données")
    ),
    mainPanel(
      plotlyOutput("plot1"), 
      fluidRow(
        valueBoxOutput("total_conso"),       
        valueBoxOutput("avg_power"),        
        valueBoxOutput("max_power_info")    
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  
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
      
      data <- data %>%
        mutate(Date = as.Date(Horodate)) %>%
        group_by(Date, Région, Profil) %>%
        summarise(`Total énergie soutirée (Wh)` = sum(`Total énergie soutirée (Wh)`, na.rm = TRUE),
                  Max_Power = max(`Total énergie soutirée (Wh)`, na.rm = TRUE),
                  Avg_Power = mean(`Total énergie soutirée (Wh)`, na.rm = TRUE),
                  .groups = "drop")
      
      
      data$`Courbe Moyenne n°1 (Wh)` <- data$Avg_Power  
    } else {
      # Pas demi-horaire (pas d'agrégation)
      data <- data %>%
        mutate(Max_Horodate = Horodate)
    }
    
    data
  })
  
  # Graphique interactif
  output$plot1 <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    
    data$Profile_PowerRange <- paste(data$Profil, data$`Plage de puissance souscrite`, sep = " - ")
    
    
    unique_combinations <- unique(data$Profile_PowerRange)
    colors <- viridis::viridis(length(unique_combinations))
    names(colors) <- unique_combinations
    
    
    p <- plot_ly()
    
    
    if (input$graph_type == "Total énergie soutirée") {
      p <- p %>%
        add_trace(
          data = data,
          x = if (input$aggregate_daily) ~Date else ~Horodate,
          y = ~`Total énergie soutirée (Wh)`,
          color = ~Profile_PowerRange,
          colors = colors,
          type = 'scatter',
          mode = 'lines+markers',
          hoverinfo = "text",
          text = ~paste(
            "Région :", Région,
            "<br>Profil :", Profil,
            "<br>Énergie :", round(`Total énergie soutirée (Wh)`, 2),
            if (input$aggregate_daily) paste("<br>Date :", Date) else paste("<br>Heure :", Horodate)
          )
        )
    } else if (input$graph_type == "Courbe Moyenne") {
      p <- p %>%
        add_trace(
          data = data,
          x = if (input$aggregate_daily) ~Date else ~Horodate,
          y = ~`Courbe Moyenne n°1 (Wh)`,
          color = ~Profile_PowerRange,
          colors = colors,
          type = 'scatter',
          mode = 'lines',
          line = list(dash = "dot", width = 2),
          name = "Courbe Moyenne"
        )
    }
    
    p <- p %>%
      layout(
        title = if (input$aggregate_daily) "Consommation quotidienne" else "Consommation demi-horaire",
        xaxis = list(title = if (input$aggregate_daily) "Date" else "Heure"),
        yaxis = list(title = "Énergie (Wh)"),
        legend = list(title = list(text = "<b>Profil & Région</b>"))
      )
    
    p
  })
  

  output$total_conso <- renderValueBox({
    total <- sum(filtered_data()$`Total énergie soutirée (Wh)`, na.rm = TRUE)
    valueBox(
      value = round(total, 2), 
      subtitle = if (input$aggregate_daily) "Consommation totale par jour (Wh)" else "Consommation totale (Wh)",
      icon = icon("chart-bar"),
      color = "blue"
    )
  })
  
  
  output$avg_power <- renderValueBox({
    avg_power <- mean(filtered_data()$`Total énergie soutirée (Wh)`, na.rm = TRUE)
    valueBox(
      value = round(avg_power, 2), 
      subtitle = "Puissance moyenne (Wh)",
      icon = icon("bolt"),
      color = "green"
    )
  })
  
 
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
