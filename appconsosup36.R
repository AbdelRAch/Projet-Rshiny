# bonne code qui marche

# Téléchargement des library:

library(shiny)
library(dplyr)
library(shinydashboard)
library(readxl)
library(plotly)
library(lubridate)

# Charger les données
#conso_sup36 <- read_excel("conso-sup36-30juin region.xlsx")

conso_sup36 <- read_excel("conso-sup36-region.xlsx")
# Supprimer les valeurs manquantes
conso_sup36 <- conso_sup36[complete.cases(conso_sup36), ]

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Conso >= 36kVA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Consommation >= 36kVA", tabName = "conso_sup36", icon = icon("chart-area"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "conso_sup36",
        fluidRow(
          # Filtres
          box(
            title = "Filtrage", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("regions_sup36", "Régions", choices = unique(conso_sup36$Région), multiple = TRUE),
            selectInput("secteurs_sup36", "Secteurs d'activité", choices = unique(conso_sup36$`Secteur activité`), multiple = TRUE),
            selectInput("puissance_sup36", "Plages de puissance", choices = unique(conso_sup36$`Plage de puissance souscrite`), multiple = TRUE),
            selectInput("profil_sup36", "Profil", choices = unique(conso_sup36$Profil), multiple = TRUE),
            selectInput("Nombre_points_sup36", "Nombre de points", choices = unique(conso_sup36$`Nb points soutirage`), multiple = TRUE),
            dateRangeInput("date_sup36", "Période", start = min(conso_sup36$Horodate), end = max(conso_sup36$Horodate)),
            radioButtons("pas_de_temps", "Choisir le pas de temps",
                         choices = list("Demi-horaire" = "half-hour", "Quotidien" = "daily"),
                         selected = "daily"),
            selectInput("plot_type", "Type de graphique", choices = c("Consommation Totale", "Consommation Moyenne"))
          ),
          
          # Résumé avec valueboxes
          box(
            title = "Résumé", status = "primary", solidHeader = TRUE, width = 8,
            fluidRow(
              valueBoxOutput("total_consumption", width = 4),
              valueBoxOutput("max_power", width = 4),
              valueBoxOutput("total_points", width = 4)  # Ajouter un nouveau valueBox pour le nombre de points
            )
          )
        ),
        
        fluidRow(
          # Graphique interactif
          box(
            title = "Graphique interactif", status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("plot_consumption"),
            downloadButton("download_sup36", "Télécharger les données")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filtrage des données en fonction des inputs
  filtered_data <- reactive({
    df <- conso_sup36
    if (!is.null(input$regions_sup36)) {
      df <- df %>% filter(Région %in% input$regions_sup36)
    }
    if (!is.null(input$secteurs_sup36)) {
      df <- df %>% filter(`Secteur activité` %in% input$secteurs_sup36)
    }
    if (!is.null(input$puissance_sup36)) {
      df <- df %>% filter(`Plage de puissance souscrite` %in% input$puissance_sup36)
    }
    if (!is.null(input$profil_sup36)) {
      df <- df %>% filter(Profil %in% input$profil_sup36)
    }
    if (!is.null(input$Nombre_points_sup36)) {
      df <- df %>% filter(`Nb points soutirage` %in% input$Nombre_points_sup36)
    }
    if (!is.null(input$date_sup36)) {
      df <- df %>% filter(Horodate >= input$date_sup36[1] & Horodate <= input$date_sup36[2])
    }
    
    # Ajustement du pas de temps
    if(input$pas_de_temps == "daily") {
      df$Horodate <- as.Date(df$Horodate)  # Regrouper par jour
    } else {
      # Regrouper par demi-heure en utilisant cut.POSIXt
      df$Horodate <- cut(as.POSIXct(df$Horodate), breaks = "30 min")  # Regrouper par demi-heure
    }
    
    return(df)
  })
  
  # Valuebox : Consommation totale
  output$total_consumption <- renderValueBox({
    df <- filtered_data()
    total_consumption <- sum(df$`Total énergie soutirée (Wh)`, na.rm = TRUE)
    valueBox(format(total_consumption, big.mark = ","), "Consommation Totale (Wh)", icon = icon("bolt"), color = "blue")
  })
  
  # Valuebox : Puissance maximale et Horodate
  output$max_power <- renderValueBox({
    df <- filtered_data()
    max_power <- max(df$`Total énergie soutirée (Wh)`, na.rm = TRUE)
    max_time <- df$Horodate[which.max(df$`Total énergie soutirée (Wh)`)]
    
    valueBox(paste(format(max_power, digits = 2), "à", max_time), "Puissance Max (Wh)", icon = icon("bolt"), color = "red")
  })
  
  # Valuebox : Nombre de points
  output$total_points <- renderValueBox({
    df <- filtered_data()
    total_points <- length(unique(df$`Nb points soutirage`))  # Calculer le nombre de points uniques
    valueBox(format(total_points, big.mark = ","), "Nombre de Points", icon = icon("point"), color = "green")
  })
  
  # Graphique de la consommation Totale ou Moyenne avec plotly ou plot()
  output$plot_consumption <- renderPlotly({
    df <- filtered_data()
    
    # Si le type de graphique est "Consommation Totale"
    if(input$plot_type == "Consommation Totale") {
      # Graphique en ligne de la consommation totale
      p <- plot_ly(df, x = ~as.POSIXct(Horodate), y = ~`Total énergie soutirée (Wh)`, type = 'scatter', mode = 'lines+markers',
                   color = ~interaction(Région, `Secteur activité`),
                   line = list(shape = "linear")) %>%
        layout(title = "Consommation Totale",
               xaxis = list(title = "Date", tickformat = "%d-%m-%Y", tickangle = 45),  # Format des dates et angle des labels
               yaxis = list(title = "Consommation Totale (Wh)"),
               margin = list(t = 50, b = 100))  # Ajustement des marges pour la lisibilité des dates
    } else {
      # Graphique de la consommation moyenne par point
      avg_consumption <- df$`Total énergie soutirée (Wh)` / df$`Nb points soutirage`
      
      p <- plot_ly(df, x = ~as.POSIXct(Horodate), y = ~avg_consumption, type = 'scatter', mode = 'lines+markers',
                   color = ~interaction(Région, `Secteur activité`)) %>%
        layout(title = "Consommation Moyenne (Wh par Point)",
               xaxis = list(title = "Date", tickformat = "%d-%m-%Y", tickangle = 45),  # Format des dates et angle des labels
               yaxis = list(title = "Consommation Moyenne (Wh par Point)"),
               margin = list(t = 50, b = 100))  # Ajustement des marges pour la lisibilité des dates
    }
    
    p
  })
  
  # Bouton de téléchargement
  output$download_sup36 <- downloadHandler(
    filename = function() {
      paste("data_filtered_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
}


#shinyApp(ui, server)



# Run the application 
shinyApp(ui = ui, server = server)

