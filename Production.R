library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(dplyr)
library(stringr)
library(lubridate)
library(hms)
library(leaflet)
library(sf)

<<<<<<< HEAD
colnames(prod_reg)<-prod_reg[1,]
# df <- df %>%
=======
 Prod_enrgies <- readRDS("Prod_enrgies.rds")
# df <- Prod_enrgies %>%
>>>>>>> d874eb07ff0fc832746581a667a8efc221334bca
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

# Charger les frontières des régions françaises depuis le fichier GeoJSON
regions_geo <- st_read("regions.geojson")  # Si le fichier est dans le même dossier que le script
regions_geo$code<-as.numeric(regions_geo$code)

# UI de l'application
ui <- dashboardPage(
  # Titre de l'application
  dashboardHeader(title ="Analyse de la Production d'Energie",titleWidth = 400),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    radioButtons(
      inputId = "choix_pas",          # Identifiant de l'input
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
      # Sélecteur de flière
      column(2,
             selectInput("fliere", "Sélectionner la filère", 
                         choices = unique(df$Filière.de.production), 
                         selected = unique(df$Filière.de.production)[1], 
             )),
      
      column(2,
             selectInput("plage_puissance", "la plage de puissance", 
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
      valueBoxOutput("puissance_moyenne"),
      valueBoxOutput("puissance_max")
    ),
    
    # Affichage du graphique interactif
    fluidRow(
      column(8,plotlyOutput("energy_plot")),
      column(4,leafletOutput("map"))
    )
  )
)

# Serveur de l'application
server <- function(input, output) {
  
  output$date_selector <- renderUI({
    if (input$choix_pas == "demi_horaire") {
      dateInput("date", "Date", value = "2024-06-30")
    } else {
      dateRangeInput("date_range", "Plage de Dates", start = as.Date("2024-06-30") - 3, end = as.Date("2024-06-30"), width =1000)
    }
  })

  # Filtrer les données en fonction des choix de l'utilisateur
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
      df_filtered <- df %>%
        filter(Région %in% input$region,
               Plage.de.puissance.injection %in% input$plage_puissance,
               Date >= input$date_range[1] & Date <= input$date_range[2],
               Filière.de.production == input$fliere) %>%
        group_by(Date,Région,Plage.de.puissance.injection) %>%
        summarise(Total_energie = sum(Total.énergie.injectée..Wh., na.rm = TRUE),Courbe.Moyenne1=sum(Courbe.Moyenne.n.1..Wh., na.rm = TRUE),Courbe.Moyenne2=sum(Courbe.Moyenne.n.2..Wh., na.rm = TRUE))
    }
    return(df_filtered)
  })
  
  output$total_energie <- renderValueBox({
    data <- filtered_data()
    if (input$choix_pas == "demi_horaire") {
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
      puissance_max <- max(data$Total.énergie.injectée..Wh. , na.rm = TRUE)
      horodate_max <- data$Horodate_clean[which.max(data$Total.énergie.injectée..Wh.)]
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
}

shinyApp(ui = ui, server = server)
