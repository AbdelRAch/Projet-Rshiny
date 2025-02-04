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
library(data.table)
library(janitor)


# chargement des données:
conso_sup36= fread("conso-sup36-region.csv")
conso= fread("conso-inf36-region.csv")
df<-readRDS("Prod_enrgies.rds")

conso <- conso %>% clean_names()
conso_sup36 <- conso_sup36 %>% clean_names()
df <- df %>% clean_names()

# Transformation des dataset:

conso <- conso %>%
  mutate(
    horodate_clean = sub("\\+.*$", "", horodate),  # Supprimer le fuseau horaire
    date = as.Date(sub("T.*$", "", horodate_clean)),  # Extraire la date
    heure = format(ymd_hms(horodate_clean), "%H:%M:%S"),  # Extraire l'heure
    total_energie_soutiree_wh = as.numeric(total_energie_soutiree_wh),
    courbe_moyenne_n_1_wh = as.numeric(courbe_moyenne_n_1_wh),
    courbe_moyenne_n_2_wh = as.numeric(courbe_moyenne_n_2_wh)
  ) %>%
  select(c(16:18,2:15))

conso_sup36 <- conso_sup36 %>%
  mutate(
    horodate_clean = sub("\\+.*$", "", horodate),  # Supprimer le fuseau horaire
    date = as.Date(sub("T.*$", "", horodate_clean)),  # Extraire la date
    heure = format(ymd_hms(horodate_clean), "%H:%M:%S"),  # Extraire l'heure
    total_energie_soutiree_wh = as.numeric(total_energie_soutiree_wh),
    courbe_moyenne_n_1_wh = as.numeric(courbe_moyenne_n_1_wh),
    courbe_moyenne_n_2_wh = as.numeric(courbe_moyenne_n_2_wh)
  ) %>%
  select(c(17:19,2:16))


# Charger les frontières des regions françaises depuis le fichier GeoJSON téléchargé
regions_geo <- st_read("regions.geojson")  
regions_geo$code<-as.numeric(regions_geo$code)


courbe_moyenne_plots <- function(data,data1,data2,regions,filiere,puissance,puissance1,puissance2) {
  dd<-data %>%
    filter(region %in% regions,
           filiere_de_production == filiere,
           plage_de_puissance_injection == puissance)%>%
    mutate(total_energie=total_energie_injectee_wh/nb_points_injection)%>%
    group_by(date,region,plage_de_puissance_injection) %>%
    summarise(Total_energie = sum(total_energie, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
  
  
  dd1<-data1 %>%
    filter(region %in% regions,
           plage_de_puissance_souscrite == puissance1)%>%
    mutate(total_energie=total_energie_soutiree_wh/nb_points_soutirage)%>%
    group_by(date,region,plage_de_puissance_souscrite) %>%
    summarise(Total_energie = sum(total_energie, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
  
  dd2<-data2 %>%
    filter(region %in% regions,
           plage_de_puissance_souscrite == puissance2)%>%
    mutate(total_energie=total_energie_soutiree_wh/nb_points_soutirage)%>%
    group_by(date,region,plage_de_puissance_souscrite) %>%
    summarise(Total_energie = sum(total_energie, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
  
  conso_inf <- ggplot(dd1, aes(x = date)) +
    geom_line(aes(y = Total_energie, color = "Energie Moyenne soutirées")) +
    geom_line(aes(y = Courbe_Moyenne1, color = "Courbe Moyenne 1"), linetype = "dotted", size = 1) +
    geom_line(aes(y = Courbe_Moyenne2, color = "Courbe Moyenne 2"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Energie Moyenne soutirées" = "green", 
                                  "Courbe Moyenne 1" = "red", 
                                  "Courbe Moyenne 2" = "yellow")) +
    labs(title = "Moyenne Totale (Tous profils) d'énergie soutirée <=36k",
         x = "date",
         y = "Consomation",
         color = NULL) +
    theme_minimal() +
    theme(
      legend.position = c(0, 1), # Positionnement relatif (en haut à gauche)
      legend.justification = c(0, 1), # Ancre de la légende en haut à gauche
      legend.background = element_rect(fill = "transparent", color = NA), # Fond transparent
    )
  
  
  conso_sup <- ggplot(dd2, aes(x = date)) +
    geom_line(aes(y = Total_energie, color = "Energie Moyenne soutirées")) +
    geom_line(aes(y = Courbe_Moyenne1, color = "Courbe Moyenne 1"), linetype = "dotted", size = 1) +
    geom_line(aes(y = Courbe_Moyenne2, color = "Courbe Moyenne 2"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Energie Moyenne soutirées" = "green", 
                                  "Courbe Moyenne 1" = "red", 
                                  "Courbe Moyenne 2" = "yellow")) +
    labs(title = "Moyenne Totale (Tous profils) d'énergie soutirée >36k",
         x = "date",
         y = "Consomation",
         color = NULL) +
    theme_minimal() +
    theme(
      legend.position = c(0, 1), # Positionnement relatif (en haut à gauche)
      legend.justification = c(0, 1), # Ancre de la légende en haut à gauche
      legend.background = element_rect(fill = "transparent", color = NA), # Fond transparent
    )
  
    production <- ggplot(dd, aes(x = date)) +
    geom_line(aes(y = Total_energie, color = "Energie Moyenne injectées")) +
    geom_line(aes(y = Courbe_Moyenne1, color = "Courbe Moyenne 1"), linetype = "dotted", size = 1) +
    geom_line(aes(y = Courbe_Moyenne2, color = "Courbe Moyenne 2"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Energie Moyenne injectées" = "green", 
                                  "Courbe Moyenne 1" = "red", 
                                  "Courbe Moyenne 2" = "yellow")) +
    labs(title = "Moyenne Totale d'énergie injectée",
         x = "date",
         y = "Production",
         color = NULL) +
    theme_minimal() +
    theme(
      legend.position = c(0, 1), # Positionnement relatif (en haut à gauche)
      legend.justification = c(0, 1), # Ancre de la légende en haut à gauche
      legend.background = element_rect(fill = "transparent", color = NA), # Fond transparent
    )
  
  list(conso_inf, conso_sup, production)
}

ui <- dashboardPage(
  dashboardHeader(title = "Analyse de la Production & la consomation d'Energie",titleWidth = 500),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(12, align = "center",
             actionButton("eng", "Courbes Moyenne", class = "btn-secondary"),
             actionButton("tab_36k", "Conso > 36k", class = "btn-primary"),
             actionButton("tab_36k_inf", "Conso <= 36k", class = "btn-success"),
             actionButton("tab_prod", "Production", class = "btn-warning")
      )
    ),
    br(),  
    uiOutput("main_content")
  )
)

server <- function(input, output, session) {
  
  output$main_content <- renderUI({
    tagList(
      h2("Courbes moyenne d'Énergie"),
      fluidRow(align = "center",
               column(3,
                      selectInput(
                        inputId = "region",
                        label = "Sélectionner les regions",
                        choices = unique(df$region),
                        selected = unique(df$region)[1]
                      )),
               column(3,
                      selectInput("fliere", "Sélectionner la filère (Production)", 
                                  choices = unique(df$filiere_de_production), 
                                  selected = unique(df$filiere_de_production)[1]
                      )),
               column(2,
                      selectInput(
                        inputId = "plage_puissance",
                        label = "La plage de puissance",
                        choices = unique(df$plage_de_puissance_injection),
                        selected = unique(df$plage_de_puissance_injection)[1],
                      )),
               column(2,
                      selectInput(
                        inputId = "plage_puissance_inf",
                        label = "La plage de puissance <=36k",
                        choices = unique(conso$plage_de_puissance_souscrite),
                        selected = unique(conso$plage_de_puissance_souscrite)[1],
                      )),
               column(2,
                      selectInput(
                        inputId = "plage_puissance_sup",
                        label = "La plage de puissance >36k",
                        choices = unique(conso_sup36$plage_de_puissance_souscrite),
                        selected = unique(conso_sup36$plage_de_puissance_souscrite)[1],
                      ))
      ),
      fluidRow(
        column(4, plotOutput(outputId = "conso_inf")),
        column(4, plotOutput(outputId = "conso_sup")),
        column(4, plotOutput(outputId = "Prod_Energie"))
      )
    )
  })
  
  observe({
    req(input$region,input$plage_puissance,input$fliere)  
    plots <- courbe_moyenne_plots(df,conso,conso_sup36, input$region,input$fliere,input$plage_puissance,input$plage_puissance_inf,input$plage_puissance_sup)
    
    output$conso_inf <- renderPlot({ plots[[1]] })
    output$conso_sup <- renderPlot({ plots[[2]] })
    output$Prod_Energie <- renderPlot({ plots[[3]] })
  })
  
  
  observeEvent(input$eng, {
    output$main_content <- renderUI({
      tagList(
        h2("Courbes moyenne d'Énergie"),
        fluidRow(align = "center",
                 column(4,
                        selectInput(
                          inputId = "region",
                          label = "Sélectionner les regions",
                          choices = unique(df$region),
                          selected = unique(df$region)[1]
                        )),
                 column(2,
                        selectInput("fliere", "Sélectionner la filère (Production)", 
                                    choices = unique(df$filiere_de_production), 
                                    selected = unique(df$filiere_de_production)[1]
                        )),
                 column(2,
                        selectInput(
                          inputId = "plage_puissance",
                          label = "La plage de puissance",
                          choices = unique(df$plage_de_puissance_injection),
                          selected = unique(df$plage_de_puissance_injection)[1],
                        )),
                 column(2,
                        selectInput(
                          inputId = "plage_puissance_inf",
                          label = "La plage de puissance <=36k",
                          choices = unique(conso$plage_de_puissance_souscrite),
                          selected = unique(conso$plage_de_puissance_souscrite)[1],
                        )),
                 column(2,
                        selectInput(
                          inputId = "plage_puissance_sup",
                          label = "La plage de puissance >36k",
                          choices = unique(conso_sup36$plage_de_puissance_souscrite),
                          selected = unique(conso_sup36$plage_de_puissance_souscrite)[1],
                        ))
        ),
        fluidRow(
          column(4, plotOutput(outputId = "conso_inf")),
          column(4, plotOutput(outputId = "conso_sup")),
          column(4, plotOutput(outputId = "Prod_Energie"))
        )
      )
    })
    
    observe({
      req(input$region,input$plage_puissance,input$fliere)  
      plots <- courbe_moyenne_plots(df,conso,conso_sup36, input$region,input$fliere,input$plage_puissance,input$plage_puissance_inf,input$plage_puissance_sup)
      
      output$conso_inf <- renderPlot({ plots[[1]] })
      output$conso_sup <- renderPlot({ plots[[2]] })
      output$Prod_Energie <- renderPlot({ plots[[3]] })
    })
  })
  
  # Mettre à jour le contenu lorsque l'utilisateur clique sur un bouton
  observeEvent(input$tab_36k, {
    output$main_content <- renderUI({
      tagList(h2("Analyse de la Consommation supérieur à 36k "),
              radioButtons(
                inputId = "choix_pas",
                label = "Choisissez le pas de temps :",
                choices = c("Demi-horaire" = "demi_horaire", "Quotidien" = "quotidien"),
                selected = "demi_horaire",
                inline = TRUE
              ),
              fluidRow(
                column(3,
                       selectInput("region", "Sélectionner les regions", 
                                   choices = unique(conso_sup36$region), 
                                   selected = unique(conso_sup36$region)[1], 
                                   multiple = TRUE)),
                column(2,
                       selectInput("profil", "Sélectionner le Profil", 
                                   choices = unique(conso_sup36$profil), 
                                   selected = unique(conso_sup36$profil)[1]
                       )),
                column(2,
                       selectInput("plage_puissance", "La plage de puissance", 
                                   choices = unique(conso_sup36$plage_de_puissance_souscrite), 
                                   selected = unique(conso_sup36$plage_de_puissance_souscrite)[1], 
                                   multiple = TRUE)),
                column(2,
                       selectInput("secteur", "Secteur d'activité", 
                                   choices = unique(conso_sup36$secteur_activite), 
                                   selected = unique(conso_sup36$secteur_activite)[1], 
                                   multiple = TRUE)),
                column(1,
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
        dateInput("date", "date", value = "2024-06-30")
      } else {
        dateRangeInput("date_range", "Plage de dates", 
                       start = as.Date("2024-06-30") - 5, 
                       end = as.Date("2024-06-30"), 
                       width = 1000)
      }
    })
    filtered_data <- reactive({
      if (input$choix_pas == "demi_horaire") {
        # Filtrer les données selon les intervalles de temps
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin,":00"))
        
        conso_sup36_filtered <- conso_sup36 %>%
          filter(region %in% input$region,
                 plage_de_puissance_souscrite %in% input$plage_puissance,
                 secteur_activite %in% input$secteur,
                 date %in% input$date,
                 profil == input$profil,
                 heure >= debut & heure <= fin)
      }
      else {
        conso_sup36_filtered <- conso_sup36 %>%
          filter(region %in% input$region,
                 plage_de_puissance_souscrite %in% input$plage_puissance,
                 secteur_activite %in% input$secteur,
                 profil == input$profil,
                 date >= input$date_range[1] & date <= input$date_range[2])%>%
          group_by(date,region,plage_de_puissance_souscrite) %>%
          summarise(Total_energie = sum(total_energie_soutiree_wh, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
      }
      return(conso_sup36_filtered)
    })
    
    output$total_energie <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        
        total_energie <- sum(data$total_energie_soutiree_wh, na.rm = TRUE)
        valueBox(format(total_energie, big.mark = ","), "Total Energie soutirée", icon = icon("bolt"), color = "light-blue") 
      } else {
        total_energie <- sum(data$Total_energie, na.rm = TRUE)
        valueBox(format(total_energie, big.mark = ","), "Total Energie soutirée", icon = icon("bolt"), color = "light-blue")
      }
    })
    
    output$puissance_moyenne <- renderValueBox({
      data <- filtered_data()
      
      if (input$choix_pas == "demi_horaire") {
        
        puissance_moyenne <- mean(data$total_energie_soutiree_wh, na.rm = TRUE)
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
        
        puissance_max <- max(data$total_energie_soutiree_wh , na.rm = TRUE)
        horodate_max <- data$horodate_clean[which.max(data$total_energie_soutiree_wh)]
        valueBox(format(puissance_max, big.mark = ","), paste("Puissance Max. à", horodate_max), icon = icon("chart-line"), color = "yellow")
      } else {
        puissance_max <- max(data$Total_energie, na.rm = TRUE)
        horodate_max <- data$date[which.max(data$Total_energie)]
        valueBox(format(puissance_max, big.mark = ","), paste("Puissance Max. à", horodate_max), icon = icon("chart-line"), color = "yellow")
      }
    })
    
    output$energy_plot <- renderPlotly({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        
        fig <- plot_ly(data, 
                       x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
                       y = ~ total_energie_soutiree_wh/nb_points_soutirage, 
                       color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~region) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ courbe_moyenne_n_1_wh,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
          ) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ courbe_moyenne_n_2_wh,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
          ) %>%
          layout(
            title = "Totale d'énergie injéctées",
            xaxis = list(title = "Plage horaire"),
            yaxis = list(title = "Production")
          )
      }else {
        fig <- plot_ly(data, 
                       x = ~ data$date,
                       y = ~ Total_energie, 
                       color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~region) %>%
          add_trace(
            x = ~ data$date,
            y = ~ Courbe_Moyenne1,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
          ) %>%
          add_trace(
            x = ~ data$date,
            y = ~ Courbe_Moyenne2,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
          ) %>%
          layout(
            title = "Totale d'énergie injéctées",
            xaxis = list(title = "date"),
            yaxis = list(title = "Production")
          )
      }
      
      fig
      
    })
    output$map <- renderLeaflet({
      if (input$choix_pas == "demi_horaire") {
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin,":00"))
        
        conso_sup36_aggregated <- conso_sup36 %>%
          filter(plage_de_puissance_souscrite %in% input$plage_puissance,
                 secteur_activite %in% input$secteur,
                 date %in% input$date,
                 profil == input$profil,
                 heure >= debut & heure <= fin)%>%
          group_by(code_region,nb_points_soutirage,profil) %>%
          summarise(Total_energie = sum(as.numeric(total_energie_soutiree_wh), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(conso_sup36_aggregated, by = c("code" = "code_region"))
        
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("region: ", nom, ",", "\nTotal énergie soutirée: ", Total_energie,"\nNb de points d'injection : ",unique(nb_points_soutirage)),
            label = ~paste(nom, ",", "Total énergie soutirée: ", round(Total_energie/10^6,3),"Mkh"),
            labelOptions = labelOptions(
              direction = "center",  
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black")
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
            values = regions_geo$Total_energie,
            title = "Total énergie soutirée",
            opacity = 0.5
          )
      }else {
        conso_sup36_aggregated <- conso_sup36 %>%
          filter(plage_de_puissance_souscrite %in% input$plage_puissance,
                 secteur_activite %in% input$secteur,
                 profil == input$profil,
                 date >= input$date_range[1] & date <= input$date_range[2])%>%
          group_by(code_region) %>%
          summarise(Total_energie = sum(as.numeric(total_energie_soutiree_wh), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(conso_sup36_aggregated, by = c("code" = "code_region"))
        
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("region: ", nom, ",", "\nTotal énergie soutirée: ", Total_energie),
            label = ~paste(nom, ",", "Total énergie soutirée: ", round(Total_energie/10^6,3),"Mkh"),  
            labelOptions = labelOptions(
              direction = "center",  
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black") 
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
            values = regions_geo$Total_energie,
            title = "Total énergie soutirée",
            opacity = 0.5
          )
        
      }
    })
    output$download_data <- downloadHandler(
      filename = function() { 
        paste("prod_energies",input$region,input$profil ,input$date, ".csv", sep = "") 
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
  
  observeEvent(input$tab_36k_inf, {
      output$main_content <- renderUI({
        tagList(h2("Analyse de la Consommation inférieur à 36k "),            
                radioButtons(
                  inputId = "choix_pas",
                  label = "Choisissez le pas de temps :",
                  choices = c("Demi-horaire" = "demi_horaire", "Quotidien" = "quotidien"),
                  selected = "demi_horaire",
                  inline = TRUE
                ),
                fluidRow(
                  column(3,
                         selectInput("region", "Sélectionner les regions", 
                                     choices = unique(conso$region), 
                                     selected = unique(conso$region)[1], 
                                     multiple = TRUE)),
                  column(3,
                         selectInput("profil", "Sélectionner le Profil", 
                                     choices = unique(conso$profil), 
                                     selected = unique(conso$profil)[1]
                         )),
                  column(2,
                         selectInput("plage_puissance", "La plage de puissance", 
                                     choices = unique(conso$plage_de_puissance_souscrite), 
                                     selected = unique(conso$plage_de_puissance_souscrite)[1], 
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
          dateInput("date", "date", value = "2024-06-30")
        } else {
          dateRangeInput("date_range", "Plage de dates", 
                         start = as.Date("2024-06-30") - 5, 
                         end = as.Date("2024-06-30"), 
                         width = 1000)
        }
      })
      filtered_data <- reactive({
        if (input$choix_pas == "demi_horaire") {
          # Filtrer les données selon les intervalles de temps
          debut <- as_hms(paste0(input$heure_debut, ":00"))
          fin <- as_hms(paste0(input$heure_fin,":00"))
          
          conso_filtered <- conso %>%
            filter(region %in% input$region,
                   plage_de_puissance_souscrite %in% input$plage_puissance,
                   date %in% input$date,
                   profil == input$profil,
                   heure >= debut & heure <= fin)
        }
        else {
          conso_filtered <- conso %>%
            filter(region %in% input$region,
                   plage_de_puissance_souscrite %in% input$plage_puissance,
                   profil == input$profil,
                   date >= input$date_range[1] & date <= input$date_range[2])%>%
            group_by(date,region,plage_de_puissance_souscrite) %>%
            summarise(Total_energie = sum(total_energie_soutiree_wh, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
        }
        return(conso_filtered)
      })
      
      output$total_energie <- renderValueBox({
        data <- filtered_data()
        if (input$choix_pas == "demi_horaire") {
          
          total_energie <- sum(data$total_energie_soutiree_wh, na.rm = TRUE)
          valueBox(format(total_energie, big.mark = ","), "Total Energie soutirée", icon = icon("bolt"), color = "light-blue") 
        } else {
          total_energie <- sum(data$Total_energie, na.rm = TRUE)
          valueBox(format(total_energie, big.mark = ","), "Total Energie soutirée", icon = icon("bolt"), color = "light-blue")
        }
      })
      
      output$puissance_moyenne <- renderValueBox({
        data <- filtered_data()
        
        if (input$choix_pas == "demi_horaire") {
          
          puissance_moyenne <- mean(data$total_energie_soutiree_wh, na.rm = TRUE)
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
          
          puissance_max <- max(data$total_energie_soutiree_wh , na.rm = TRUE)
          horodate_max <- data$horodate_clean[which.max(data$total_energie_soutiree_wh)]
          valueBox(format(puissance_max, big.mark = ","), paste("Puissance Max. à", horodate_max), icon = icon("chart-line"), color = "yellow")
        } else {
          puissance_max <- max(data$Total_energie, na.rm = TRUE)
          horodate_max <- data$date[which.max(data$Total_energie)]
          valueBox(format(puissance_max, big.mark = ","), paste("Puissance Max. à", horodate_max), icon = icon("chart-line"), color = "yellow")
        }
      })
      
      output$energy_plot <- renderPlotly({
        data <- filtered_data()
        View(data)
        if (input$choix_pas == "demi_horaire") {
          
          fig <- plot_ly(data, 
                         x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
                         y = ~ total_energie_soutiree_wh/nb_points_soutirage, 
                         color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"),
                         type = 'scatter', mode = 'line', name = ~region) %>%
            add_trace(
              x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
              y = ~ courbe_moyenne_n_1_wh,
              type = 'scatter',
              mode = 'line',
              name = 'Courbe Moyenne n.1',
              line = list(dash = 'dot', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
            ) %>%
            add_trace(
              x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
              y = ~ courbe_moyenne_n_2_wh,
              type = 'scatter',
              mode = 'line',
              name = 'Courbe Moyenne n.2',
              line = list(dash = 'dash', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
            ) %>%
            layout(
              title = "Totale d'énergie injéctées",
              xaxis = list(title = "Plage horaire"),
              yaxis = list(title = "Production")
            )
        }else {
          fig <- plot_ly(data, 
                         x = ~ data$date,
                         y = ~ Total_energie, 
                         color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"),
                         type = 'scatter', mode = 'line', name = ~region) %>%
            add_trace(
              x = ~ data$date,
              y = ~ Courbe_Moyenne1,
              type = 'scatter',
              mode = 'line',
              name = 'Courbe Moyenne n.1',
              line = list(dash = 'dot', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
            ) %>%
            add_trace(
              x = ~ data$date,
              y = ~ Courbe_Moyenne2,
              type = 'scatter',
              mode = 'line',
              name = 'Courbe Moyenne n.2',
              line = list(dash = 'dash', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
            ) %>%
            layout(
              title = "Totale d'énergie injéctées",
              xaxis = list(title = "date"),
              yaxis = list(title = "Production")
            )
        }
        
        fig
        
      })
      output$map <- renderLeaflet({
        if (input$choix_pas == "demi_horaire") {
          debut <- as_hms(paste0(input$heure_debut, ":00"))
          fin <- as_hms(paste0(input$heure_fin,":00"))
          
          conso_aggregated <- conso %>%
            filter(plage_de_puissance_souscrite %in% input$plage_puissance,
                   date %in% input$date,
                   profil == input$profil,
                   heure >= debut & heure <= fin)%>%
            group_by(code_region,nb_points_soutirage,profil) %>%
            summarise(Total_energie = sum(as.numeric(total_energie_soutiree_wh), na.rm = TRUE))
          regions_geo <- regions_geo %>%
            left_join(conso_aggregated, by = c("code" = "code_region"))
          
          leaflet(data = regions_geo) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = ~paste("region: ", nom, ",", "\nTotal énergie soutirée: ", Total_energie,"\nNb de points d'injection : ",unique(nb_points_soutirage)),
              label = ~paste(nom, ",", "Total énergie soutirée: ", round(Total_energie/10^6,3),"Mkh"),
              labelOptions = labelOptions(
                direction = "center",  
                style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black")
              )
            ) %>%
            addLegend(
              position = "bottomleft",
              pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
              values = regions_geo$Total_energie,
              title = "Total énergie soutirée",
              opacity = 0.5
            )
        }else {
          conso_aggregated <- conso %>%
            filter(plage_de_puissance_souscrite %in% input$plage_puissance,
                   profil == input$profil,
                   date >= input$date_range[1] & date <= input$date_range[2])%>%
            group_by(code_region) %>%
            summarise(Total_energie = sum(as.numeric(total_energie_soutiree_wh), na.rm = TRUE))
          regions_geo <- regions_geo %>%
            left_join(conso_aggregated, by = c("code" = "code_region"))
          
          leaflet(data = regions_geo) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = ~paste("region: ", nom, ",", "\nTotal énergie soutirée: ", Total_energie),
              label = ~paste(nom, ",", "Total énergie soutirée: ", round(Total_energie/10^6,3),"Mkh"),  
              labelOptions = labelOptions(
                direction = "center",  
                style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black") 
              )
            ) %>%
            addLegend(
              position = "bottomleft",
              pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
              values = regions_geo$Total_energie,
              title = "Total énergie soutirée",
              opacity = 0.5
            )
          
        }
      })
      output$download_data <- downloadHandler(
        filename = function() { 
          paste("prod_energies",input$region,input$profil ,input$date, ".csv", sep = "") 
        },
        content = function(file) {
          write.csv(filtered_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      )
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
                       selectInput("region", "Sélectionner les regions", 
                                   choices = unique(df$region), 
                                   selected = unique(df$region)[1], 
                                   multiple = TRUE)),
                column(2,
                       selectInput("fliere", "Sélectionner la filère", 
                                   choices = unique(df$filiere_de_production), 
                                   selected = unique(df$filiere_de_production)[1]
                       )),
                column(3,
                       selectInput("plage_puissance", "La plage de puissance", 
                                   choices = unique(df$plage_de_puissance_injection), 
                                   selected = unique(df$plage_de_puissance_injection)[1], 
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
        dateInput("date", "date", value = "2024-06-30")
      } else {
        dateRangeInput("date_range", "Plage de dates", 
                       start = as.Date("2024-06-30") - 5, 
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
          filter(region %in% input$region,
                 plage_de_puissance_injection %in% input$plage_puissance,
                 date %in% input$date,
                 filiere_de_production == input$fliere,
                 heure >= debut & heure <= fin)
      } else {
        if(input$fliere != "F0 : Total toutes filières"){
          df_filtered <- df %>%
            filter(region %in% input$region,
                   plage_de_puissance_injection %in% input$plage_puissance,
                   date >= input$date_range[1] & date <= input$date_range[2],
                   filiere_de_production == input$fliere) %>%
            group_by(date,region,plage_de_puissance_injection) %>%
            summarise(Total_energie = sum(total_energie_injectee_wh/nb_points_injection, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
        }else{
          df_filtered <- df %>%
            filter(region %in% input$region,
                   plage_de_puissance_injection %in% input$plage_puissance,
                   date >= input$date_range[1] & date <= input$date_range[2],
                   filiere_de_production == "F0 : Total toutes filières") %>%
            group_by(date,region,plage_de_puissance_injection) %>%
            summarise(Total_energie = sum(total_energie_injectee_wh, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
        }
      }
      return(df_filtered)
    })
    
    output$total_energie <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(filiere_de_production == "F0 : Total toutes filières")}
        else{data%>% filter(filiere_de_production != "F0 : Total toutes filières")}
        total_energie <- sum(data$total_energie_injectee_wh, na.rm = TRUE)
        valueBox(format(total_energie, big.mark = ","), "Total Energie Injectée", icon = icon("bolt"), color = "light-blue") 
      } else {
        total_energie <- sum(data$Total_energie, na.rm = TRUE)
        valueBox(format(total_energie, big.mark = ","), "Total Energie Injectée", icon = icon("bolt"), color = "light-blue")
      }
    })
    
    output$puissance_moyenne <- renderValueBox({
      data <- filtered_data()
      
      if (input$choix_pas == "demi_horaire") {
        
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(filiere_de_production == "F0 : Total toutes filières")}       
        else{data%>% filter(filiere_de_production != "F0 : Total toutes filières")}
        puissance_moyenne <- mean(data$total_energie_injectee_wh, na.rm = TRUE)
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
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(filiere_de_production == "F0 : Total toutes filières")}
        else{data%>% filter(filiere_de_production != "F0 : Total toutes filières")}
        puissance_max <- max(data$total_energie_injectee_wh , na.rm = TRUE)
        horodate_max <- data$horodate_clean[which.max(data$total_energie_injectee_wh)]
        valueBox(format(puissance_max, big.mark = ","), paste("Puissance Max. à", horodate_max), icon = icon("chart-line"), color = "yellow")
      } else {
        puissance_max <- max(data$Total_energie, na.rm = TRUE)
        horodate_max <- data$date[which.max(data$Total_energie)]
        valueBox(format(puissance_max, big.mark = ","), paste("Puissance Max. à", horodate_max), icon = icon("chart-line"), color = "yellow")
      }
    })
    
    output$energy_plot <- renderPlotly({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(filiere_de_production == "F0 : Total toutes filières")}       
        else{data%>% filter(filiere_de_production != "F0 : Total toutes filières")}
        
        fig <- plot_ly(data, 
                       x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
                       y = ~ total_energie_injectee_wh/nb_points_injection, 
                       color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~region) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ courbe_moyenne_n_1_wh,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"))
          ) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ courbe_moyenne_n_2_wh,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"))
          ) %>%
          layout(
            title = "Totale d'énergie injéctées",
            xaxis = list(title = "Plage horaire"),
            yaxis = list(title = "Production")
          )
      }else {
        fig <- plot_ly(data, 
                       x = ~ data$date,
                       y = ~ Total_energie, 
                       color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~region) %>%
          add_trace(
            x = ~ data$date,
            y = ~ Courbe_Moyenne1,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"))
          ) %>%
          add_trace(
            x = ~ data$date,
            y = ~ Courbe_Moyenne2,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"))
          ) %>%
          layout(
            title = "Totale d'énergie injéctées",
            xaxis = list(title = "date"),
            yaxis = list(title = "Production")
          )
      }
      
      fig
      
    })
    output$map <- renderLeaflet({
      if (input$choix_pas == "demi_horaire") {
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin,":00"))
        
        df_aggregated <- df %>%
          filter(plage_de_puissance_injection %in% input$plage_puissance,
                 date %in% input$date,
                 filiere_de_production == input$fliere,
                 heure >= debut & heure <= fin)%>%
          group_by(code_region,nb_points_injection,filiere_de_production) %>%
          summarise(Total_energie = sum(as.numeric(total_energie_injectee_wh), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(df_aggregated, by = c("code" = "code_region"))
        
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("region: ", nom, ",", "\nTotal énergie injectée: ", Total_energie,"\nNb de points d'injection : ",unique(nb_points_injection)),
            label = ~paste(nom, ",", "Total énergie injectée: ", round(Total_energie/10^6,3),"Mkh"),
            labelOptions = labelOptions(
              direction = "center",  
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black")
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
            values = regions_geo$Total_energie,
            title = "Total énergie injectée",
            opacity = 0.5
          )
      }else {
        df_aggregated <- df %>%
          filter(plage_de_puissance_injection %in% input$plage_puissance,
                 filiere_de_production == input$fliere,
                 date >= input$date_range[1] & date <= input$date_range[2])%>%
          group_by(code_region) %>%
          summarise(Total_energie = sum(as.numeric(total_energie_injectee_wh), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(df_aggregated, by = c("code" = "code_region"))
        
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("region: ", nom, ",", "\nTotal énergie injectée: ", Total_energie),
            label = ~paste(nom, ",", "Total énergie injectée: ", round(Total_energie/10^6,3),"Mkh"),  
            labelOptions = labelOptions(
              direction = "center",  
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black") 
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
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

