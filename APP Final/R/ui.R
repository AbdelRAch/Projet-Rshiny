library(shinydashboard)
library(shinythemes)

# Interface utilisateur de l'application Shiny:
ui <- dashboardPage(
  dashboardHeader(title = "Analyse de la Production & la consommation d'Energie",titleWidth = 500),
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