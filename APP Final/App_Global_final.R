source("R/ui.R")
source("R/server.R")

#lancer l'app
shinyApp(ui = ui, server = server)
