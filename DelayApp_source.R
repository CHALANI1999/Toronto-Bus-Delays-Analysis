library(shiny)
library(shinydashboard)



# Load the UI code from ui.R
source("D:/3RD YEAR/SEMESTER II/ST3082/Project II Materials/DelayApp_UI.R")
#source("D:/3RD YEAR/SEMESTER II/ST3082/Project II Materials/D_UI.R")

# Load the server code from server.R
source("D:/3RD YEAR/SEMESTER II/ST3082/Project II Materials/DelayApp_server.R")


# Run the app
shinyApp(ui, server)