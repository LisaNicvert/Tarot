#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "TarotCounter"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Qui joue ?", tabName = "quijoue"),
      menuItem("Récap", tabName = "recap"),
      menuItem("Stats", tabName = "stats")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "quijoue",
              column(width = 12,
                h2("Qui joue ?"),
                numericInput("njoueurs", "Nombre de joueurs", 
                             value = 3, min = 3, max = 5, step = 1,
                             width = "150px")
                ),
              column(width = 2,
                     textInput("J1", "Joueur-euse 1")),
              column(width = 2,
                     textInput("J2", "Joueur-euse 2")),
              column(width = 2,
                     textInput("J3", "Joueur-euse 3")),
              column(width = 2,
                conditionalPanel(condition = "input.njoueurs >= 4",
                                 textInput("J4", "Joueur-euse 4"))
                ),
              column(width = 2,
                conditionalPanel(condition = "input.njoueurs >= 5",
                                 textInput("J5", "Joueur-euse 5"))
                ),
              column(width = 12,
                     fileInput("inputtab", "Importer d'anciens scores", 
                               multiple = FALSE, accept = "text/csv")
                     )
              ),
      tabItem(tabName = "recap",
              fluidRow(
                box(h2("Qui prend etc")),
                box(h2("Tableau"))
                )
              ),
      tabItem(tabName = "stats",
              h2("Graphiques"))
    )
  )
)

server <- function(input, output) { }

shinyApp(ui, server)