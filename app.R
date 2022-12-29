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
              h2("Qui joue ?")),
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