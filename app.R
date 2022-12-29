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
library(DT)

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
                     textInput("J1", "Joueur-euse 1", value = "J1")),
              column(width = 2,
                     textInput("J2", "Joueur-euse 2", value = "J2")),
              column(width = 2,
                     textInput("J3", "Joueur-euse 3", value = "J3")),
              column(width = 2,
                conditionalPanel(condition = "input.njoueurs >= 4",
                                 textInput("J4", "Joueur-euse 4", value = "J4"))
                ),
              column(width = 2,
                conditionalPanel(condition = "input.njoueurs >= 5",
                                 textInput("J5", "Joueur-euse 5", value = "J5"))
                ),
              column(width = 12,
                     fileInput("inputtab", "Importer d'anciens scores", 
                               multiple = FALSE, accept = "text/csv")
                     )
              ),
      tabItem(tabName = "recap",
              fluidRow(
                box(h2("Qui prend etc"), width = 4,
                    selectInput("prend", "Qui prend ?",
                                choices = NULL)
                    ),
                box(h2("Scores"), width = 8,
                    dataTableOutput("scores"))
                )
              ),
      tabItem(tabName = "stats",
              h2("Graphiques"))
    )
  )
)

server <- function(input, output, session) { 
  
  # Keep updated joueurs list
  joueurs <- reactive({
    # Initialize choices
    joueurs <- c(input$J1, input$J2, input$J3)
    # Add other players
    if(input$njoueurs >= 4) {
      joueurs <- c(joueurs, input$J4)
    }
    if(input$njoueurs >= 5) {
      joueurs <- c(joueurs, input$J5)
    }
    joueurs
  })
  
  # Initialize scores dataframe
  output$scores <- renderDataTable({
    df <- as.data.frame(matrix(nrow = 0, ncol = length(joueurs())))
    colnames(df) <- joueurs()
    df
  })
  
  
  # Update preneur
  observe({
    updateSelectInput(session = session, 
                      inputId = "prend", choices = joueurs())
  })
  
  }

shinyApp(ui, server)