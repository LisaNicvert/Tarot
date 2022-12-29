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
library(magrittr)

# Define global threshold variable
thr <- c(56, 51, 41, 36)
names(thr) <- as.character(0:3)

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
                box(h2("Partie en cours"), width = 4,
                    selectInput("prend", "Qui prend ?",
                                choices = NULL),
                    selectInput("contrat", "Contrat",
                                choices = list("petite", "pousse", "garde")),
                    numericInput("nbouts", "Bouts", 
                                 value = 0, min = 0, max = 3, step = 1),
                    textOutput("contract_text"),
                    numericInput("scorepren", "Score du preneur", 
                                 value = 0, min = 0, max = 91, step = 0.5),
                    numericInput("scorechall", "Score des challengers", 
                                 value = 0, min = 0, max = 91, step = 0.5)
                    ),
                box(h2("Scores"), width = 8,
                    dataTableOutput("scores_disp"))
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
  scores <- reactive({
    df <- data.frame("j1" = numeric(0),
                     "j2" = numeric(0),
                     "j3" = numeric(0),
                     "j4" = numeric(0),
                     "j5" = numeric(0),
                     "Preneur" = character(0),
                     "Contrat" = character(0),
                     "Score" = numeric(0),
                     "Bouts" = numeric(0)
                     )
    njoueurs <- length(joueurs())
    colnames(df)[1:njoueurs] <- joueurs()
    df
  })
  
  # Display scores dataframe
  output$scores_disp <- renderDataTable({
    njoueurs <- length(joueurs())
    
    # Display only relevant scores
    output_df <- scores()[c(1:njoueurs, 6:ncol(scores()))]
    output_df
  })
  
  # Display contract
  output$contract_text <- renderText({
    paste("Le preneur doit faire au moins", thr[as.character(input$nbouts)], "points.")
  })
  
  # Update preneur
  observe({
    updateSelectInput(session = session, 
                      inputId = "prend", choices = joueurs())
  })
  
  # Update challenger score
  observe({
    updateNumericInput(session = session,
                       "scorepren", value = 91 - input$scorechall)
  }) %>% bindEvent(input$scorechall, ignoreInit = TRUE)
  # Update preneur score
  observe({
    updateNumericInput(session = session,
                       "scorechall", value = 91 - input$scorepren)
  }) %>% bindEvent(input$scorepren, ignoreInit = TRUE)
  
  }

shinyApp(ui, server)