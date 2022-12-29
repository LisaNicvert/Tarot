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
library(dplyr)

source("functions.R")

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
                numericInput("nplayers", "Nombre de joueurs", 
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
                conditionalPanel(condition = "input.nplayers >= 4",
                                 textInput("J4", "Joueur-euse 4", value = "J4"))
                ),
              column(width = 2,
                conditionalPanel(condition = "input.nplayers >= 5",
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
                    conditionalPanel(condition = "input.nplayers == 5",
                      selectInput("avec", "Avec qui ?",
                                  choices = NULL)
                      ),
                    selectInput("contrat", "Contrat",
                                choices = list("petite", "pousse", "garde")),
                    numericInput("nbouts", "Bouts", 
                                 value = 0, min = 0, max = 3, step = 1),
                    textOutput("contract_text"),
                    column(6, 
                           style='padding-left:0px; padding-right:5px; padding-top:0px; padding-bottom:0px',
                           numericInput("scorepren", "Score du preneur", 
                                        value = NULL, min = 0, max = 91, step = 0.5)
                           ),
                    column(6, 
                           style='padding-left:0px; padding-right:5px; padding-top:0px; padding-bottom:0px',
                           numericInput("scorechall", "Score des challengers", 
                                        value = NULL, min = 0, max = 91, step = 0.5)
                           ),
                    h3("Scores finaux"),
                    dataTableOutput("scores_round"),
                    column(12, align="center",
                           br(),
                           actionButton("addround", "Valider")
                    )
                    ),
                box(h2("Scores"), width = 8,
                    dataTableOutput("scores_disp"),
                    downloadButton('download',"Télécharger les scores"))
                )
              ),
      tabItem(tabName = "stats",
              h2("Graphiques"))
    )
  )
)

server <- function(input, output, session) { 
  
  # Keep updated players list
  players <- reactive({
    # Initialize choices
    players <- c(input$J1, input$J2, input$J3)
    # Add other players
    if(input$nplayers >= 4) {
      players <- c(players, input$J4)
    }
    if(input$nplayers >= 5) {
      players <- c(players, input$J5)
    }
    players
  })
  
  # Initialize scores dataframe
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
  
  scores <- reactiveValues(data = df)
  
  # Modify scores colnames
  observe({
    nplayers <- length(players())
    colnames(scores$data)[1:nplayers] <- players()
  })
  
  # Compute scores for current round
  scores_round <- reactive({
    # Get number of players
    nplayers <- length(players())
    # Initialize teams
    teams <- rep("challenger", nplayers)
    
    # Modify preneur
    teams[which(players() == input$prend)] <- "preneur"
    
    # Modify avec
    if (nplayers == 5) { # Look for a 'with'
      if(input$avec != "alone") {
        teams[which(players() == input$avec)] <- "avec"
      }
    }
    
    points <- get_points(scorepren = input$scorepren, 
                         nbouts = input$nbouts,
                         contract = input$contrat, 
                         teams = teams)
    points <- c(points, rep(NA, 5 - length(points)))
    points_list <- as.list(points)
    
    other_info <- list(input$prend,
                       input$contrat,
                       input$scorepren,
                       input$nbouts)

    points_df <- as.data.frame(c(points_list, other_info))
    
    # Initialize names
    colnames(points_df) <- c("j1", "j2", "j3", "j4", "j5", 
                             "Preneur", "Contrat", "Score", "Bouts")
    # Add actual player names
    colnames(points_df)[1:nplayers] <- players()
    
    points_df
  })
  
  # Display current round scores
  output$scores_round <- renderDataTable({
    df <- scores_round()[1:length(players())]
    DT::datatable(df, 
                  rownames = FALSE,
                  options = list(dom = 't', ordering = FALSE))
  })
  
  # Display scores dataframe
  output$scores_disp <- renderDataTable({
    nplayers <- length(players())
    
    # Display only relevant scores
    output_df <- scores$data[c(1:nplayers, 6:ncol(scores$data))]
    output_df
  })
  
  # Display contract
  output$contract_text <- renderText({
    paste("Le preneur doit faire au moins", thr[as.character(input$nbouts)], "points.")
  })
  
  # Update preneur
  observe({
    updateSelectInput(session = session, 
                      inputId = "prend", choices = players())
  })
  
  # Update avec
  observe({
    # Get all players
    all_players <- players()
    # Remove preneur
    no_preneur <- all_players[all_players != input$prend]
    
    # Add an 'alone' choice
    with_choices <- as.list(c(no_preneur, "alone"))
    names(with_choices) <- c(no_preneur, 
                             paste(input$prend, "est tout seul-e !"))
    updateSelectInput(session = session, 
                      inputId = "avec", choices = with_choices)
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
  
  
  # Validate
  observeEvent(input$addround, {
    # Check that data is not NA
    if(!is.na(input$scorepren) & !is.na(input$scorechall) &
       all(!is.na(scores_round()[1, players()]))) {
      
      # Add scores to total scores dataframe
      df_final <- scores$data %>% bind_rows(scores_round())
      scores$data <- df_final
      
      # Reinitialize inputs
      updateSelectInput(session = session, "prend",
                        selected = NULL)
      updateSelectInput(session = session, "contrat",
                        selected = "petite")
      updateNumericInput(session = session, "nbouts",
                         value = 0)
      updateNumericInput(session = session, "scorepren", 
                         value = NA)
      updateNumericInput(session = session, "scorechall", 
                         value = NA)
    }
    
  })
  
  # Download button
  output$download <- downloadHandler(
    filename = function(){"tarot.csv"}, 
    content = function(fname){
      write.csv(scores$data, fname)
    }
  )
  
  }

shinyApp(ui, server)