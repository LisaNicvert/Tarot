#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(DT)
library(magrittr)
library(dplyr)
library(lubridate)

source("functions.R")


# Global variables --------------------------------------------------------
# Define global threshold variable
thr <- c(56, 51, 41, 36)
names(thr) <- as.character(0:3)

# Define sizes for poignées
poignees <- data.frame(njoueurs = c(3, 4, 5),
                       simple = c(13, 10, 8),
                       double = c(15, 13, 10),
                       triple = c(18, 15, 13))

# Initialize scores dataframe
df <- data.frame("j1" = numeric(0),
                 "j2" = numeric(0),
                 "j3" = numeric(0),
                 "j4" = numeric(0),
                 "j5" = numeric(0),
                 "Preneur" = character(0),
                 "Contrat" = character(0),
                 "Score" = numeric(0),
                 "Bouts" = numeric(0),
                 "Date" = POSIXct(0)
                 )

# ui ----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "TarotCounter"),

## Sidebar -----------------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Qui joue ?", tabName = "quijoue"),
      menuItem("Partie", tabName = "partie"),
      menuItem("Stats", tabName = "stats")
    )
  ),

## Body --------------------------------------------------------------------
  dashboardBody(
    tabItems(

### Qui joue ? -------------------------------------------------------------
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
                               multiple = FALSE, accept = "text/csv"),
                     textOutput("message_input_data")
              )
      ), # tabItem
### Partie -------------------------------------------------------------------
      tabItem(tabName = "partie",
              fluidRow(
                box(h2("Partie en cours"), width = 5,
#### Prise ------------------------------------------------------------------
                    h3("Prise"),
                    selectInput("prend", "Qui prend ?",
                                choices = NULL),
                    selectInput("contrat", "Contrat",
                                choices = list("petite", "pousse", "garde")),
                    checkboxInput("poignee",
                                  "Poignée déclarée ?"),
                    column(12,
                           conditionalPanel(condition = "input.poignee",
                                            selectInput("size_poignee",
                                                        label = "Taille",
                                                        choices = NULL))
                    ),
                    checkboxInput("chelem_annonce",
                                  "Chelem annoncé ?"),
#### Verdict -------------------------------------------------------------
                    h3("Verdict"),
                    conditionalPanel(condition = "input.nplayers == 5",
                                     selectInput("avec", "Qui a été appelé-e ?",
                                                 choices = NULL)
                    ),
                    numericInput("nbouts", "Nombre de bouts", 
                                 value = 0, min = 0, max = 3, step = 1),
                    textOutput("contract_text"),
                    checkboxInput("chelem_reussi",
                                  label = "Chelem réussi ?"),
                    column(12,
                           conditional_en_plus("chelem_reussi")
                    ),
                    checkboxInput("petitbout",
                                  "Petit au bout ?"),
                    column(12,
                           conditional_en_plus("petitbout")
                    ),
                    column(6,
                           style='padding-left:0px; padding-right:5px; padding-top:0px; padding-bottom:0px',
                           numericInput("scorepren", "Score du preneur",
                                        value = NULL, min = 0, max = 91, step = 0.5)
                    ),
                    column(6,
                           style='padding-left:5px; padding-right:0px; padding-top:0px; padding-bottom:0px',
                           numericInput("scorechall", "Score des challengers",
                                        value = NULL, min = 0, max = 91, step = 0.5)
                    ),
                    br(),
#### Sous-total -------------------------------------------------------------
                    h3("Scores de la partie"),
                    dataTableOutput("scores_round"),
                    column(12, align="center",
                           br(),
                           actionButton("addround", "Valider")
                    )
                ), # box
#### Total -------------------------------------------------------------
                box(h2("Scores totaux"), width = 7,
                    dataTableOutput("scores_disp"),
                    conditionalPanel(condition = "input.scores_disp_rows_selected > 0",
                                     style='padding-bottom:15px;',
                                     actionButton("modify", "Modifier ces scores")
                    ),
                    downloadButton('download',"Télécharger les scores"))
              ) # fluidrow
      ), # tabItem

### Stats -------------------------------------------------------------------
      tabItem(tabName = "stats",
              h2("Graphiques")
      )
    ) # tabItems
  ) # dashboardBody
) # dashboardPage



# server ------------------------------------------------------------------
server <- function(input, output, session) { 
## Initialize scores dataframe ---------------------------------------------
  scores <- reactiveValues(data = df)

## Update data with imported scores table ----------------------------------
  # Update input players with old scores table input
  observe({
    # Check that oldscores exist
    req(oldscores())
    
    # Get actual players
    players_NA <- get_NA_players(oldscores())
    playernames <- names(players_NA)[!players_NA]
    nplayers <- length(playernames)
    
    # --- Update nplayers
    updateNumericInput(session = session,
                       "nplayers", value = nplayers)
    
    # --- Update player names inputs values
    updateTextInput(session = session,
                    "J1", value = playernames[1])
    updateTextInput(session = session,
                    "J2", value = playernames[2])
    updateTextInput(session = session,
                    "J3", value = playernames[3])
    if(nplayers >= 4) {
      updateTextInput(session = session,
                      "J4", value = playernames[4])
    }
    if(nplayers >= 5) {
      updateTextInput(session = session,
                      "J5", value = playernames[5])
    }
    
    # --- Update scores
    scores$data <- oldscores()
  }) %>% bindEvent(input$inputtab)
  
  # Get info from uploaded dataframe
  oldscores <- reactive({
    # Run only if an input has been made
    req(input$inputtab)
    
    # Read data
    dat <- input$inputtab
    
    # Check extension
    ext <- tools::file_ext(dat$datapath)
    validate(need(ext == "csv", "Veuillez importer un fichier csv"))
    
    # Read
    df <- read.csv(dat$datapath)
    
    # Check column count
    validate(need(ncol(df) >=8 & ncol(df) <= 10, 
                  "Veuillez importer un fichier valide"))
    
    # Check column names
    validate(need(all(c("Preneur", "Contrat", "Score", "Bouts", "Date") %in% colnames(df)), 
                  "Veuillez importer un fichier valide"))
    
    # Cast last column to POSIXct
    df$Date <- as.POSIXct(df$Date)
    
    # Add NA columns with j4 and j5 if needed
    playercols <- df %>%
      select(-c("Preneur", "Contrat", "Score", "Bouts", "Date"))
    if (ncol(playercols) == 3) {
      df <- df %>% mutate("j4" = NA, 
                          "j5" = NA,
                          .after = 3)
    }
    if (ncol(playercols) == 4) {
      df <- df %>% mutate("j5" = NA, .after = 4)
    }
    df
  })
  
  output$message_input_data <- renderText({
    oldscores()
    "Les données ont été importées avec succès !"
  }) %>% bindEvent(input$inputtab, 
                   ignoreNULL = TRUE, ignoreInit = TRUE)
  
## Players list ------------------------------------------------------------
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
  
  # Update poignee size following njoueurs
  observe({
    poignees_nj <- poignees[poignees$njoueurs == length(players()), ]
    simple <- poignees_nj$simple
    double <- poignees_nj$double
    triple <- poignees_nj$triple
    
    # Create choices list
    choices <- list("simple",
                    "double",
                    "triple")
    names(choices) <- c(paste0("Simple (", simple, ")"),
                        paste0("Double (", double, ")"),
                        paste0("Triple (", triple, ")"))
    
    updateSelectInput(session = session,
                      "size_poignee",
                      choices = choices)
    
  }) %>% bindEvent(input$nplayers)
  
## Update scores with players ----------------------------------------------
  # Modify scores colnames
  observe({
    nplayers <- length(players())
    colnames(scores$data)[1:nplayers] <- players()
  })
  
## Variables for the round -------------------------------------------------
  # Update challenger card points count
  observe({
    updateNumericInput(session = session,
                       "scorepren", value = 91 - input$scorechall)
  }) %>% bindEvent(input$scorechall, ignoreInit = TRUE)
  
  # Update preneur card points count
  observe({
    updateNumericInput(session = session,
                       "scorechall", value = 91 - input$scorepren)
  }) %>% bindEvent(input$scorepren, ignoreInit = TRUE)
  
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

## Compute scores ----------------------------------------------------------
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
    
    # Get petit_bout
    petitbout <- code_bonus(input$petitbout,
                            input$qui_petitbout)
    
    # Get chelem
    chelem_reussi <- code_bonus(input$chelem_reussi,
                                input$qui_chelem_reussi)
    
    # Get poignee
    poignee <- ifelse(input$poignee,
                      input$size_poignee,
                      "0")

    # Compute total
    points <- get_points(scorepren = input$scorepren, 
                         nbouts = input$nbouts,
                         contract = input$contrat, 
                         teams = teams,
                         petitbout = petitbout,
                         chelem_annonce = input$chelem_annonce,
                         chelem_reussi = chelem_reussi,
                         poignee = poignee)

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


## Display scores tables ---------------------------------------------------
  # Display current round scores
  output$scores_round <- renderDataTable({
    df <- scores_round()[1:length(players())]
    DT::datatable(df, 
                  rownames = FALSE,
                  options = list(dom = 't', ordering = FALSE),
                  selection = 'none')
  })
  
  # Display scores dataframe
  output$scores_disp <- renderDataTable({
    nplayers <- length(players())
    
    # Display only relevant scores + preneur
    output_df <- scores$data[c(1:nplayers, 6)]
    DT::datatable(output_df, 
                  selection = list(mode = 'single', selected = NULL))
  })
  
## Modify ----------------------------------------------------------
  observeEvent(input$modify, {
    row <- scores$data[input$scores_disp_rows_selected, ]
    
    # Update preneur
    updateSelectInput(session = session, 
                      inputId = "prend", selected = row$Preneur)
    # Update contrat
    updateSelectInput(session = session, 
                      inputId = "contrat", selected = row$Contrat)
    
  })
  
## Validate round ----------------------------------------------------------
  # Validate
  observeEvent(input$addround, {
    # Check that data is not NA
    if(!is.na(input$scorepren) & !is.na(input$scorechall) &
       all(!is.na(scores_round()[1, players()]))) {
      
      # Get date
      date <- Sys.time()
      
      # Add date to table
      to_add <- scores_round()
      to_add$Date <- date
      
      # Add scores to total scores dataframe
      df_final <- scores$data %>% bind_rows(to_add)
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
  

## Download ----------------------------------------------------------------
  NA_cols <- reactive({
    # Get only "TRUE" players
    get_NA_players(scores$data)
  })
  
  # Download button
  output$download <- downloadHandler(
    filename = function(){
      # Get player names
      NA_players <- NA_cols()
      non_NA <- names(NA_players)[!NA_players]
      
      # Get file name
      fname <- paste0("tarot_",
                      paste(non_NA, collapse = "_"),
                      ".csv")
      fname}, 
    content = function(fname){
      # Get only non-NA players
      NA_players <- NA_cols()
      index_NA_players <- which(NA_players)
      if(length(index_NA_players) != 0) { # If some scores are NAs
        to_write <- scores$data[, -index_NA_players]
      } else { # No scores are NA: we want the whole table
        to_write <- scores$data
      }
      
      
      # Write
      write.csv(to_write, fname,
                row.names = FALSE)
    }
  )
  
  }


# app ---------------------------------------------------------------------
shinyApp(ui, server)