#' Get point count
#' 
#' Get the number of points won for each player
#'
#' @param scorepren The score of the preneur team
#' @param nbouts Oudler counts
#' @param contract The contract done (petite, pousse, garde)
#' @param teams A vector giving the order of the teams. Can have values
#' 'preneur', 'challenger', 'avec'
#'
#' @return A vector of scores in the same order as the input teams vector.
get_points <- function(scorepren, nbouts,
                       contract, teams) {
  
  if(is.na(scorepren)) {
    res <- rep(NA, length(teams))
    return(res)
  }
  
  # Initialize parameters ---
  # Winning threshold
  thr <- c(56, 51, 41, 36)
  names(thr) <- as.character(0:3)
  # Contract coefficients
  coeff <- c(1, 2, 3)
  names(coeff) <- c("petite", "pousse", "garde")
  
  # Difference between contract and threshold ---
  difference <- scorepren - thr[as.character(nbouts)]
  
  # Get people teaming with player
  nwith <- length(teams[teams == "avec"])
  # Get njoueurs
  njoueurs <- length(teams)
  
  # Initialize scores vector ---
  round_score <- numeric(length(teams))
  
  # Compute scores ---
  if (difference >=0) { # Case the preneur won
    # 25 points added because respected contract
    raw_score <- (25 + difference)*coeff[contract]
  } else { # They lost
    raw_score <- difference*coeff[contract]
  }
  
  if (nwith == 0) { # preneur is alone
    round_score[which(teams == "preneur")] <- raw_score*(njoueurs-1)
    round_score[which(teams == "challenger")] <- - raw_score
  } else { # There is a traitor
    round_score[which(teams == "preneur")] <- raw_score*2
    round_score[which(teams == "avec")] <- raw_score
    round_score[which(teams == "challenger")] <- - raw_score
  }
  return(round_score)
}

#' Get columns with only NAs
#' 
#' Get columns with only NAs in the first 5 columns of a df.
#' Useful to discriminate actual players from placeholders used in the data
#'
#' @param df The dataframe
#'
#' @return A vector of length 5 named with column names containing
#' TRUE if all values were NA, FALSE else.
get_NA_players <- function(df) {
  NA_cols <- apply(df[, 1:5], 2, 
                   function(c) all(is.na(c)))
  return(NA_cols)
}

#' Conditional panel
#'
#' This function creates a selectInput widget with 2 choices
#' based on the condition of id.
#' This is used for the additional points (petit au bout, chelem, 
#' poignées).
#' 
#' @param id The id of the widget the condition is based on
#'
#' @return a conditionalPanel containing a selectInput list with 2 choices,
#' with the id "qui-id"
conditional_en_plus <- function(id) {
  
  conditionalPanel(condition = paste("input", id, sep = "."),
                   selectInput(paste("input", id, sep = "_"),
                               label = "Pour", 
                               choices = list("Le preneur-euse" = "preneur",
                                              "Les challengeur-euses" = "challengers"))
  )
} 
