#' Get point count
#' 
#' Get the number of points won for each player
#'
#' @param scorepren The score of the preneur team
#' @param nbouts Oudler counts
#' @param contract The contract done (petite, pousse, garde)
#' @param teams A vector giving the order of the teams. Can have values
#' 'preneur', 'challenger', 'avec'
#' @param petitbout A coded value for 'petit au bout' : P, C or 0.
#' @param chelem A coded value for 'chelem' : P, C or 0.
#' @param chelem_annonce TRUE or FALSE
#' @param chelem_reussi TRUE or FALSE
#' @param poignee A coded value for 'poignee' : P, C or 0.
#' @param size_poignee "simple", "double" or "triple
#'
#' @return A vector of scores in the same order as the input teams vector.
get_points <- function(scorepren, nbouts,
                       contract, teams,
                       petitbout,
                       chelem,
                       chelem_annonce,
                       chelem_reussi,
                       poignee,
                       size_poignee) {
  
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
  # Petit au bout
  petitbout_bonus <- 10
  # 'Poignee'
  poignee_bonus <- c(20, 30, 40)
  names(poignee_bonus) <- c("simple", "double", "triple")
  # Chelem
  chelem_bonus <- c(200, 400)
  names(chelem_bonus) <- c("annonce", "non_annonce")
  
  # Difference between contract and threshold ---
  difference <- scorepren - thr[as.character(nbouts)]
  
  # Get people teaming with player
  nwith <- length(teams[teams == "avec"])
  # Get njoueurs
  njoueurs <- length(teams)
  
  # Get 'petit au bout' score
  if (petitbout == "0") {
    petitbout_score <- 0
  } else {
    if(difference >= 0) { # preneur won
      if (petitbout == "P") {
        petitbout_score <- petitbout_bonus*coeff[contract]
      } else {
        petitbout_score <- -petitbout_bonus*coeff[contract]
      }
    } else {
      if (petitbout == "C") {
        petitbout_score <- petitbout_bonus*coeff[contract]
      } else {
        petitbout_score <- -petitbout_bonus*coeff[contract]
      }
    }
  }
  
  # Get 'poignées' score
  if (poignee == "0") {
    poignee_score <- 0
  } else { # Score depends on the size of the 'poignée'
    # Multiplied by the number of challengers
    poignee_score <- poignee_bonus[size_poignee]
  }
  
  # Get chelem score
  if (chelem == "0") {
    chelem_score <- 0
  } else {
    if (chelem == 'P') { # preneur made a chelem
      # Chelem annoncé -> 400
      if (chelem_annonce & chelem_reussi) {
        chelem_score <- chelem_bonus["annonce"]
      } else if (chelem_annonce & !chelem_reussi) {
        # Chelem annoncé mais non réussi -> -200
        chelem_score <- -chelem_bonus["non_annonce"]
      } else if (!chelem_annonce & chelem_reussi) {
        # Chelem non annoncé -> 200
        chelem_score <- chelem_bonus["non_annonce"]
      } else if (!chelem_annonce & !chelem_reussi) {
        # Chelem non-annoncé et non réussi -> pas de chelem (avoid bugs)
        chelem_score <- 0
      }
    } else if (chelem == "C") {
      # If challengers made a chelem they gain 200 points
      chelem_score <- chelem_bonus["non_annonce"]
    }
  }
  
  # Initialize scores vector ---
  round_score <- numeric(length(teams))
  
  # Compute scores ---
  raw_score <- (25 + abs(difference))*coeff[contract]

  # Add bonuses
  raw_score <- raw_score + petitbout_score + chelem_score + poignee_score
  
  if (nwith == 0) { # preneur is alone
    if (difference >= 0) { # preneur won
      round_score[which(teams == "preneur")] <- raw_score*(njoueurs-1)
      round_score[which(teams == "challenger")] <- -raw_score
    } else {
      round_score[which(teams == "preneur")] <- -raw_score*(njoueurs-1)
      round_score[which(teams == "challenger")] <- raw_score
    }
  } else { # There is a traitor
    if (difference >= 0) { # preneur won
      round_score[which(teams == "preneur")] <- raw_score*2
      round_score[which(teams == "avec")] <- raw_score
      round_score[which(teams == "challenger")] <- -raw_score
    } else {
      round_score[which(teams == "preneur")] <- -raw_score*2
      round_score[which(teams == "avec")] <- -raw_score
      round_score[which(teams == "challenger")] <- raw_score
    }
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
                   selectInput(paste("qui", id, sep = "_"),
                               label = "Pour", 
                               choices = list("Le preneur-euse" = "preneur",
                                              "Les challengeur-euses" = "challengers"))
  )
} 


#' Code bonus status
#'
#' Code values for bonus points (petit au bout, chelem or poignée)
#'
#' @param input The input value (TRUE or FALSE)
#' @param input_qui Who got the input ("preneur" or "challengers")
#'
#' @return A code for the bonus: P, C or 0
code_bonus <- function(input, input_qui) {
  
  if (input) {
    res <- ifelse(input_qui == "preneur",
                  "P", "C")
  } else {
    res <- "0"
  }
  return(res)
}
