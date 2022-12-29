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