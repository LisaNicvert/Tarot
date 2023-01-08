source("functions.R")

teams <- c("preneur", "challenger", "challenger")
teams <- c("preneur", "challenger", "challenger", "challenger", "avec")
teams <- c("preneur", "challenger", "challenger", "challenger", "challenger")

t <- get_points(41, 2, "petite", teams)

# Example df
# df <- read.csv("tests/tarot_J1_J2_J3.csv")
df <- read.csv("tests/tarot_Bonjour_Je_Suis_Le_Champignon.csv")

# Test code to get columns with all NAs
NA_cols <- apply(df[, 1:5], 2, function(c) all(is.na(c)))
index_NA_players <- which(NA_cols)

df[, -index_NA_players]

# Test code to get players columns
df %>% select(-c("Preneur", "Contrat", "Score", "Bouts", "Date"))
