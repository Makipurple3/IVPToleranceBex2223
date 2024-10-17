
# ELO RATING 



# Load necessary libraries
library(EloRating)
library(dplyr)
library(ggplot2)
library(data.table)

# Load presence data
AKpres <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/pres_ak.csv")
colnames(AKpres)[1] <- "Date"
AKpres$Date <- as.Date(AKpres$Date, format = "%Y-%m-%d")
AKpres <- AKpres[!is.na(AKpres$Date), ]

# Load interaction data
interactions <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/WinnerLoser.csv")
interactions$Date <- as.Date(interactions$Date, format = "%d/%m/%Y")

# Step 1: Filter interactions for adult females in the Ankhase group
AF <- interactions %>%
  filter(AgeClassWinner %in% c("AF")) %>%
  filter(AgeClassLoser %in% c("AF")) %>%
  filter(Group == "Ankhase")

# Check presence and interaction alignment with seqcheck
seqcheck(winner = AF$winner, loser = AF$loser, Date = AF$Date, presence = AKpres)

# Step 2: Run Elo rating for Ankhase females
AKELO <- elo.seq(
  winner = AF$winner,
  loser = AF$loser,
  Date = AF$Date,
  presence = AKpres,
  runcheck = FALSE
)

# Step 3: Plot the Elo score evolution
eloplot(AKELO, ids = c("Ginq", "Gubh", "Ndaw", "Nkos", "Ghid", "Ndon", "Ncok"), 
        from = "2022-06-01", to = "2023-10-01")

# Step 4: Extract Elo scores for a specific period
extract_elo(AKELO, "2023-06-01", daterange = 120, standardize = TRUE)

# Plot the final scores
final_scores <- extract_elo(AKELO, "2023-10-01", daterange = 120, standardize = TRUE)
print(final_scores)
