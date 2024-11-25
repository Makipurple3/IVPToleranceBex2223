
# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Import DSI ELO and Bex For Maerge

BexDSI <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexDSI.csv")
BexElo <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexElo.csv")
DyadSummary <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/dyad_summary_clean.csv")
BexStat <-read.csv("//Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexStat.csv")


print(BexDSI)
print(BexElo)
print(DyadSummary)
head(BexStat)

colnames(BexDSI)
colnames(BexElo)
colnames(DyadSummary)
colnames(BexStat)


#Variables to select for 
#BexFinal <- Dyad, MeloQ, FEloQ, ZMFEloQ (rename to IELO), IzDSI, FzDSI, TrialDay, DyadDay, Trial, Month, Date, DyadResponse, Tol, Agg, NotApp, Intrud


# Step 1: Standardize column names
colnames(BexDSI)[colnames(BexDSI) == "dyad"] <- "Dyad"
# Step 2: Convert `Dyad` in BexElo to character
BexElo$Dyad <- as.character(BexElo$Dyad)
# Step 3: Select relevant variables from BexStat
BexStat_selected <- BexStat %>%
  select(Dyad, TrialDay, DyadDay, Trial, Date, DyadResponse, Tol, Agg, NotApp, Intrud)

# Step 4: Merge datasets by Dyad
# Merge BexDSI
BexFinal <- BexStat_selected %>%
  left_join(BexDSI %>% select(Dyad, IzDSI, FzDSI), by = "Dyad")
# Merge BexElo
BexFinal <- BexFinal %>%
  left_join(BexElo %>% select(Dyad, MEloQ, FEloQ, ZMFEloQ), by = "Dyad")
# Merge DyadSummary
BexFinal <- BexFinal %>%
  left_join(DyadSummary %>% select(Dyad, AgeDiff, AgeDir), by = "Dyad")

# Step 5: Rename ZMFEloQ to IELO
colnames(BexFinal)[colnames(BexFinal) == "ZMFEloQ"] <- "IzELO"
# Step 6: Retain only the required variables in BexFinal
BexFinal <- BexFinal %>%
  select(Dyad, MEloQ, FEloQ, IzELO, IzDSI, FzDSI, TrialDay, DyadDay, Trial, Date, 
         DyadResponse, Tol, Agg, NotApp, Intrud)

# Step 7: Verify variables in BexFinal
cat("Column names in BexFinal:\n")
print(colnames(BexFinal))

cat("\nPreview of the merged dataset BexFinal:\n")
print(head(BexFinal))

# Step 8: Check for any missing values in the dataset
cat("\nSummary of missing values in BexFinal:\n")
print(colSums(is.na(BexFinal)))



# Convert Date column to Date format
BexFinal <- BexFinal %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
# Verify the structure of BexFinal
str(BexFinal)



# Step 9: Export BexFinal to a CSV file
output_path <- "/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexFinal.csv"
write.csv(BexFinal, output_path, row.names = FALSE)

cat("\nBexFinal has been successfully exported to:", output_path, "\n")






#TASKS
# Check the evolution of the proportion of tolerance per Dyad with tolerance rounded by the week
# Round dates to the nearest week

# Convert Date column to Date format
BexFinal <- BexFinal %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Round dates to the nearest week and calculate weekly averages
tolerance_evolution_week <- BexFinal %>%
  mutate(Week = floor_date(Date, unit = "week")) %>%
  group_by(Dyad, Week) %>%
  summarise(ToleranceProportion = mean(Tol, na.rm = TRUE), .groups = "drop")



# Plot tolerance evolution by week
g# Plot smoothed tolerance evolution by week
ggplot(tolerance_evolution_week, aes(x = Week, y = ToleranceProportion, color = Dyad, group = Dyad)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +  # Smoothed curves
  labs(
    title = "Smoothed Evolution of Tolerance Proportion per Dyad (Rounded to Week)",
    x = "Week",
    y = "Tolerance Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")








# PRediction
# Aggregate data by week for modeling
tolerance_model_data <- tolerance_evolution_week %>%
  group_by(Week) %>%
  summarise(ToleranceProportion = mean(ToleranceProportion, na.rm = TRUE), .groups = "drop")

# Create a time series object
tolerance_ts <- ts(tolerance_model_data$ToleranceProportion, frequency = 52)

# Fit a time series model
tolerance_model <- auto.arima(tolerance_ts)

# Predict for the next 12 weeks
predictions <- forecast(tolerance_model, h = 12)

# Create predicted data
predicted_data <- data.frame(
  Week = seq(max(tolerance_model_data$Week) + weeks(1), 
             by = "week", 
             length.out = 12),
  ToleranceProportion = predictions$mean
)

# Combine observed and predicted data
combined_data <- bind_rows(
  tolerance_evolution_week,
  predicted_data %>% mutate(Dyad = "Prediction")
)




















# see what are the values in IzELO and create a binmoial  
#Like Similar Rank/Different Rank And/Or Female HIgh/Male High deoending which has higher quartile using MElo vs FElo


# Since they are only 4 quartiles ,1,2,3,4, we will asses the follwoing way,
# Using MEloQ and FEloQ we will rate the higher rating individual, is the higher quartile individual is Male then we have MaleHigh if female higher the FemaleHigh




# RESEARCH QUESTION
#Variations in Age, Rank, IDSI, and IEloDiff and time spent together (amount of trials on a set period/in general) can explain differneces in tolerance (aggression/not approaching?!) rates in a forced proximity box experiment conducted in wild vervet monkeys
#The effects may be moderated by /> Seasonality, Male tenure, Female pregnancy

#>Lower variations in age and rank and higher inital social bonds and overall amount of time spent otgether leads to higher rates of tolerance




# STATISTICAL TEST  
#glmer
#{lmerTest} / pTol ~ AmountTrial (totalNtrial  vs fixed amount each dyad) + AgeDiff + IEloDiff + IDsi +(Day| Dyad)



