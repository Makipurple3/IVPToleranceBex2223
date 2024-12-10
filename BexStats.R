# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(lmerTest)
library(effects)
library(car)

# Import DSI ELO and Bex For Maerge
BexDSI <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexDSI.csv")
BexElo <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexElo.csv")
DyadSummary <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/dyad_summary_clean.csv")
BexStat <-read.csv("//Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexStat.csv")
MaleSummary <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/MaleSummary.csv")

print(BexDSI)
print(BexElo)
print(DyadSummary)
head(BexStat)
print(MaleSummary)

colnames(BexDSI)
colnames(BexElo)
colnames(DyadSummary)
colnames(BexStat)
colnames(MaleSummary)


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
  left_join(BexDSI %>% select(Dyad, IzDSI,), by = "Dyad")
# Merge BexElo
BexFinal <- BexFinal %>%
  left_join(BexElo %>% select(Dyad, MEloQ, FEloQ, ZMFEloQ), by = "Dyad")
# Merge DyadSummary
BexFinal <- BexFinal %>%
  left_join(DyadSummary %>% select(Dyad, AgeDiff, AgeDir, TotalTrials), by = "Dyad")
# Step 5: Rename ZMFEloQ to IELO
colnames(BexFinal)[colnames(BexFinal) == "ZMFEloQ"] <- "IzELO"
# Step 6: Retain only the required variables in BexFinal
BexFinal <- BexFinal %>%
  select(Dyad, MEloQ, FEloQ, IzELO, IzDSI, FzDSI, TrialDay, DyadDay, Trial,TotalTrials, Date, 
         DyadResponse, Tol, Agg, NotApp, Intrud, AgeDiff, AgeDir)
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










# OPEN BEXFINAL

BexFinal <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexFinal.csv")

colnames(BexFinal)

#TASKS
# Check the evolution of the proportion of tolerance per Dyad with tolerance rounded by the week
# Round dates to the nearest week
# Convert Date column to Date format
BexFinal <- BexFinal %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

#BY WEEK
# Smooth plot of dyad evolution and tolerance: BY WEEK
# Round dates to the nearest week and calculate weekly averages
tolerance_evolution_week <- BexFinal %>%
  mutate(Week = floor_date(Date, unit = "week")) %>%
  group_by(Dyad, Week) %>%
  summarise(ToleranceProportion = mean(Tol, na.rm = TRUE), .groups = "drop")

# Plot smoothed tolerance evolution by week
ggplot(tolerance_evolution_week, aes(x = Week, y = ToleranceProportion, color = Dyad, group = Dyad)) +
  geom_smooth(se = FALSE, method = "lm", span = 0.3) +  # Smoothed curves
  labs(
    title = "Smoothed Evolution of Tolerance Proportion per Dyad (Rounded to Week)",
    x = "Week",
    y = "Tolerance Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
# Plot LINES tolerance evolution by month
ggplot(tolerance_evolution_week, aes(x = Week, y = ToleranceProportion, color = Dyad, group = Dyad)) +
  geom_smooth(se = FALSE, method = "lm")   +  # Smoothed curves
  labs(
    title = "Smoothed Evolution of Tolerance Proportion per Dyad (Rounded to Week)",
    x = "Week",
    y = "Tolerance Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


#BY MONTH
# Smooth plot od dyad evolution and tolerance: BY MONTH
# Round dates to the nearest week and calculate monthly averages
tolerance_evolution_month <- BexFinal %>%
  mutate(Month = floor_date(Date, unit = "month")) %>%
  group_by(Dyad, Month) %>%
  summarise(ToleranceProportion = mean(Tol, na.rm = TRUE), .groups = "drop")
# Plot smoothed tolerance evolution by month
ggplot(tolerance_evolution_month, aes(x = Month, y = ToleranceProportion, color = Dyad, group = Dyad)) +
  geom_smooth(se = FALSE, method = "lm", span = 0.4) +  # Smoothed curves
  labs(
    title = "Smoothed Evolution of Tolerance Proportion per Dyad (Rounded to Month)",
    x = "Month",
    y = "Tolerance Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
# Plot LINES tolerance evolution by month
ggplot(tolerance_evolution_month, aes(x = Month, y = ToleranceProportion, color = Dyad, group = Dyad)) +
  geom_smooth(se = FALSE, method = "lm")   +  # Smoothed curves
  labs(
    title = "Smoothed Evolution of Tolerance Proportion per Dyad (Rounded to Month)",
    x = "Month",
    y = "Tolerance Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# see what are the values in IzELO and create a binmoial  
#Like Similar Rank/Different Rank And/Or Female HIgh/Male High deoending which has higher quartile using MElo vs FElo
# Since they are only 4 quartiles ,1,2,3,4, we will asses the follwoing way,
# Using MEloQ and FEloQ we will rate the higher rating individual, is the higher quartile individual is Male then we have MaleHigh if female higher the FemaleHigh



# UPDATES AND CREATION OF COLUMNS IN BEXFINAL


#MAX TRIAL DAY
# Create max trial day to compute statistics on dyad day
# Add a new column with the maximum TrialDay per Dyad and Date
BexFinal <- BexFinal %>%
  group_by(Dyad, Date) %>%
  mutate(MaxTrialDay = max(TrialDay)) %>%
  ungroup()

# SEASONS
# Add a "Season" column based on the Date
BexFinal <- BexFinal %>%
  mutate(Season = case_when(
    month(Date) %in% c(9, 10, 11) ~ "Spring",
    month(Date) %in% c(12, 1, 2) ~ "Summer",
    month(Date) %in% c(3, 4, 5) ~ "Autumn",
    month(Date) %in% c(6, 7, 8) ~ "Winter",
    TRUE ~ NA_character_  # Handles missing or invalid dates
  ))

# VERVET SEASONS
# Ass a vervet sweasons with amting season and bb season
# Create "VervetSeason" based on months
BexFinal <- BexFinal %>%
  mutate(VervetSeason = case_when(
    month(Date) %in% c(1, 2, 3) ~ "Summer",
    month(Date) %in% c(4, 5, 6) ~ "Mating",
    month(Date) %in% c(7, 8, 9) ~ "Winter",
    month(Date) %in% c(10, 11, 12) ~ "Baby",
    TRUE ~ NA_character_  # Default for missing dates
  ))

# Change format Season & Vevert Season
BexFinal$Season <- factor(BexFinal$Season, levels = c("Winter", "Spring",  "Summer", "Autumn"), ordered = T)
# Change format Season & Vevert Season
BexFinal$VervetSeason <- factor(BexFinal$VervetSeason, levels = c("Summer","Mating", "Winter","Baby"), ordered = T)









# RESEARCH QUESTION
#Variations in Age, Rank, IDSI, and IEloDiff and time spent together (amount of trials on a set period/in general) can explain differences in tolerance (aggression/not approaching?!) rates in a forced proximity box experiment conducted in wild vervet monkeys
#The effects may be moderated by /> Seasonality, Male tenure, Female pregnancy
# Add Male/Female Higher for ELO , column "Higher" M or F diffelo*higher > see if effect of sex + rank on tolerance<
# Add Male/Female higher for AGE > same idea as Elo

#>Lower variations in age and rank and higher initial social bonds and overall amount of time spent together leads to higher rates of tolerance




# STATISTICAL TEST  
#glmer
#{lmerTest} / pTol ~ AmountTrial (totalNtrial  vs fixed amount each dyad) + AgeDiff + IEloDiff + IDsi +(Day| Dyad)

# > > > Use Totaltrials from DyadSummary and include it back in BexFinal?

cdplot(factor(Tol) ~  IzDSI, data = BexFinal)

cdplot(factor(Tol) ~  TotalTrials, data = BexFinal)

cdplot(factor(Tol) ~  IzELO, data = BexFinal)

cdplot(factor(Tol) ~  AgeDiff, data = BexFinal)


#ANAYLSIS
# ORDER SEASONS FOR STATS


xtabs(~ Tol+ IzELO, data= BexFinal)
xtabs(~ Tol+ Dyad, data= BexFinal)

BexFinal$IzELO <- as.factor(BexFinal$IzELO)
require(party)
plot(ctree(factor(Tol) ~  IzDSI + factor(IzELO) + TotalTrials + AgeDiff, data = BexFinal))

dat0 <- aggregate(Tol~ Dyad + IzDSI + IzELO + TotalTrials + AgeDiff + Season + VervetSeason, sum, data = BexFinal)




mod0 <- glmer(cbind(Tol, TotalTrials - Tol) ~  IzDSI + IzELO + AgeDiff + Season + (1|Dyad), binomial,    data = dat0)
mod1 <- glmer(cbind(Tol, TotalTrials - Tol) ~  IzDSI + IzELO + AgeDiff + VervetSeason + (1|Dyad), binomial,    data = dat0)
summary(mod0)
Anova(mod0)


plot(effect( "IzELO", mod0), type="response")
plot(effect( "AgeDiff", mod0))
plot(effect( "IzDSI", mod0))



plot(effect( "Season", mod0))

# TASK: ANOVA mod 1 & mod 0 
# A) Create mod 1 & mod 0, 1 VVSeason on Season, both ordered
(anova(mod0, mod1))
# nod0 is better model, Season is better predictor than VervetSeason
# LOWER AIC Score when running anova between two models is the best













 # Seasonality, male tenure and pregnancy?
#> Import Male tenure

# Check how to do seasonality





