RStudio.Version()


# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(lmerTest)
library(effects)
library(car)

# Import DSI ELO and Bex For Maerge
BexDSI <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexDSI.csv")
BexElo <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/BexElo.csv")
DyadSummary <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/dyad_summary_clean.csv")
BexStat <-read.csv("//Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexStat.csv")
MaleSummary <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/MaleSummary.csv")
BexMothers <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexMothers.csv")

print(BexDSI)
print(BexElo)
print(DyadSummary)
head(BexStat)
print(MaleSummary)
print(BexMothers)

colnames(BexDSI)
colnames(BexElo)
colnames(DyadSummary)
colnames(BexStat)
colnames(MaleSummary)
colnames(BexMothers)

str(BexElo)

#Variables to select for 
#BexFinal <- Dyad, MeloQ, FEloQ, ZMFEloQ (rename to IELO), IzDSI, FzDSI, TrialDay, DyadDay, Trial, Month, Date, DyadResponse, Tol, Agg, NotApp, Intrud
# Step 1: Standardize column names
colnames(BexDSI)[colnames(BexDSI) == "dyad"] <- "Dyad"
# Step 2: Convert `Dyad` in BexElo to character
BexElo$Dyad <- as.character(BexElo$Dyad)
# Step 3: Select relevant variables from BexStat
BexStat_selected <- BexStat %>%
  select(Dyad, Male, Female, TrialDay, DyadDay, Trial, Date, DyadResponse, Tol, Agg, NotApp, Intrud)
# Step 4: Merge datasets by Dyad
# Merge BexDSI
BexFinal <- BexStat_selected %>%
  left_join(BexDSI %>% select(Dyad, IzDSI,), by = "Dyad")
# Merge BexElo
BexElo <- BexElo %>%
  mutate(HigherElo = case_when(
    MElo > FElo ~ "Male",     # If the male Elo score is higher
    MElo < FElo ~ "Female",   # If the female Elo score is higher
    MElo == FElo ~ "Equal"    # If both Elo scores are the same
  ))
BexFinal <- BexFinal %>%
  left_join(BexElo %>% select(Dyad, MEloQ, FEloQ, ZMFEloQ, HigherElo), by = "Dyad")

# Merge DyadSummary
BexFinal <- BexFinal %>%
  left_join(DyadSummary %>% select(Dyad, AgeDiff, AgeDir, TotalTrials), by = "Dyad")

#Merge Mother into Bex stat
# Rename Mother code into 
BexFinal <- BexFinal %>%
  left_join(BexMothers, by = c("Female" = "MotherCode"))

BexFinal <- BexFinal%>%
  mutate(BB = ifelse(as.Date(Date)>as.Date(FirstRecorded),"Yes","No"),
         BB = ifelse(!is.na(LastSeen1), ifelse(as.Date(Date)>as.Date(LastSeen1), "No",BB),BB)) 

# Merge Male Tenure
BexFinal <- BexFinal %>%
  left_join(MaleSummary %>% select(MaleID, TenureYears), by = c("Male" = "MaleID"))


# Step 5: Rename ZMFEloQ to IELO
colnames(BexFinal)[colnames(BexFinal) == "ZMFEloQ"] <- "IzELO"
# Step 6: Retain only the required variables in BexFinal
BexFinal <- BexFinal %>%
  select(Dyad, MEloQ, FEloQ, IzELO, IzDSI, FzDSI, TrialDay, DyadDay, Trial,TotalTrials, Date, 
         DyadResponse, Tol, Agg, NotApp, Intrud, AgeDiff, AgeDir, BB, TenureYears, HigherElo)
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



# Higher ELO states which of male or femlae has a higher Elo, the same has been done for
# Since they are only 4 quartiles ,1,2,3,4, we will asses the follwoing way,
# Using MEloQ and FEloQ we will rate the higher rating individual, is the higher quartile individual is Male then we have MaleHigh if female higher the FemaleHigh

# Age Diff that gives the direction (male or female) for the older ID in dyad



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

# Ensure Date column is properly recognized as a Date type
BexFinal$Date <- as.Date(BexFinal$Date)
# Get the range (min and max) of the entire Date column
date_range <- range(BexFinal$Date)

dyad_summary <- BexFinal %>%
  group_by(Dyad) %>%
  summarize(
    Min_Date = min(Date),
    Max_Date = max(Date),
    Min_DyadDay = min(DyadDay),
    Max_DyadDay = max(DyadDay),
    .groups = "drop"
  )

# Display results
print(date_range) # Overall date range
print(dyad_summary) # Per-dyad summary of dates and DyadDays



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

cdplot(factor(Tol) ~  Season, data = BexFinal)

cdplot(factor(Tol) ~  TenureYears, data = BexFinal)

cdplot(factor(Tol) ~  HigherElo, data = BexFinal)



#ANAYLSIS
# ORDER SEASONS FOR STATS
xtabs(~ Tol+ IzELO, data= BexFinal)
xtabs(~ Tol+ Dyad, data= BexFinal)

# MOD 0 + MOD 1
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
# ANOVA mod 1 & mod 0 : mod0 is better model, Season is better predictor than VervetSeason




# MOD 2

# Varaibles included now
# IzDSI = Initial DSI (at beggning of the experiment in dyad)
# IzELO = Quartile difference from male/female rank respectively to their sex and group at beginning of the experiment
# AgeDiff = Age difference between M/F in dyad
# AgeDir = Which ID is older between M/F in dyad
# Season = natural easons in south africa
# TenureYears = years since male is in the group
# Dyad = Dyad being tester
# BirthGp = BirthGP of the ID
# TotalTrials = Total amount of trials per dyad
# HigherElo = Sex of ID with higher Elo quartile respectively to it's group and sex
# BB = Says if BB is born or not

# TO add
# Control experiment for amount of trials done as we can expect thhat mroe trials = more tolerance 
# and more trials P/day = more tolerance

# DAT2 &  MOD2

dat2 <- aggregate(Tol~ IzDSI + IzELO + AgeDiff + AgeDir + Season + TenureYears + Dyad + BirthGp + TotalTrials + HigherElo + BB, sum, data = BexFinal)
mod2 <- glmer(cbind(Tol, TotalTrials - Tol) ~  IzDSI + IzELO + AgeDiff + AgeDir + Season + TenureYears + HigherElo + BB + (1|Dyad) , binomial, data = dat2)
summary(mod2)
Anova(mod2)


plot(effect( "IzELO", mod2), type="response")
plot(effect( "AgeDiff", mod2))
plot(effect( "IzDSI", mod2))
plot(effect( "Season", mod2))
plot(effect( "TenureYears", mod2))
plot(effect( "AgeDiff:AgeDir", mod2))
plot(effect( "AgeDiff:AgeDir", mod2))
plot(effect( "AgeDir", mod2))
plot(effect( "HigherElo", mod2))
plot(effect( "BB", mod2))




#  ASSUMPTION CHECK FOR MOD 0,1,2

# GMLER ASSUMPTIONS
install.packages("DHARMa")
update.packages(ask=F)
library(DHARMa)

# MOD 1
# Model assumptions:
simulationOutput <- simulateResiduals(fittedModel = mod0, plot = F)
# 1) Overdispersion. no need for binomial models
testDispersion(mod0)
# 2) Plot, looking for HOMOSCEDASTICITY and outliers
plot(simulationOutput)

# MOD 2
# Model assumptions:
simulationOutput <- simulateResiduals(fittedModel = mod2, plot = F)
# 1) Overdispersion. no need for binomial models
testDispersion(mod2)
# 2) Plot, looking for HOMOSCEDASTICITY and outliers
plot(simulationOutput)




# Variables to add
# Seasonality > OK
# Male tenure > OK
# Pregnancy > OK

# CONTROL
# season for amount of trials , use trialday maybe?








#TASKS
# CHECK BOUNDARY SINGULAR FIT / CANT ESTIMATE VARIANCE OF DYAD( RANDOM EFFECT) : CHECK HOW /WHAT TO SIMPLIFY
# ChecK the evolution of the proportion of tolerance per Dyad with tolerance rounded by the week?
# Round dates to the nearest week?
