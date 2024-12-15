# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(lmerTest)
library(effects)
library(car)


# OPEN BEXFINAL
BexFinal <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexFinal.csv")
colnames(BexFinal)

# Convert Date column to Date format
BexFinal <- BexFinal %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))


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
