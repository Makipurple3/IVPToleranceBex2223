rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(lmerTest)
library(effects)
library(car)
library(lattice)
require(blme)
library(RVAideMemoire)
library(DHARMa)
library(MASS)
library(lme4)
library(emmeans)


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
BexFinal$Season <- factor(BexFinal$Season, levels = c("Winter", "Spring",  "Summer", "Autumn"), ordered = F)
# Change format Season & Vevert Season
BexFinal$VervetSeason <- factor(BexFinal$VervetSeason, levels = c("Summer","Mating", "Winter","Baby"), ordered = F)
# Ensure Date column is properly recognized as a Date type
BexFinal$Date <- as.Date(BexFinal$Date)

BexFinal$BB <- as.factor(BexFinal$BB)
BexFinal$AgeDir <- as.factor(BexFinal$AgeDir)
#BexFinal$IzELO <- as.factor(BexFinal$IzELO) # IzELO is NOT a factor! It should be numeric: the difference increases when it goes from 1 to 2. 
# If you make it a factor, it sees 1 as a seperate group (like NH for example) from 2 (as if it was BD), but they're not, it's increasing
BexFinal$Dyad <- as.factor(BexFinal$Dyad)
BexFinal$Date <- as.Date(BexFinal$Date) # You changed this into a factor, don't do that!!!

## Evolution of tolerance over time ####
# Make AgeDiff pos if male is older, neg if Fem is older
# Make IzELO pos if male is higher, neg if fem is higher
BexFinal <- BexFinal %>% 
  mutate(IzELO = as.numeric(IzELO),
         AgeDiffDir = case_when(
           AgeDir == "Male Older" ~ AgeDiff,
           AgeDir == "Female Older" ~ -AgeDiff,
           TRUE ~ NA
         ),
         IzELODir = case_when(
           as.factor(HigherElo) == "Male" ~ IzELO,
           as.factor(HigherElo) == "Female" ~ -IzELO,
           TRUE ~ NA
         )) # To include the direction of age difference and elo difference (male higher or female higher)
# These have no effect on the evolution of tolerance though (Tol ~ DyadDay), so I've left them out.
# Because these don't have an effect, I'm not including the interaction between AgeDiff and AgeDir (same thing as AgeDiffDir)
# or the interaction between IzElo and HigherElo.

# We have to rescale variables because otherwise the model doesn't converge. Doesn't change anything, 
# just makes your continuous variables around 0 (-1 to 1 for example)
BexFinal$IzDSI <- scale(BexFinal$IzDSI)
BexFinal$AgeDiff <- scale(BexFinal$AgeDiff)
BexFinal$IzELO <- scale(BexFinal$IzELO)
BexFinal$AgeDiffDir <- scale(BexFinal$AgeDiffDir)
BexFinal$IzELODir <- scale(BexFinal$IzELODir)

?scale

BexFinal

m0 <- glmer(Tol ~ DyadDay * (IzDSI + IzELO + AgeDiff + AgeDir + HigherElo) + VervetSeason + BirthGp + BB +
              (1|Date), 
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
            family = binomial(link = "logit"),
            data = BexFinal)

# Why Season and not VervetSeason?

#you can follow your reasoning for the best fixed effects but with the random effect which is not subject to negotiation, 
#it must be (DyadDay|Dyad:Date) at least and if possible also with (1|Dyad) in addition. which corresponds to: 
#variability between dates in a dyad and variability between dyads.

# Kom extreme in Age?
summary(m0)
Anova(m0)
# signficnat resutls

# table sing

plot(allEffects(m0))
plot(effect("DyadDay", m0)) # Tolerance increases in significantly over time (i.e., dyads learn to tolerate each other, great!)
plot(effect("IzELO", m0)) # Tolerance decreases with an increasing difference in ELO
plot(effect("DyadDay:IzELO", m0))
plot(effect("AgeDiff", m0)) # Tolerance is higher when individuals are closer in age (marginally non-significant)
plot(effect("VervetSeason", m0)) # Tolerance changes over the seasons, being highest in winter and lowest in summer
plot(effect("BirthGp", m0)) # Tolerance is different between groups, with highest tolerance in NH and lowest in AK (be aware, it's not *birth* group but just group. The variable is called birth group but it's the same)
plot(effect("DyadDay:IzDSI", m0)) # Tolerance increases over time, but mainly for dyads with the lowest startig DSI
# When DSI is already high, tolerance decreases (maybe because it just couldn't increase more? Interesting to see the raw data!)
plot(effect("BB", m0)) # Tolerance increases over time, especially when females have a baby
# Elo, AgeDir, HigherElo  and BB present have no effect on tolerance.
# AgeDiff, Elo, AgeDir and HigherElo have no effect on the increase of tolerance over time.
# No different effects of AgeDiff or ELO over different seasons.

test(emtrends(m0, pairwise ~ IzDSI, var="DyadDay")) # If you want to describe different slopes here, you have to 
# categorize IzDSI. I don't think it's necessary, you can just describe what you see (increase in tolerance is highest when initial DSI is lowest)
test(emtrends(m0, pairwise ~ BB, var="DyadDay")) # Significant increase of tolerance for females with baby, not for females without baby


# Model assumptions:
simulationOutput <- simulateResiduals(fittedModel = m0, plot = F)
# 1) Overdispersion. no need for binomial models
testDispersion(m0)
#The dispersion test ensures there is no overdispersion in the model. A p-value > 0.05 confirms the model is appropriately specified.
# p value of 0.096

# Residual Diagnostics
# 2) Plot, looking for HOMOSCEDASTICITY and outliers
plot(simulationOutput) # Good enough



# Variance Inflation Factor (VIF)
vif(m0)
#All VIF values are below 5, indicating no multicollinearity among predictors.
#If any VIF were > 10, the associated variable would need to be reconsidered.!
# Highest VIF is at 8, for Interaction DyadDay:BB





# MAIN EFFECTS: ELO, Vervet Season then AgeDif, AdeDir, BB, DyadDay * DSI (3)
#Dharma, VIF; post Hoc(low, mid, high dsi), Elo (check emailg graphs Josie)

# If plot, categoriz my DSI values (when raw) 

# PLOTTING TIMEEEEE

library(dplyr)
library(ggplot2)

# Use BexFinal and correctly name the variables
BexFinal <- BexFinal %>%
  mutate(
    # Categorize Elo Difference
    EloDiffCat = case_when(
      IzELO <= quantile(IzELO, 0.33, na.rm = TRUE) ~ "Elo1",
      IzELO > quantile(IzELO, 0.33, na.rm = TRUE) & IzELO <= quantile(IzELO, 0.66, na.rm = TRUE) ~ "Elo2",
      IzELO > quantile(IzELO, 0.66, na.rm = TRUE) ~ "Elo3"
    ),
    
    # Categorize Age Difference
    AgeDiffCat = case_when(
      AgeDiff <= 2 ~ "Small",
      AgeDiff > 2 & AgeDiff <= 5 ~ "Mid",
      AgeDiff > 5 ~ "High"
    ),
    
    # Categorize IzDSI
    IzDSICat = case_when(
      IzDSI <= quantile(IzDSI, 0.33, na.rm = TRUE) ~ "Low",
      IzDSI > quantile(IzDSI, 0.33, na.rm = TRUE) & IzDSI <= quantile(IzDSI, 0.66, na.rm = TRUE) ~ "Mid",
      IzDSI > quantile(IzDSI, 0.66, na.rm = TRUE) ~ "High"
    )
  )

#NDAY SPECIFIC PLOT


# DYAD 

# Plot 3: Dyad-Specific Trends by DyadDay
ggplot(BexFinal, aes(x = DyadDay, y = Tol, color = Dyad)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Dyad-Specific Tolerance by DyadDay",
    x = "Days of experiment",
    y = "Proportion of Tolerance"
  ) +
  theme_minimal()

# Plot 4: Dyad-Specific Trends by Date
ggplot(BexFinal, aes(x = Date, y = Tol, color = Dyad)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Dyad-Specific Tolerance by Date",
    x = "Date",
    y = "Proportion of Tolerance"
  ) +
  theme_minimal()













library(ggplot2)
library(dplyr)

# Example: Creating Vervet Ecological Season and Month in Season directly from Date
BexFinal <- BexFinal %>%
  mutate(
    VervetSeason = factor(
      case_when(
        month(Date) %in% c(1, 2, 3) ~ "Summer (Jan-Mar)",
        month(Date) %in% c(4, 5, 6) ~ "Mating Season (Apr-Jun)",
        month(Date) %in% c(7, 8, 9) ~ "Winter (Jul-Sep)",
        month(Date) %in% c(10, 11, 12) ~ "Birth Period (Oct-Dec)",
        TRUE ~ NA_character_
      ),
      levels = c("Summer (Jan-Mar)", "Mating Season (Apr-Jun)", "Winter (Jul-Sep)", "Birth Period (Oct-Dec)")
    ),
    MonthInSeason = case_when(
      month(Date) %in% c(1, 4, 7, 10) ~ 1,
      month(Date) %in% c(2, 5, 8, 11) ~ 2,
      month(Date) %in% c(3, 6, 9, 12) ~ 3
    )
  )

# Plot directly using linear model (lm)
ggplot(BexFinal, aes(x = MonthInSeason, y = Tol, color = VervetSeason)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  labs(
    title = "Tolerance Levels per Month within Vervet Ecological Seasons",
    x = "Month in Season (1 = Start, 3 = End)",
    y = "Tolerance Levels",
    color = "Vervet Ecological Season"
  ) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("Month 1", "Month 2", "Month 3")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )














library(ggplot2)
library(dplyr)

# Categorize scaled Elo differences (IzELO)
BexFinal <- BexFinal %>%
  mutate(
    EloDiffCat = case_when(
      IzELO <= quantile(IzELO, 0.33, na.rm = TRUE) ~ "Small",
      IzELO > quantile(IzELO, 0.33, na.rm = TRUE) & IzELO <= quantile(IzELO, 0.66, na.rm = TRUE) ~ "Average",
      IzELO > quantile(IzELO, 0.66, na.rm = TRUE) ~ "Big"
    )
  )

# Plot 1: Scaled Elo Differences by DyadDay
ggplot(BexFinal, aes(x = DyadDay, y = IzELO, color = EloDiffCat)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  labs(
    title = "Evolution of Scaled Elo Differences by DyadDay",
    x = "Dyad Day",
    y = "Scaled Elo Difference",
    color = "Elo Difference Category"
  ) +
  theme_minimal()

# Plot 2: Scaled Elo Differences by Date
ggplot(BexFinal, aes(x = Date, y = IzELO, color = EloDiffCat)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  labs(
    title = "Evolution of Scaled Elo Differences by Date",
    x = "Date",
    y = "Scaled Elo Difference",
    color = "Elo Difference Category"
  ) +
  theme_minimal()








library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)
library(scales)  # For date formatting

# Categorize Elo differences with updated labels
BexFinal <- BexFinal %>%
  mutate(
    EloDiffCat = case_when(
      IzELO <= quantile(IzELO, 0.33, na.rm = TRUE) ~ "Low (Q1)",
      IzELO > quantile(IzELO, 0.33, na.rm = TRUE) & IzELO <= quantile(IzELO, 0.66, na.rm = TRUE) ~ "Mid (Q2)",
      IzELO > quantile(IzELO, 0.66, na.rm = TRUE) ~ "High (Q3)"
    )
  )

# Set locale to English for date labels
Sys.setlocale("LC_TIME", "C")

# Plot with improved readability and additional details
ggplot(BexFinal, aes(x = Date, y = Tol, color = EloDiffCat, group = EloDiffCat)) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5) +  # Add confidence intervals
  labs(
    title = "Effect of Difference in Rank on Tolerance Over Time",
    subtitle = "Trends across Elo categories with confidence intervals",
    x = "Date (Months)",
    y = "Tolerance Probability",
    color = "Rank Difference"
  ) +
  scale_x_date(
    date_labels = "%b %Y", 
    date_breaks = "3 months"
  ) +  # Adjust x-axis labels
  theme_minimal(base_family = "serif") +  # Use academic serif font
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),  # Larger, centered title
    plot.subtitle = element_text(size = 16, hjust = 0.5),  # Subtitle for context
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "right",  # Legend on the right
    panel.grid.major = element_line(size = 0.5),  # Enhance grid visibility
    panel.grid.minor = element_blank()
  ) 
