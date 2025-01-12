
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




#  









model.interaction <- glmer(Tol ~  AgeDiff + IzELO + VervetSeason  + AgeDiff:IzELO:VervetSeason
                    + (1|Date), 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    family = binomial,
                    data = BexFinal
)
summary(model.interaction)
anova(model.interaction)


# ANOVA TYPE II
# Analysis of Deviance (Type II Wald Chi-square test)
Anova(model.interaction, type = "II")



#EFFECT PLOTS
plot(effect("IzELO", model.interaction), type="response")
plot(effect("AgeDiff", model.interaction))
plot(effect("VervetSeason", model.interaction))

# Visualizing interactions
plot(effect("AgeDiff:IzELO:VervetSeason", model.interaction))



# Final model with significant

# Model check
qqnorm(resid(model.interaction))  # outliers
plot(model.interaction) # homogeneity
bwplot(resid(model.interaction) ~ VervetSeason, data=BexFinal) # homogeneity
xyplot(resid(model.interaction) ~ IzDSI, type=c("p", "smooth"), data=BexFinal) # homogeneity
qqmath(ranef(model.interaction))  # normality
simulateResiduals(fittedModel = model.interaction, plot = T)

#Dharma check
# MOD 1
# Model assumptions:
simulationOutput <- simulateResiduals(fittedModel = model.interaction, plot = F)
# 1) Overdispersion. no need for binomial models
testDispersion(model.interaction)
#The dispersion test ensures there is no overdispersion in the model. A p-value > 0.05 confirms the model is appropriately specified.
# p value of 0.096

# Residual Diagnostics
# 2) Plot, looking for HOMOSCEDASTICITY and outliers
plot(simulationOutput)










# Check for homoscedasticity (residuals vs predictors)
plotResiduals(simulationOutput, form = BexFinal$IzELO)
plotResiduals(simulationOutput, form = BexFinal$AgeDiff)
plotResiduals(simulationOutput, form = BexFinal$VervetSeason) 
# Outlier check
testOutliers(simulationOutput)
# The residual plots confirm that residuals are evenly distributed (no patterns) and there are no significant outliers (p > 0.05).



# Variance Inflation Factor (VIF)
vif_model <- lm(
  Tol ~ AgeDiff + IzELO + VervetSeason + AgeDiff:IzELO:VervetSeason,
  data = BexFinal
)
vif(vif_model)
#All VIF values are below 5, indicating no multicollinearity among predictors.
#If any VIF were > 10, the associated variable would need to be reconsidered.





# Null model without random effects for comparison
null_model <- glm(
  Tol ~ AgeDiff + IzELO + VervetSeason +  AgeDiff:IzELO:VervetSeason 
  , 
  family = binomial,
  data = BexFinal
)

# Likelihood ratio test for random effect of Date
anova(model.interaction, null_model, test = "Chisq")



# Calculate R² for fixed and random effects
library(MuMIn)
r.squaredGLMM(model.interaction)




# post hocs:
# Post-hoc analyses for interaction effects
library(emmeans)
emtrends(model.interaction, "VervetSeason", var = "AgeDiff")
pairs(emtrends(model.interaction, "VervetSeason", var = "AgeDiff"))


emtrends(model.interaction, "IzELO", var = "AgeDiff")
pairs(emtrends(model.interaction, "IzELO", var = "AgeDiff"))



# Pairwise comparisons for IzELO across VervetSeason
pairs(emmeans(model.interaction, "AgeDiff", by = "IzELO"))


