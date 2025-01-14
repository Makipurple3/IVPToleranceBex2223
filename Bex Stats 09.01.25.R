
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


# DESCRIPTION OF VARIABLES

# IzDSI = Initial DSI (at beggning of the experiment in dyad)
# IzELO = Quartile difference from male/female rank respectively to their sex and group at beginning of the experiment
# AgeDiff = Age difference between M/F in dyad
# AgeDir = Which ID is older between M/F in dyad
# Season = natural easons in south africa
# TenureYears = years since male is in the group
# Dyad = Dyad being tester
# TotalTrials = Total amount of trials per dyad
# HigherElo = Sex of ID with higher Elo quartile respectively to it's group and sex
# BB = Says if BB is born or not




# CD PLOTS : CHECK TENDICES FOR VARIABLES
BexFinal=as.data.frame(BexFinal)

cdplot(factor(Tol) ~  IzDSI, data = BexFinal)
cdplot(factor(Tol) ~  TotalTrials, data = BexFinal)
cdplot(factor(Tol) ~  IzELO, data = BexFinal)
cdplot(factor(Tol) ~  AgeDiff, data = BexFinal)
cdplot(factor(Tol) ~  Season, data = BexFinal)
cdplot(factor(Tol) ~  VervetSeason, data = BexFinal)
cdplot(factor(Tol) ~  TenureYears, data = BexFinal)

#cdplot(factor(Tol) ~  HigherElo, data = BexFinal)

xtabs(~ Tol+ IzELO, data= BexFinal)
xtabs(~ VervetSeason+IzELO, data= BexFinal)

BexFinal$BB <- as.factor(BexFinal$BB)
BexFinal$AgeDir <- as.factor(BexFinal$AgeDir)
BexFinal$IzELO <- as.factor(BexFinal$IzELO)
BexFinal$Dyad <- as.factor(BexFinal$Dyad)
BexFinal$Date <- as.factor(BexFinal$Date)

require(party)
plot(ctree(factor(Tol) ~  IzELO  + IzDSI +  AgeDiff + Season + VervetSeason + TotalTrials
           + AgeDir + BB, data = BexFinal))



# MODEL CHECK WITH AIC 
# CHECK WHICH MDOEL WOULD BE THE BEST FOR AIC, THIS WILL BE USED AS A PèOTENTIEL INDICATOR BUT COMBBINATIONS WILL BE TESTED INDIVIDUALLY
# Check model fit and interactions with AIC

#Logistic regression model for Tol using both main effects and all pairwise interactions of the specified predictors.
#Selects the best subset of these predictors (main effects and interactions) by optimizing AIC, ensuring the model is neither overfit nor underfit.


#MAIN PREDICTORS
tri1 <- stepAIC(glm(Tol ~ (AgeDiff + IzDSI + IzELO + VervetSeason)^2, 
                    family = binomial, data = BexFinal))
#Final model = Tol ~ AgeDiff + IzDSI + IzELO + VervetSeason + AgeDiff:IzDSI + AgeDiff:VervetSeason + IzDSI:VervetSeason

#MAIN PREDICTORS WTH SEASON (INSTEAD OF V.SEASON)
tri1season <- stepAIC(glm(Tol ~ (AgeDiff + IzDSI + IzELO + Season)^2, 
                          family = binomial, data = BexFinal))

#MAIN + SECONDARY PREDICTORS
tri2 <- stepAIC(glm(Tol ~ (AgeDiff + AgeDir + IzELO + HigherElo + VervetSeason)^2,
                    family = binomial, data = BexFinal))

#MAIN + ALL PREDICTORS
tri3 <- stepAIC(glm(Tol ~ (AgeDiff + AgeDir + IzELO + HigherElo + VervetSeason + TenureYears + BB)^2, 
                    family = binomial, data = BexFinal))

#MODEULATED MODEL FROM PREVIOUS OUTPUTS
tri4 <- stepAIC(glm(Tol ~ (AgeDiff + IzELO + IzDSI + VervetSeason + AgeDiff:IzDSI + AgeDiff:VervetSeason + IzELO:VervetSeason)^2,
                    family = binomial, data = BexFinal))
#>BEST AIC 
#>
# Tests
tri5 <-stepAIC(glm(Tol ~  ( IzELO + VervetSeason  + AgeDir + HigherElo+ AgeDiff:IzELO + IzELO:VervetSeason + AgeDiff:VervetSeason +AgeDiff:IzELO:VervetSeason)^2,
                   family = binomial, data = BexFinal))

#Impact of Variables and Interactions:

#Significant Variables: AgeDiff, IzELO, IzDSI and VervetSeason consistently remain in the final models. This suggests their importance in explaining Tol.
#Non-Significant Variables: BB, Male Tenure, and variations for AgeDir and HigherElo
#BB was consistently removed, indicating it doesn't significantly explain variation.
#HigherElo and IzDSI are retained in some runs, but their contributions are marginal.

# Season vs Vervet Season: check which predictor to keep

# Model with VervetSeason
model.vervet <- glmer(
  Tol ~ IzELO + AgeDiff + IzDSI + VervetSeason + (1|Dyad) + (TrialDay|Dyad:Date),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
  family = binomial,
  data = BexFinal
)


# Model with Season
model.season <- glmer(
  Tol ~ IzELO + AgeDiff + IzDSI + Season + (1|Dyad) + (TrialDay|Dyad:Date),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
  family = binomial,
  data = BexFinal
)
AIC(model.vervet, model.season)
anova(model.vervet, model.season)


#MAIN PRED
model.mainpred <- glmer(Tol ~ IzELO  + AgeDiff + IzDSI
                        +(1|Dyad) + (TrialDay|Dyad:Date), 
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                        family = binomial,
                        data = BexFinal
)

#X
# MAIN PREDICTORS + SECONDARY PREDICTORS
model.2ndpred <- glmer(Tol ~ IzELO  +  AgeDiff + AgeDir + HigherElo + IzDSI
                       + (TrialDay|Dyad:Date),  
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                       family = binomial,
                       data = BexFinal
)


#X
# MAIN PREDICTORS + SECONDARY PREDICTORS + INTERACTIONS
# MODEL BIN2B :  
model.bin2B<- glmer(Tol ~ IzELO*HigherElo+ AgeDiff*AgeDir + IzDSI
                      + (TrialDay|Dyad:Date), 
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                     family = binomial,
                     data = BexFinal
)


# 3 MODEL 3:
# Adding Gender-Specific Interaction Terms to Model
# Interactions tested are Agedir*HigherElo + AgeDir*VervetSeason
model.bin3 <- glmer(
  Tol ~ IzELO + AgeDiff + AgeDir + HigherElo + VervetSeason + 
    AgeDir:HigherElo + AgeDir:VervetSeason + (TrialDay|Dyad:Date),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
  family = binomial,
  data = BexFinal
)




# REMOVAL OF AGEDIR AND ADDITION OF BIRTHGROUP
#MOD 4
model.bin4 <- glmer(Tol ~ IzELO + HigherElo + AgeDiff + VervetSeason
                    +(1|Dyad) + (TrialDay|Dyad:Date), 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    family = binomial,
                    data = BexFinal
)


# COMBINED MODEL WITH INTERACTIONS
# MOD 5 
model.bin5 <- glmer(Tol ~  AgeDiff + IzELO + IzDSI + VervetSeason + AgeDiff:VervetSeason 
                    + (1|Dyad) + (TrialDay|Dyad:Date), 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    family = binomial,
                    data = BexFinal
)



# MOD 6
model.bin6 <- glmer(Tol ~  AgeDiff + IzELO  + VervetSeason + AgeDiff:VervetSeason 
                    + (1|Dyad)+ (TrialDay|Dyad:Date), 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    family = binomial,
                    data = BexFinal
)



# Model 7
# Bin 7
model.bin7 <- glmer(Tol ~ AgeDiff + IzELO + VervetSeason + AgeDiff:IzELO + AgeDiff:VervetSeason + 
                      IzELO:VervetSeason 
                     + (TrialDay|Dyad:Date), 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    family = binomial,
                    data = BexFinal
)


# Model Bin 8, 

# ADDITION OF INTERACTIONS OF INTEREST
# AgeDiff:VervetSeason  

model.bin8 <- glmer(Tol ~  AgeDiff + IzELO  + IzDSI+  VervetSeason + AgeDiff:VervetSeason +IzDSI:TenureYears + TenureYears:VervetSeason + AgeDir:VervetSeason 
                    + (1|Dyad) + (TrialDay|Dyad:Date), 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    family = binomial,
                    data = BexFinal
)


# Model Bin 9, 
# REMOVAL OF 
# ADDITION OF INTERACTIONS OF INTEREST
#  

model.bin9 <- glmer(Tol ~  AgeDiff + IzELO + VervetSeason  + AgeDiff:IzELO:VervetSeason
                     + (TrialDay|Dyad:Date), 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    family = binomial,
                    data = BexFinal
)


#mod10
model.bin10 <- glmer(Tol ~ IzELO + VervetSeason + AgeDiff  + BB:VervetSeason + IzDSI:TenureYears  + (TrialDay|Dyad:Date), 
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                     family = binomial,
                     data = BexFinal
)



# mod11
model.bin11 <- glmer(Tol ~  AgeDiff + IzELO  + IzDSI + VervetSeason  +AgeDiff:VervetSeason +AgeDiff:IzELO
                   + (TrialDay|Dyad:Date), 
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                  family = binomial,
                  data = BexFinal
)

  
  # mode12
model.bin12 <- glmer(Tol ~  AgeDiff + IzELO + VervetSeason + AgeDiff:VervetSeason + AgeDiff:IzELO 
                      + (TrialDay|Dyad:Date), 
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                       family = binomial,
                       data = BexFinal

  )


##################

#  MODEL WITH DSI
# PREDICTORS: AGEDIFF, IZELO, VERVETSEASON, DSI
# INTERACTIONS: AGEDIFF: VERVET SEASON + AGEDIFF:VERVETSEASON + AGEDIFF:IZELO




model.finaldsi <- glmer(Tol ~  AgeDiff + IzELO  + IzDSI + VervetSeason + AgeDiff:VervetSeason + AgeDiff:IzELO
                         + (TrialDay|Dyad:Date), 
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                        family = binomial,
                        data = BexFinal
)



#########################


# FINAL & MODEL
# PREDICTORS: AGEDIFF, IZELO, VERVETSEASON
# INTERACTIONS:AGEDIFF:VERVETSEASON + AGEDIFF:IZELO


# final model need DSI to be removed

final.model <- glmer(Tol ~  AgeDiff + IzELO + VervetSeason + AgeDiff:VervetSeason + AgeDiff:IzELO 
                      + (TrialDay|Dyad:Date), 
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                     family = binomial,
                     data = BexFinal
)

summary(final.model)
Anova(final.model)
# ANOVA TYPE II
# Analysis of Deviance (Type II yxWald Chi-square test)
Anova(final.model, type = "II")




#EFFECT PLOTS
plot(effect("IzELO", final.model), type="response")
plot(effect("AgeDiff", final.model))
plot(effect("VervetSeason", final.model))

# Visualizing interactions
plot(effect("AgeDiff:VervetSeason", final.model))
plot(effect("AgeDiff:IzELO", final.model))


# Final model with significant

# Model check
qqnorm(resid(final.model))  # outliers
plot(final.model) # homogeneity
bwplot(resid(final.model) ~ VervetSeason, data=BexFinal) # homogeneity
xyplot(resid(final.model) ~ IzDSI, type=c("p", "smooth"), data=BexFinal) # homogeneity
qqmath(ranef(final.model))  # normality
simulateResiduals(fittedModel = final.model, plot = T)

#Dharma check
# MOD 1
# Model assumptions:
simulationOutput <- simulateResiduals(fittedModel = final.model, plot = F)
# 1) Overdispersion. no need for binomial models
testDispersion(final.model)
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
  Tol ~ AgeDiff + IzELO + VervetSeason + AgeDiff:VervetSeason + AgeDiff:IzELO,
  data = BexFinal
)
vif(vif_model)
#All VIF values are below 5, indicating no multicollinearity among predictors.
#If any VIF were > 10, the associated variable would need to be reconsidered.





# Null model without random effects for comparison
null_model <- glm(
  Tol ~ AgeDiff + IzELO + VervetSeason + AgeDiff:VervetSeason + AgeDiff:IzELO 
  , 
  family = binomial,
  data = BexFinal
)

# Likelihood ratio test for random effect of Date
anova(final.model, null_model, test = "Chisq")



# Calculate R² for fixed and random effects
library(MuMIn)
r.squaredGLMM(final.model)




# post hocs:
# post hocs for single effects
pairs(emmeans(final.model, ~ VervetSeason))

emmeans(final.model, ~ IzELO)
emmeans(final.model, ~ AgeDiff)



# Post-hoc analyses for interaction effects
library(emmeans)
emtrends(final.model, "VervetSeason", var = "AgeDiff")
pairs(emtrends(final.model, "VervetSeason", var = "AgeDiff"))


emtrends(final.model, "IzELO", var = "AgeDiff")
pairs(emtrends(final.model, "IzELO", var = "AgeDiff"))


# Pairwise comparisons for IzELO across VervetSeason
pairs(emmeans(final.model, "AgeDiff", by = "IzELO"))






#The results indicate that AgeDiff interacts significantly with VervetSeason and IzELO to influence tolerance, with differences across seasons and dominance ranks.




#BOX PLOT SEASON TOL Y X V SEAS, ONE P










    AIC(model.vervet, model.season, model.mainpred, model.2ndpred , model.bin2B, model.bin3, model.bin5, model.bin6, model.bin7, model.bin8, model.bin9, model.bin10, model.11, model.12, final.model, model.finaldsi)

anova(model.vervet, model.season, model.mainpred, model.2ndpred , model.bin2B, model.bin3, model.bin5, model.bin6, model.bin7, model.bin8, model.bin9, model.bin10, model.11, model.12, final.model, model.finaldsi)


# Correction in graphs
library(effects)

# Effect of IzELO (Quartile rank difference)
plot(
  effect("IzELO", final.model),
  type = "response",
  main = "Predicted effect of difference of rank on tolerance",
  ylab = "Tolerance",
  xlab = "Quartile rank difference"
)

# Effect of AgeDiff (Difference of age within the dyad)
plot(
  effect("AgeDiff", final.model),
  main = "Predicted effect of age difference on tolerance",
  ylab = "Tolerance",
  xlab = "Difference of age within the dyad"
)

# Effect of VervetSeason (Vervet Ecological Seasons)
plot(
  effect("VervetSeason", final.model),
  main = "Predicted effect of vervet ecological seasons on tolerance",
  ylab = "Tolerance",
  xlab = "Vervet Ecological Seasons"
)

# Interaction: AgeDiff and VervetSeason
plot(
  effect("AgeDiff:VervetSeason", final.model),
  main = "Predicted effect of interaction of age difference and vervet ecological seasons",
  ylab = "Tolerance",
  xlab = "Difference of age within the dyad"
)

# Interaction: AgeDiff and IzELO
plot(
  effect("AgeDiff:IzELO", final.model),
  main = "Predicted effect of interaction of age and rank difference",
  ylab = "Tolerance",
  xlab = "Difference of age within the dyad"
)









