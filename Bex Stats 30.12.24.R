
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








# RESEARCH QUESTION

  # Variations in Age, Rank, IDSI, and IEloDiff and time spent together 
  # (amount of trials on a set period/in general) can explain differences in tolerance 
  # (aggression/not approaching?!) rates in a forced proximity box experiment 
  # conducted in wild vervet monkeys
  # The effects may be moderated by /> Seasonality, Male tenure, Female pregnancy
  # Add Male/Female Higher for ELO , column "Higher" M or F diffelo*higher > see if effect of sex + rank on tolerance<
  # Add Male/Female higher for AGE > same idea as Elo

  #>Lower variations in age and rank and higher initial social bonds and overall amount of time spent together leads to higher rates of tolerance

  
  
  
# DESCRIPTION OF VARIABLES

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
BexFinal$BirthGp <- as.factor(BexFinal$BirthGp)
BexFinal$IzELO <- as.factor(BexFinal$IzELO)
BexFinal$Dyad <- as.factor(BexFinal$Dyad)
BexFinal$Date <- as.factor(BexFinal$Date)

require(party)
plot(ctree(factor(Tol) ~  IzELO  + IzDSI +  AgeDiff + Season + VervetSeason + TotalTrials
           + AgeDir + BB + BirthGp, data = BexFinal))



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
  Tol ~ IzELO + AgeDiff + IzDSI + VervetSeason + (1|Dyad) + (1|Date),
  binomial,
  data = BexFinal
)


# Model with Season
model.season <- glmer(
  Tol ~ IzELO + AgeDiff + IzDSI + Season + (1|Dyad) + (1|Date),
  binomial,
  data = BexFinal
)    
AIC(model.vervet, model.season)
anova(model.vervet, model.season)


# MAIN PREDICTORS 
# 1 MODEL BIN1 :
  #ELO, DSI AGEDIFF
model.mainpred <- glmer(Tol ~ IzELO  + AgeDiff + IzDSI
                  +(1|Dyad) + (1|Date), binomial, data= BexFinal)  # add here all 
  summary(model.mainpred)
  Anova(model.mainpred)

  # AIC 3058.4
  # VARIANCE RANDOM EFFECT. 
  # DATE = 0.29, DYAD = 0.02
  # SIGNIFICANT VARIABLES 
  # IZELO***, AGEDIFF*
  # NOT SIGNIFICANT 
  # IZDSI


# MAIN PREDICTORS + SECONDARY PREDICTORS
# 2 MODEL BIN2 :
  # ADDITION OF: HIGHER ELO + AGE DIR
  # KEEP ELO, AGEDIFF
  # REMOVE DSI 

  # MODEL BIN2 :
model.2ndpred <- glmer(Tol ~ IzELO  +  AgeDiff + AgeDir + HigherElo + IzDSI
                    +(1|Dyad) + (1|Date), binomial, data= BexFinal) 
  summary(model.2ndpred)
  Anova(model.2ndpred)
  
  # AIC 3060.5
  # VARIANCE RANDOM EFFECT. 
  # DATE = 0.29, DYAD = 0.14
  # SIGNIFICANT VARIABLES 
  # IZELO***, AGEDIFF*
  # NOT SIGNIFICANT 
  # IZDSI, AGEDIR, HIGHERELO

  
 # MAIN PREDICTORS + SECONDARY PREDICTORS + INTERACTIONS
  # MODEL BIN2B :  
model.bin2B <- glmer(Tol ~ IzELO*HigherElo+ AgeDiff*AgeDir + IzDSI
                      +(1|Dyad) + (1|Date), binomial, data= BexFinal) 
  summary(model.bin2B)
  Anova(model.bin2B)
  
  # AIC 3052.6
  # VARIANCE RANDOM EFFECT. 
  # DATE = 32.962e-01, DYAD = 2.202e-05.929e-10 1.712e-05
  # SIGNIFICANT VARIABLES 
  # IZELO***, AGEDIFF**, IzDSI **
  # HigherEloMale***, IzELo:HigherEloMale*** 
  # NOT SIGNIFICANT
  # AGEDIRMALE OLDER, AGEDIFF:AGEDIRMALE OLDER
  
  
  # N MODEL BIN2BC, IS BETTER MDOEL 
  # IzElo significant
  
  
    
# 3 MODEL 3:
  # Adding Gender-Specific Interaction Terms to Model
    # Interactions tested are Agedir*HigherElo + AgeDir*VervetSeason
    model.bin3 <- glmer(
      Tol ~ IzELO + AgeDiff + AgeDir + HigherElo + VervetSeason + 
        AgeDir:HigherElo + AgeDir:VervetSeason + (1|Dyad) + (1|Date),
      binomial,
      data = BexFinal
    )
    summary(model.bin3)
    Anova(model.bin3)
    
    
    # AIC 3022.6 (lowest for now) 
    # VARIANCE RANDOM EFFECT. 
    # DATE = 0.78, DYAD = 0.06
    # SIGNIFICANT VARIABLES 
    # IZELO***, VERVETSEASONS*** AGEDIFF**,
    # HigherEloMale***, IzELo:HigherEloMale*** 
    # NOT SIGNIFICANT
    # IZDSI, AGEDIR, HIGHERELO,AGEDIR:HIGHERELO, AGEDIR:VERVETSEASON

    
# REMOVAL OF AGEDIR AND ADDITION OF BIRTHGROUP
#MOD 4
    model.bin4 <- glmer(Tol ~ IzELO + HigherElo + AgeDiff + VervetSeason + BirthGp 
                        +(1|Dyad) + (1|Date), binomial, data= BexFinal) 
    summary(model.bin4)
    Anova(model.bin4)
    # AIC 3010.4 (lowest for now) 
    # VARIANCE RANDOM EFFECT. 
    # DATE = 6.933e-02, DYAD = 6.647e-08
    # SIGNIFICANT VARIABLES 
    # IZELO***, VERVETSEASONS*** AGEDIFF**, BIRTHGP
    #
    # NOT SIGNIFICANT
    # HIGHERELO
    
    

# COMBINED MODEL WITH INTERACTIONS
# MOD 5 
  model.bin5 <- glmer(Tol ~  AgeDiff + IzELO + IzDSI + VervetSeason + AgeDiff:VervetSeason + BirthGp
                             + (1|Date), 
                              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                               family = binomial,
                               data = BexFinal
  )


summary(model.bin5)
Anova(model.bin5)
# AIC 3004.8 (lowest for now) 
# VARIANCE RANDOM EFFECT. 
# DATE = 0.0896
# SIGNIFICANT VARIABLES 
# IZELO***, VERVETSEASONS*** AGEDIFF**, BIRTHGP**, AGEDIFF:VERVETSEASON
#
# NOT SIGNIFICANT
# IZDSI





# MOD 6
model.bin6 <- glmer(Tol ~  AgeDiff + IzELO  + VervetSeason + AgeDiff:VervetSeason + BirthGp
                    + (1|Date), 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    family = binomial,
                    data = BexFinal
)
summary(model.bin6)
Anova(model.bin6)
# AIC 3004.0 (lowest for now) 
# VARIANCE RANDOM EFFECT. 
# DATE = 0.0675 
# SIGNIFICANT VARIABLES 
# IZELO***, VERVETSEASONS*** AGEDIFF**, BIRTHGP**, AGEDIFF:VERVETSEASON*
#
# NOT SIGNIFICANT
#NONE


# Model 7
# Bin 7
model.bin7 <- glmer(Tol ~ AgeDiff + IzELO + VervetSeason + AgeDiff:IzELO + AgeDiff:VervetSeason + 
                      IzELO:VervetSeason 
                    + (1|Date), 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    family = binomial,
                    data = BexFinal
)

summary(model.bin7)
anova(model.bin7)



# Model Bin 8, 
    # REMOVAL OF BIRTHGP
    # ADDITION OF INTERACTIONS OF INTEREST
      # AgeDiff:VervetSeason  

model.bin8 <- glmer(Tol ~  AgeDiff + IzELO  + IzDSI+  VervetSeason + AgeDiff:VervetSeason +IzDSI:TenureYears + TenureYears:VervetSeason + AgeDir:VervetSeason 
                     + (1|Date), 
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                     family = binomial,
                     data = BexFinal
)
summary(model.bin8)
anova(model.bin8)

# AIC 3004.7 (lowest for now) 
# VARIANCE RANDOM EFFECT. 
# DATE = 0.06601 
# SIGNIFICANT VARIABLES 
# IZELO***, VERVETSEASONS***, BIRTHGP, AGEDIFF**
#  
# NONE



# Model Bin 9, 
# REMOVAL OF 
# ADDITION OF INTERACTIONS OF INTEREST
#  

model.bin9 <- glmer(Tol ~  AgeDiff + IzELO + VervetSeason  + AgeDiff:IzELO:VervetSeason
                    + (1|Date), 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    family = binomial,
                    data = BexFinal
)
summary(model.bin9)
anova(model.bin9)


# AIC
# VARIANCE RANDOM EFFECT. 
# DATE = 
# SIGNIFICANT VARIABLES 
#  


#mod10
model.bin10 <- glmer(Tol ~ IzELO + VervetSeason + AgeDiff + BirthGp  + BB:VervetSeason + IzDSI:TenureYears +(1|Date), 
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                     family = binomial,
                     data = BexFinal
)
summary(model.bin10)
anova(model.bin10)


# mod11
model.11 <- glmer(Tol ~  AgeDiff + IzELO  + IzDSI + VervetSeason + BirthGp +AgeDiff:VervetSeason +AgeDiff:IzELO
                        + (1|Date), 
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                        family = binomial,
                        data = BexFinal
)

summary(model.11)
anova(model.11)

##################

#  MODEL WITH DSI
  # PREDICTORS: AGEDIFF, IZELO, VERVETSEASON, DSI
  # INTERACTIONS: AGEDIFF: VERVET SEASON + AGEDIFF:VERVETSEASON + AGEDIFF:IZELO




model.finaldsi <- glmer(Tol ~  AgeDiff + IzELO  + IzDSI + VervetSeason + AgeDiff:VervetSeason + AgeDiff:IzELO
                     + (1|Date), 
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                     family = binomial,
                     data = BexFinal
)
summary(model.finaldsi)
Anova(model.finaldsi)

# AIC 3004.3
# VARIANCE RANDOM EFFECT
# DATE: 0.07117



#########################


# FINAL & MODEL
# PREDICTORS: AGEDIFF, IZELO, VERVETSEASON
# INTERACTIONS:AGEDIFF:VERVETSEASON + AGEDIFF:IZELO


# final model need DSI to be removed

final.model <- glmer(Tol ~  AgeDiff + IzELO + VervetSeason + AgeDiff:VervetSeason + AgeDiff:IzELO 
                     + (1|Date), 
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                     family = binomial,
                     data = BexFinal
)

summary(final.model)
Anova(final.model)
# AIC 3004.0 (lowest for now) 
# VARIANCE RANDOM EFFECT. 
# DATE = 0.06912 
# SIGNIFICANT VARIABLES 
# IZELO***, VERVETSEASONS***, AGEDIFF**
# AGEDIFF:IZELO***, AGEDIFF:VERVETSEASON
# NONE



# ANOVA TYPE II
# Analysis of Deviance (Type II Wald Chi-square test)
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



Inter1 <- emtrends(final.model, "IzELO", var = "AgeDiff")
summary(Inter1)

# post hocs:
# Post-hoc analyses for interaction effects
library(emmeans)
emtrends(final.model, "VervetSeason", var = "AgeDiff")
pairs(emtrends(final.model, "VervetSeason", var = "AgeDiff"))


emtrends(final.model, "IzELO", var = "AgeDiff")
pairs(emtrends(final.model, "IzELO", var = "AgeDiff"))



# Pairwise comparisons for IzELO across VervetSeason
pairs(emmeans(final.model, "AgeDiff", by = "IzELO"))



 
#The results indicate that AgeDiff interacts significantly with VervetSeason and IzELO to influence tolerance, with differences across seasons and dominance ranks.




###### UPDATE RESULTS

#Significant Effects
#Main Effects:
  #IzDSI:TenureYears (p < 0.05): Longer tenures with higher dominance scores significantly increase tolerance.
  #VervetSeason (p < 0.001): Mating and winter seasons show higher tolerance compared to summer and baby seasons.
  #IzELO:AgeDir (p < 0.001): Rank effects (IzELO) interact with age differences to influence tolerance.
#Interactions:
  #TenureYears:VervetSeason (p < 0.001): Seasonal changes moderate the effect of tenure on tolerance.
  #Non-Significant Effects
  #AgeDirMale Older:VervetSeasonWinter and AgeDirMale Older:VervetSeasonBaby (p > 0.1): These could be removed to simplify the model, but they align structurally with other interactions.

  #AIC = 3003.3: Indicates a well-fitting model. Lower than alternative models, so this is a good choice.
  #Dispersion Test: No overdispersion (p > 0.05).
  #Residual Diagnostics: No issues with homoscedasticity, normality, or outliers.
  #Random Effects: The contribution of Date is small but helps account for temporal variability.Variance for Date is 0.07563, suggesting that while the random effect for Date is small, it slightly contributes to the model fit.

  #Median residuals are close to zero, and the distribution appears balanced, with no extreme outliers affecting the model's performance.




# AGE DIFF

# Calculate range
age_diff_range <- range(BexFinal$AgeDiff, na.rm = TRUE)

# Calculate mean
age_diff_mean <- mean(BexFinal$AgeDiff, na.rm = TRUE)

# Calculate standard deviation
age_diff_sd <- sd(BexFinal$AgeDiff, na.rm = TRUE)

# Display results
list(
  Range = age_diff_range,
  Mean = age_diff_mean,
  StandardDeviation = age_diff_sd
)


# AGE DIR
# Calculate range
table(BexFinal$Dyad, BexFinal$AgeDir)

#ELO

range(BexFinal$IzELO)
mean(BexFinal$IzELO)
sd(BexFinal$IzELO)

table(BexFinal$Dyad, BexFinal$IzELO)
table(BexFinal$Dyad, BexFinal$HigherElo) 


# DSI
range(BexFinal$IzDSI)
mean(BexFinal$IzDSI)
sd(BexFinal$IzDSI)

#TENURE YEARS
range(BexFinal$TenureYears)
mean(BexFinal$TenureYears)
sd(BexFinal$TenureYears)


#BB
table(BexFinal$Dyad,BexFinal$BB)
