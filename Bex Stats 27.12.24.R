
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



tri <- stepAIC(glm(Tol ~ (AgeDiff + AgeDir + IzELO + VervetSeason)^2, 
                   family = binomial, data = BexFinal))
  
  # BEST MODEL WITH AIC 1
  #Tol ~ AgeDiff + AgeDir + IzELO + VervetSeason + AgeDiff:IzELO + AgeDiff:VervetSeason
  


tri <- stepAIC(glm(Tol ~ (AgeDiff + AgeDir + BB + TenureYears + IzELO + IzDSI+HigherElo+
                            VervetSeason + BirthGp), binomial, data= BexFinal)) 
  # BEST MODEL WITH AIC 2
  # Tol ~ AgeDiff + AgeDir + TenureYears + IzELO + IzDSI + HigherElo + VervetSeason


tri <- stepAIC(glm(Tol ~ (AgeDiff + AgeDir + BB + TenureYears + IzELO + IzDSI+HigherElo+
                            VervetSeason + BirthGp)^2, binomial, data= BexFinal)) 
  # BEST MODEL WITH AIC 3
  # Tol ~ AgeDiff + AgeDir + TenureYears + IzELO + IzDSI + HigherElo + VervetSeason



#Impact of Variables and Interactions:
  
  #Significant Variables: AgeDiff, AgeDir, IzELO, and VervetSeason consistently remain in the final models. This suggests their importance in explaining Tol.
  #Non-Significant Variables:
  #BB was consistently removed, indicating it doesn't significantly explain variation.
  #HigherElo and IzDSI are retained in some runs, but their contributions are marginal.




# 1 MODEL BIN1 :
  #ELO, DSI AGEDIFF
model.bin1 <- glmer(Tol ~ IzELO  +  AgeDiff + IzDSI
                  +(1|Dyad) + (1|Date), binomial, data= BexFinal)  # add here all 
  summary(model.bin1)
  Anova(model.bin1)

# DHARMA BIN1
# Model assumptions:
simulationOutput <- simulateResiduals(fittedModel = model.bin1, plot = F)
# 1) Overdispersion. no need for binomial models
testDispersion(model.bin1)
# 2) Plot, looking for HOMOSCEDASTICITY and outliers
plot(simulationOutput)



# 2 MODEL BIN2 :
  # HIGHER ELO + AGE DIR
  # KEEP ELO, AGEDIFF
  # REMOVE DSI 

  # MODEL BIN2 :
model.bin2 <- glmer(Tol ~ IzELO  +  AgeDiff + AgeDir + HigherElo
                    +(1|Dyad) + (1|Date), binomial, data= BexFinal) 
  summary(model.bin2)
  Anova(model.bin2)

  # MODEL BIN2B :  
model.bin2B <- glmer(Tol ~ IzELO*HigherElo+ AgeDiff*AgeDir +
                      +(1|Dyad) + (1|Date), binomial, data= BexFinal) 
  summary(model.bin2)
  Anova(model.bin2)
  
  # MODEL BIN2C
model.bin2c <- glmer(Tol ~ IzELO + HigherElo+ AgeDiff + AgeDir +
                         +(1|Dyad) , binomial, data= BexFinal) 
  summary(model.bin2c)
  Anova(model.bin2c)
  
  # No BIG difference with in MODEL BIN2, MODELBIN2B, MODELBIN2C
  # IzElo significant
  
  # DHARMA BIN2
  # Model assumptions:
  simulationOutput <- simulateResiduals(fittedModel = model.bin2, plot = F)
  # 1) Overdispersion. no need for binomial models
  testDispersion(model.bin2)
  # 2) Plot, looking for HOMOSCEDASTICITY and outliers
  plot(simulationOutput)
  

  

  

# 3 MODEL BIN3:
  # KEEP IZELO
  # TEST WITH OR WITHOUT AGEDIR
  # ADD SEASON, VSEASON, TOTALTRIALS + BIRTHGP
  
  model.bin3 <- glmer(Tol ~ IzELO + AgeDiff + Season + VervetSeason + BirthGp 
  +(1|Dyad) + (1|Date), binomial, data= BexFinal) 
summary(model.bin3)
Anova(model.bin3)

#Dharma check
# MOD 3
# Model assumptions:
simulationOutput <- simulateResiduals(fittedModel = model.bin3, plot = F)
# 1) Overdispersion. no need for binomial models
testDispersion(model.bin3)
# 2) Plot, looking for HOMOSCEDASTICITY and outliers
plot(simulationOutput)
 
  # Final model with significant

    # Model check
    qqnorm(resid(model.bin))  # outliers
    plot(model.bin) # homogeneity
    bwplot(resid(model.bin) ~ VervetSeason, data=BexFinal) # homogeneity
    xyplot(resid(model.bin) ~ IzDSI, type=c("p", "smooth"), data=BexFinal) # homogeneity
    qqmath(ranef(model.bin))  # normality
    simulateResiduals(fittedModel = model.bin, plot = T)
    
    #Dharma check
    # MOD 1
    # Model assumptions:
    simulationOutput <- simulateResiduals(fittedModel = model.bin, plot = F)
    # 1) Overdispersion. no need for binomial models
    testDispersion(model.bin)
    # 2) Plot, looking for HOMOSCEDASTICITY and outliers
    plot(simulationOutput)
    
    
    
    model.bin3 <- glmer(Tol ~ IzELO + AgeDiff + Season + VervetSeason + BirthGp 
                        +(1|Dyad) + (1|Date), binomial, data= BexFinal) 
    
    
    
# Season vs Vervet Season: check which predictor to keep
    
    # Model with VervetSeason
    model.vervet <- glmer(
      Tol ~ IzELO + AgeDiff + AgeDir + VervetSeason + (1|Dyad) + (1|Date),
      binomial,
      data = BexFinal
    )
    
    # Model with Season
    model.season <- glmer(
      Tol ~ IzELO + AgeDiff + AgeDir + Season + (1|Dyad) + (1|Date),
      binomial,
      data = BexFinal
    )
    
    # Compare AIC
    AIC(model.vervet, model.season)
    # Lower AIC for model.vervet indicates VervetSeason is better
    
# 4 MODEL 4:
  # Adding Gender-Specific Interaction Terms to Model
    # Interactions tested are Agedir*HigherElo + AgeDir*VervetSeason
    model.bin4 <- glmer(
      Tol ~ IzELO + AgeDiff + AgeDir + HigherElo + VervetSeason + BirthGp +
        AgeDir:HigherElo + AgeDir:VervetSeason + (1|Dyad) + (1|Date),
      binomial,
      data = BexFinal
    )
    summary(model.bin4)
    Anova(model.bin4)
    
    
    

    #MOD 5
    model.bin5 <- glmer(Tol ~ IzELO + HigherElo + AgeDiff +AgeDir + VervetSeason + BirthGp 
                        +(1|Dyad) + (1|Date), binomial, data= BexFinal) 
    summary(model.bin5)
    Anova(model.bin5)
    
    
    

  
plot(effect("IzELO", model.bin), type="response")
plot(effect("AgeDiff", model.bin))
plot(effect("IzDSI", model.bin))
plot(effect("VervetSeason", model.bin))

      




# MOD TEST
  model.binvlast <- glmer(Tol ~ IzELO + VervetSeason + BirthGp + AgeDir:VervetSeason + 
                            TenureYears:VervetSeason + IzELO:VervetSeason + (1|Date), binomial, data= BexFinal) 
summary(model.binvlast)
Anova(model.binvlast)

# BIN 5


model.bin5 <- glmer(formula = Tol ~ AgeDir + TenureYears + IzELO + 
                          VervetSeason + BirthGp +  AgeDir:VervetSeason + 
                          TenureYears:VervetSeason + IzELO:VervetSeason
                        +(1|Dyad)+(TrialDay|Dyad:Date), 
                        control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                        binomial, data= BexFinal) 

summary(model.bin5)
Anova(model.bin5)








# BIN FINAL

# Final Model
final_model <- glmer(
  Tol ~ IzDSI:TenureYears + IzELO:AgeDir + VervetSeason + AgeDiff:AgeDir + 
    AgeDir:VervetSeason + TenureYears:VervetSeason + (1|Date),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
  family = binomial,
  data = BexFinal
)

# Summary of the final model
summary(final_model)
Anova(final_model)

# Analysis of Deviance (Type II Wald Chi-square test)
Anova(final_model, type = "II")



# Dispersion Check
simulationOutput <- simulateResiduals(fittedModel = final_model, plot = FALSE)
testDispersion(simulationOutput)
#The dispersion test ensures there is no overdispersion in the model. A p-value > 0.05 confirms the model is appropriately specified.


# Residual Diagnostics
plot(simulationOutput)
# Check for homoscedasticity (residuals vs predictors)
plotResiduals(simulationOutput, form = BexFinal$IzELO)
plotResiduals(simulationOutput, form = BexFinal$AgeDiff)
# Outlier check
testOutliers(simulationOutput)
# The residual plots confirm that residuals are evenly distributed (no patterns) and there are no significant outliers (p > 0.05).



# Variance Inflation Factor (VIF)
vif_model <- lm(
  Tol ~ IzELO + AgeDir + AgeDiff + VervetSeason + TenureYears,
  data = BexFinal
)
vif(vif_model)
#All VIF values are below 5, indicating no multicollinearity among predictors.
#If any VIF were > 10, the associated variable would need to be reconsidered.





# Null model without random effects for comparison
null_model <- glm(
  Tol ~ IzDSI:TenureYears + IzELO:AgeDir + VervetSeason + AgeDiff:AgeDir + 
    AgeDir:VervetSeason + TenureYears:VervetSeason,
  family = binomial,
  data = BexFinal
)

# Likelihood ratio test for random effect of Date
anova(final_model, null_model, test = "Chisq")






# Check random effect correlations
rePCA_result <- rePCA(final_model)
print(rePCA_result)
plot(rePCA_result)






# Calculate R² for fixed and random effects
library(MuMIn)
r.squaredGLMM(final_model)




# Visualizing interactions
library(effects)
plot(effect("IzDSI:TenureYears", final_model))
plot(effect("TenureYears:VervetSeason", final_model))
plot(effect("AgeDir:VervetSeason", final_model), x.var = "AgeDir")





# post hocs:
# Post-hoc analyses for interaction effects
library(emmeans)
emtrends(final_model, "VervetSeason", var = "TenureYears")
pairs(emtrends(final_model, "VervetSeason", var = "TenureYears"))

# Pairwise comparisons for IzELO across VervetSeason
pairs(emmeans(final_model, "IzELO", by = "VervetSeason"))











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



#