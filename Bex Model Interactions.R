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
# Post-hoc analyses for interaction effects
library(emmeans)
emtrends(final.model, "VervetSeason", var = "AgeDiff")
pairs(emtrends(final.model, "VervetSeason", var = "AgeDiff"))


emtrends(final.model, "IzELO", var = "AgeDiff")
pairs(emtrends(final.model, "IzELO", var = "AgeDiff"))



# Pairwise comparisons for IzELO across VervetSeason
pairs(emmeans(final.model, "AgeDiff", by = "IzELO"))


