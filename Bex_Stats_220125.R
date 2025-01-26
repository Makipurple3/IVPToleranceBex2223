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
BexFinal$Date <- as.Date(BexFinal$Date) 

# UPDATES AND CREATION OF COLUMNS IN BEXFINAL
#MAX TRIAL DAY
# Create max trial day to compute statistics on dyad day
# Add a new column with the maximum TrialDay per Dyad and Date
BexFinal <- BexFinal %>%
  group_by(Dyad, Date) %>%
  mutate(MaxTrialDay = max(TrialDay)) %>%
  ungroup()

# VERVET SEASONS
BexFinal$Date <- as.Date(BexFinal$Date, format = "%Y-%m-%d")

# Ass a vervet sweasons with amting season and bb season
BexFinal <- BexFinal %>%
  mutate(
    SeasonYear = case_when(
      month(Date) %in% c(1, 2, 3) ~ paste0("Summer-", year(Date)),
      month(Date) %in% c(4, 5, 6) ~ paste0("Mating-", year(Date)),
      month(Date) %in% c(7, 8, 9) ~ paste0("Winter-", year(Date)),
      month(Date) %in% c(10, 11, 12) ~ paste0("Baby-", year(Date)),
      TRUE ~ NA_character_
    )
  )


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




# Calculate and print date ranges for each VervetSeason
date_ranges <- BexFinal %>%
  group_by(VervetSeason) %>%
  summarize(
    StartDate = min(Date, na.rm = TRUE),
    EndDate = max(Date, na.rm = TRUE),
    .groups = "drop"
  )
print(date_ranges)


date_ranges <- BexFinal %>%
  group_by(SeasonYear) %>%
  summarize(
    StartDate = min(Date, na.rm = TRUE),
    EndDate = max(Date, na.rm = TRUE),
    .groups = "drop"
  )

print(date_ranges)




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


##################################################################################################################################
# NOT USE MODEL MX 


mx <- glmer(Tol ~ VervetSeason + DyadDay*( IzELO + AgeDiff:BB:IzDSI ) +
              (1|Date), 
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
            family = binomial(link = "logit"),
            data = BexFinal)

summary(mx)
Anova(mx)
vif(mx)
plot(allEffects(mx))
# Why Season and not VervetSeason?
##################################################################################################################################






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
s# Elo, AgeDir, HigherElo  and BB present have no effect on tolerance.
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

# Calculate overall mean tolerance
overall_mean <- BexFinal %>%
  summarize(MeanTol = mean(Tol, na.rm = TRUE)) %>%
  pull(MeanTol)

# Calculate and print date ranges for each VervetSeason
date_ranges <- BexFinal %>%
  group_by(VervetSeason) %>%
  summarize(
    StartDate = min(Date, na.rm = TRUE),
    EndDate = max(Date, na.rm = TRUE),
    .groups = "drop"
  )
print(date_ranges)








# Plot: Tolerance Levels per Month within Vervet Ecological Seasons
ggplot(BexFinal, aes(x = MonthInSeason, y = Tol, color = VervetSeason)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +  # Seasonal trends
  geom_hline(aes(yintercept = overall_mean, linetype = "Tolerance Mean"), color = "black", size = 1) +  # Mean line
  labs(
    title = "Tolerance Levels per Month within Vervet Ecological Seasons",
    x = "Month in Season (1 = Start, 3 = End)",
    y = "Tolerance Levels",
    color = "Vervet Ecological Season",
    linetype = NULL  # Remove linetype title
  ) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("Month 1", "Month 2", "Month 3")) +
  scale_linetype_manual(values = c("dashed")) +  # Dashed line in legend
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )



# Combined Plot with Distinct Markers for Each Season
# Plot: Continuous Evolution of Tolerance by Vervet Seasons
ggplot(BexFinal, aes(x = Date, y = Tol, color = VervetSeason, group = VervetSeason)) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +  # Trendlines with confidence intervals
  geom_hline(yintercept = overall_mean, linetype = "dashed", color = "black", size = 1.2, aes(linetype = "Tolerance Mean")) +  # Overall mean
  labs(
    title = "Continuous Evolution of Tolerance Levels by Vervet Ecological Seasons",
    x = "Date",
    y = "Tolerance Levels",
    color = "Vervet Ecological Season",
    linetype = "Legend"
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "3 months"
  ) +
  scale_linetype_manual(values = c("Tolerance Mean" = "dashed")) +  # Ensure dashed line in the legend
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )







# Calculate date ranges for each VervetSeason
date_ranges <- BexFinal %>%
  group_by(VervetSeason) %>%
  summarize(
    StartDate = min(Date, na.rm = TRUE),
    EndDate = max(Date, na.rm = TRUE),
    .groups = "drop"
  )

# Display the date ranges
print(date_ranges)
library(ggplot2)
library(dplyr)
library(lubridate)

# Add SeasonYear for clarity in winters across years
BexFinal <- BexFinal %>%
  mutate(
    SeasonYear = case_when(
      month(Date) %in% c(1, 2, 3) ~ paste0("Summer-", year(Date)),
      month(Date) %in% c(4, 5, 6) ~ paste0("Mating-", year(Date)),
      month(Date) %in% c(7, 8, 9) & year(Date) == 2022 ~ "Winter-2022",
      month(Date) %in% c(7, 8, 9) & year(Date) == 2023 ~ "Winter-2023",
      month(Date) %in% c(10, 11, 12) ~ paste0("Baby-", year(Date)),
      TRUE ~ NA_character_
    ),
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

# Calculate overall mean tolerance
overall_mean <- BexFinal %>%
  summarize(MeanTol = mean(Tol, na.rm = TRUE)) %>%
  pull(MeanTol)

# Calculate date ranges for each SeasonYear
date_ranges <- BexFinal %>%
  group_by(SeasonYear) %>%
  summarize(
    StartDate = min(Date, na.rm = TRUE),
    EndDate = max(Date, na.rm = TRUE),
    .groups = "drop"
  )

print(date_ranges)




# Calculate overall mean tolerance
overall_mean <- BexFinal %>%
  summarize(MeanTol = mean(Tol, na.rm = TRUE)) %>%
  pull(MeanTol)

# Plot: Tolerance Levels per Month within Vervet Ecological Seasons
ggplot(BexFinal, aes(x = MonthInSeason, y = Tol, color = VervetSeason)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +  # Seasonal trends
  geom_hline(aes(yintercept = overall_mean, linetype = "Tolerance Mean"), color = "black", size = 1) +  # Mean line
  labs(
    title = "Tolerance Levels per Month within Vervet Ecological Seasons",
    x = "Month in Season (1 = Start, 3 = End)",
    y = "Tolerance Levels",
    color = "Vervet Ecological Season",
    linetype = NULL  # Remove linetype title
  ) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("Month 1", "Month 2", "Month 3")) +
  scale_linetype_manual(values = c("dashed")) +  # Dashed line in legend
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )











# Update color palette for the seasons
season_highlights <- tibble(
  Season = c("Winter (2022)", "Birth Period", "Summer", "Mating", "Winter (2023)"),
  StartDate = as.Date(c("2022-09-01", "2022-10-01", "2023-01-01", "2023-04-01", "2023-07-01")),
  EndDate = as.Date(c("2022-09-30", "2022-12-31", "2023-03-31", "2023-06-30", "2023-09-30")),
  Color = c("#4f81bd", "#66cdaa", "#ffa500", "#ff69b4", "#9370db")  # New color palette
)

# Plot with new colors
ggplot(BexFinal, aes(x = Date, y = Tol)) +
  geom_rect(
    data = season_highlights,
    aes(
      xmin = StartDate,
      xmax = EndDate,
      ymin = -Inf,
      ymax = Inf,
      fill = Season
    ),
    inherit.aes = FALSE,
    alpha = 0.15,
    color = "white",
    linewidth = 0.5
  ) +
  geom_smooth(method = "loess", se = TRUE, color = "black", size = 1.5) +
  geom_hline(yintercept = overall_mean, linetype = "dashed", color = "red", size = 1.2) +
  labs(
    title = "Smoothed Overall Evolution of Tolerance Over Time",
    subtitle = "Shaded areas represent Vervet Ecological Seasons",
    x = "Date",
    y = "Mean Tolerance",
    fill = "Ecological Seasons"
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "2 months",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(values = season_highlights$Color) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position = "top"
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
  theme_minimal(base_family = "Arial") +  # Updated academic serif font
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),  # Larger, centered title
    plot.subtitle = element_text(size = 18, hjust = 0.5),  # Subtitle for context
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




# GRAPHS NON SIGNIFICANT


# AGE DIFFERENCE
plot1 <- ggplot(BexFinal, aes(x = AgeDiff, y = Tol)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Effect of Age Difference on Tolerance",
    x = "Age Difference",
    y = "Tolerance"
  ) +
  theme_minimal(base_family = "Arial")



# AGE DIRECTION
# Age Direction Effect Over Time
plot_age_dir <- ggplot(BexFinal, aes(x = Date, y = Tol, color = AgeDir, group = AgeDir)) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  labs(
    title = "Effect of Age Direction on Tolerance Over Time",
    x = "Date",
    y = "Tolerance",
    color = "Older Individual"
  ) +
  scale_color_manual(values = c("steelblue", "hotpink")) +  # Custom colors for M and F
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )




# DSI
ggplot(BexFinal, aes(x = DyadDay, y = Tol, color = IzDSI)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Interaction Between Experimental Day and Initial Social Bonds",
    x = "Experimental Day",
    y = "Tolerance",
    color = "Initial Social Bond (IzDSI)"
  ) +
  theme_minimal(base_family = "Arial")




# Baby Presence Effect Over Time
 ggplot(BexFinal, aes(x = Date, y = Tol, color = BB, group = BB)) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  labs(
    title = "Effect of Baby Presence on Tolerance Over Time",
    x = "Date",
    y = "Tolerance",
    color = "Baby Presence"
  ) +
  scale_color_manual(values = c("darkorange", "darkblue")) +  # Custom colors
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


# Combine plots into a grid layout
plot1 + plot2 + plot3 + plot4 +
  plot_layout(ncol = 2) & 
  theme(plot.title = element_text(size = 16, face = "bold"))


#TABELS

# TABLE RESULTS

library(car)
anova_table <- Anova(m0, type = "II")
print(anova_table)


random_effects <- as.data.frame(VarCorr(m0))
print(random_effects)


library(knitr)
kable(result_table, caption = "GLMM Fixed Effects Results")
kable(as.data.frame(anova_table), caption = "ANOVA Results for Fixed Effects")
kable(random_effects, caption = "Random Effects Variance Components")



library(emmeans)
posthoc_seasons <- emmeans(m0, pairwise ~ VervetSeason, adjust = "bonferroni")
summary(posthoc_seasons$contrasts)



library(lme4)
library(parameters)

# Extract model parameters
model_parameters <- parameters::model_parameters(m0)

# Convert to data frame and round to 3 decimals
result_table <- as.data.frame(model_parameters)
result_table <- result_table[, c("Parameter", "Coefficient", "SE", "z", "p")]
names(result_table) <- c("Predictor", "Estimate", "Std.Error", "z-value", "p-value")

# Round values to 3 decimal places
result_table <- transform(result_table,
                          Estimate = round(Estimate, 3),
                          Std.Error = round(Std.Error, 3),
                          `z-value` = round(`z-value`, 3),
                          `p-value` = round(`p-value`, 3))

# Display the table
result_table






# Load required libraries
library(lme4)
library(parameters)
library(emmeans)
library(kableExtra)
library(car)

# Extract model parameters (fixed effects)
model_parameters <- parameters::model_parameters(m0)

# Extract random effects
random_effects <- as.data.frame(VarCorr(m0))
random_effects <- data.frame(
  Group = random_effects$grp,
  Variance = random_effects$vcov,
  SD = random_effects$sdcor
)

# Extract ANOVA results for fixed effects
anova_results <- car::Anova(m0, type = "II")
anova_results <- as.data.frame(anova_results)
anova_results <- data.frame(
  Predictor = rownames(anova_results),
  Chisq = anova_results$Chisq,
  Df = anova_results$Df,
  p_value = anova_results$`Pr(>Chisq)`,
  Significance = ifelse(anova_results$`Pr(>Chisq)` < 0.001, "***",
                        ifelse(anova_results$`Pr(>Chisq)` < 0.01, "**",
                               ifelse(anova_results$`Pr(>Chisq)` < 0.05, "*",
                                      ifelse(anova_results$`Pr(>Chisq)` < 0.1, ".", ""))))
)

# Perform post-hoc pairwise comparisons for VervetSeason
posthoc_seasons <- emmeans(m0, pairwise ~ VervetSeason, adjust = "bonferroni")
posthoc_results <- as.data.frame(summary(posthoc_seasons$contrasts))
posthoc_results <- data.frame(
  Contrast = posthoc_results$contrast,
  Estimate = posthoc_results$estimate,
  SE = posthoc_results$SE,
  z_value = posthoc_results$z.ratio,
  p_value = posthoc_results$p.value,
  Significance = ifelse(posthoc_results$p.value < 0.001, "***",
                        ifelse(posthoc_results$p.value < 0.01, "**",
                               ifelse(posthoc_results$p.value < 0.05, "*",
                                      ifelse(posthoc_results$p.value < 0.1, ".", ""))))
)

# Styling for Fixed Effects Table
fixed_effects_table <- model_parameters[, c("Parameter", "Coefficient", "SE", "z", "p")]
colnames(fixed_effects_table) <- c("Predictor", "Estimate", "Std.Error", "z-value", "p-value")
fixed_effects_table <- fixed_effects_table %>%
  mutate(Significance = ifelse(`p-value` < 0.001, "***",
                               ifelse(`p-value` < 0.01, "**",
                                      ifelse(`p-value` < 0.05, "*",
                                             ifelse(`p-value` < 0.1, ".", ""))))) %>%
  kbl(caption = "GLMM Fixed Effects Results", format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, font_size = 14)

# Styling for Random Effects Table
random_effects_table <- random_effects %>%
  kbl(caption = "Random Effects Variance Components", format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, font_size = 14)

# Styling for ANOVA Table
anova_table <- anova_results %>%
  kbl(caption = "ANOVA Results for Fixed Effects", format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, font_size = 14)

# Styling for Post-hoc Comparisons Table
posthoc_table <- posthoc_results %>%
  kbl(caption = "Post-hoc Pairwise Comparisons for VervetSeason", format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, font_size = 14)

# Print tables
print(fixed_effects_table)
print(random_effects_table)
print(anova_table)
print(posthoc_table)






# Load necessary libraries
library(dplyr)
library(knitr)
library(kableExtra)
library(lme4)
library(parameters)
library(emmeans)
library(car)

# Extract model parameters (fixed effects)
model_parameters <- parameters::model_parameters(m0)

# Rename the predictors
predictor_names <- c(
  "VervetSeasonWinter" = "Winter (Ecological Season)",
  "VervetSeasonMating" = "Mating (Ecological Season)",
  "IzELO" = "Rank Difference",
  "AgeDiff" = "Age Difference",
  "DyadDay:IzDSI" = "Interaction x Initial Social Bond",
  "BirthGpBD" = "Birth Group BD",
  "BBYes" = "Presence of Baby",
  "BirthGpNH" = "Birth Group NH"
)

# Apply renaming to the Parameter column
model_parameters$Parameter <- predictor_names[model_parameters$Parameter]

# Filter significant and marginally significant effects
significant_fixed_effects <- model_parameters %>%
  filter(p < 0.1) %>% # Keep rows with p-values < 0.1
  select(Parameter, Coefficient, SE, z, p) %>% # Select relevant columns
  mutate(
    Significance = case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      p < 0.1 ~ ".",
      TRUE ~ ""
    )
  )

# Rename columns for better clarity
colnames(significant_fixed_effects) <- c(
  "Predictor", "Estimate", "Std.Error", "z-value", "p-value", "Significance"
)

# Sort by p-value for better presentation
significant_fixed_effects <- significant_fixed_effects %>%
  arrange(`p-value`)

# Generate the table with custom styling
significant_table <- significant_fixed_effects %>%
  kbl(
    caption = "Significant and Marginally Significant Fixed Effects (Ordered by Significance)",
    format = "html",
    digits = 4
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 16
  ) %>%
  row_spec(
    which(significant_fixed_effects$Significance %in% c("***", "**", "*")), # Highlight significant
    background = "#b3e6cc"
  ) %>%
  row_spec(
    which(significant_fixed_effects$Significance == "."), # Highlight marginally significant
    background = "#fef5e6"
  ) %>%
  column_spec(1, bold = TRUE) %>% # Bold the Predictor column
  column_spec(2:6, width = "1.5in") # Adjust column widths

# Print the styled table
print(significant_table)

