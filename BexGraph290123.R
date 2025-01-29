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

# VERVET VervetSeasonS
BexFinal$Date <- as.Date(BexFinal$Date, format = "%Y-%m-%d")

# Ass a vervet sweasons with amting VervetSeason and bb VervetSeason
# Ass a vervet sweasons with amting VervetSeason and bb VervetSeason
# Create "VervetVervetSeason" based on months
BexFinal <- BexFinal %>%
  mutate(VervetSeason = case_when(
    month(Date) %in% c(1, 2, 3) ~ "Summer",
    month(Date) %in% c(4, 5, 6) ~ "Mating",
    month(Date) %in% c(7, 8, 9) ~ "Winter",
    month(Date) %in% c(10, 11, 12) ~ "Baby",
    TRUE ~ NA_character_  # Default for missing dates
  ))


# Change format VervetSeason & Vevert VervetSeason
BexFinal$VervetSeason <- factor(BexFinal$VervetSeason, levels = c("Summer","Mating", "Winter","Baby"), ordered = F)
# Ensure Date column is properly recognized as a Date type
BexFinal$Date <- as.Date(BexFinal$Date)

BexFinal$BB <- as.factor(BexFinal$BB)
BexFinal$AgeDir <- as.factor(BexFinal$AgeDir)
#BexFinal$IzELO <- as.factor(BexFinal$IzELO) # IzELO is NOT a factor! It should be numeric: the difference increases when it goes from 1 to 2. 
# If you make it a factor, it sees 1 as a seperate group (like NH for example) from 2 (as if it was BD), but they're not, it's increasing
BexFinal$Dyad <- as.factor(BexFinal$Dyad)
BexFinal$Date <- as.Date(BexFinal$Date) # You changed this into a factor, don't do that!!!


str(BexFinal)


# MARGINAL EFFECTS

# Load required libraries
# Load required libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Re-categorize Age Differences
BexFinal <- BexFinal %>%
  mutate(
    AgeCategory = case_when(
      AgeDiff < 2 ~ "<2",
      AgeDiff >= 2 & AgeDiff <= 4 ~ "Between 2 to 4",
      AgeDiff > 4 ~ ">4"
    ),
    AgeCategory = factor(AgeCategory, levels = c("<2", "Between 2 to 4", ">4"))
  )

# Helper function to calculate proportions and standard deviations
calculate_summary <- function(data, group_var) {
  data %>%
    group_by({{ group_var }}) %>%
    summarize(
      Proportion = mean(Tol, na.rm = TRUE),
      SD = sd(Tol, na.rm = TRUE),
      Count = n(),
      .groups = "drop"
    )
}

# AGE DIFFERENCE: Bar plot with distinct colors
age_summary <- calculate_summary(BexFinal, AgeCategory)
Agediffplot <- ggplot(age_summary, aes(x = AgeCategory, y = Proportion, fill = AgeCategory)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Proportion - SD, ymax = Proportion + SD), width = 0.2, color = "black") +
  geom_hline(yintercept = mean(BexFinal$Tol, na.rm = TRUE), linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("dodgerblue", "seagreen", "darkorange")) +
  labs(
    title = "Effect of Age Difference on Tolerance",
    x = "Age Difference (years)",
    y = "Tolerance",
    fill = "Age Difference"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top"
  )

# AGE DIRECTION: Bar plot
age_dir_summary <- calculate_summary(BexFinal, AgeDir)
Agedirplot <- ggplot(age_dir_summary, aes(x = AgeDir, y = Proportion, fill = AgeDir)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Proportion - SD, ymax = Proportion + SD), width = 0.2, color = "black") +
  geom_hline(yintercept = mean(BexFinal$Tol, na.rm = TRUE), linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("hotpink", "steelblue")) +
  labs(
    title = "Effect of Older Individual on Tolerance",
    x = "Older Individual",
    y = "Tolerance",
    fill = "Older Individual"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top"
  )

# DSI: Retain trendline over time
BexFinal <- BexFinal %>%
  mutate(
    BondCategory = case_when(
      IzDSI <= quantile(IzDSI, 0.33, na.rm = TRUE) ~ "Small Bond",
      IzDSI > quantile(IzDSI, 0.33, na.rm = TRUE) & IzDSI <= quantile(IzDSI, 0.66, na.rm = TRUE) ~ "Average Bond",
      IzDSI > quantile(IzDSI, 0.66, na.rm = TRUE) ~ "High Bond"
    )
  )

Dsiplot <- ggplot(BexFinal, aes(x = Date, y = Tol, color = BondCategory, group = BondCategory)) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  geom_hline(yintercept = mean(BexFinal$Tol, na.rm = TRUE), linetype = "dashed", color = "red") +
  scale_color_manual(values = c("turquoise", "darkgreen", "purple")) +
  labs(
    title = "Effect of Initial Social Bonds on Tolerance Over Time",
    x = "Date",
    y = "Tolerance",
    color = "Initial Social Bond"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "top"
  )

# BABY PRESENCE: Bar plot
baby_summary <- calculate_summary(BexFinal, BB)
Bbplot <- ggplot(baby_summary, aes(x = BB, y = Proportion, fill = BB)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Proportion - SD, ymax = Proportion + SD), width = 0.2, color = "black") +
  geom_hline(yintercept = mean(BexFinal$Tol, na.rm = TRUE), linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("darkorange", "purple")) +
  labs(
    title = "Effect of Baby Presence on Tolerance",
    x = "Baby Presence",
    y = "Tolerance",
    fill = "Baby Presence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top"
  )

# Combine all plots into a grid layout using `patchwork`
combined_plot <- (Agediffplot | Agedirplot) / (Dsiplot | Bbplot) +
  plot_annotation(
    title = "Overview of Marginally Non-Significant Effects on Tolerance",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    )
  )

# Display the combined plot
print(combined_plot)








# Calculate proportions and standard deviations per dyad
dyad_summary <- BexFinal %>%
  group_by(Dyad) %>%  # Create a unique dyad identifier
  summarize(
    Proportion = mean(Tol, na.rm = TRUE),
    SD = sd(Tol, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  ) %>%
  arrange(Proportion)  # Arrange dyads by increasing proportion

# Bar plot: Proportion of tolerance per dyad
barplot_dyad <- ggplot(dyad_summary, aes(x = reorder(Dyad, Proportion), y = Proportion)) +
  geom_col(fill = "skyblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = Proportion - SD, ymax = Proportion + SD), width = 0.2, color = "black") +
  labs(
    title = "Proportion of Tolerance per Dyad",
    x = "Dyad",
    y = "Proportion of Tolerance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),  # Rotate dyad labels for clarity
    axis.text.y = element_text(size = 12)
  )

# Display the plot
print(barplot_dyad)






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




#BOXPLOTSFORIZELO


# Load libraries
library(ggplot2)
library(dplyr)


# Step 1: Calculate summary statistics for each IzELO category
summary_stats <- BexFinal %>%
  group_by(IzELO) %>%
  summarise(
    Mean = mean(Tol),
    Q1 = quantile(Tol, 0.25),
    Q3 = quantile(Tol, 0.75),
    Min = min(Tol),
    Max = max(Tol),
    SD = sd(Tol)
  )

print(summary_stats) # For verification

# Step 2: Create a custom boxplot using the calculated statistics
ggplot(summary_stats, aes(x = factor(IzELO), fill = factor(IzELO))) +
  # Add the box for Q1 to Q3
  geom_boxplot(aes(ymin = Q1, lower = Q1, middle = Mean, upper = Q3, ymax = Q3),
               stat = "identity", alpha = 0.5, color = "black", size = 1) +
  # Add the whiskers for Min and Max
  geom_errorbar(aes(ymin = Min, ymax = Max), width = 0.2, size = 1) +
  # Add the mean as a larger red cross
  geom_point(aes(y = Mean), color = "red", shape = 4, size = 5, stroke = 1.5) +
  # Customize the labels and theme
  labs(
    title = "Effect of Rank Difference on Tolerance",
    x = "Male-Female Quartile Rank Difference",
    y = "Tolerance Probability",
    fill = "Quartile Rank Differences"
  ) +
  scale_fill_manual(
    values = c("1" = "#D55E00", "2" = "#0072B2", "3" = "#009E73"),
    labels = c("Low (Q1)", "Mid (Q2)", "High (Q3)")
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top" # Adjusted legend position
  ) +
  coord_cartesian(ylim = c(0.4, 1.1)) # Adjust y-axis limits



# BOXPLOT FOR ELO

# Load libraries
library(ggplot2)
library(dplyr)


# Step 1: Calculate summary statistics for each IzELO category
summary_stats <- BexFinal %>%
  group_by(IzELO) %>%
  summarise(
    Mean = mean(Tolerance),
    Q1 = quantile(Tolerance, 0.25),
    Q3 = quantile(Tolerance, 0.75),
    Min = min(Tolerance),
    Max = max(Tolerance),
    SD = sd(Tolerance)
  )

print(summary_stats) # For verification

# Step 2: Create a clean custom boxplot
ggplot(summary_stats, aes(x = factor(IzELO), fill = factor(IzELO))) +
  # Add custom box representation using Q1, Q3, Min, and Max
  geom_rect(aes(xmin = as.numeric(IzELO) - 0.4, xmax = as.numeric(IzELO) + 0.4, 
                ymin = Q1, ymax = Q3), alpha = 0.5, color = "black") +
  # Add whiskers
  geom_errorbar(aes(x = as.numeric(IzELO), ymin = Min, ymax = Max), width = 0.2, size = 1) +
  # Add mean points
  geom_point(aes(x = as.numeric(IzELO), y = Mean), color = "red", shape = 19, size = 3) +
  # Customize the labels and theme
  labs(
    title = "Effect of Rank Difference on Tolerance",
    x = "Male-Female Quartile Rank Difference",
    y = "Tolerance Probability",
    fill = "Quartile Rank Differences"
  ) +
  scale_fill_manual(
    values = c("1" = "#D55E00", "2" = "#0072B2", "3" = "#009E73"),
    labels = c("Low (Q1)", "Mid (Q2)", "High (Q3)")
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top" # Adjusted legend position
  ) +
  coord_cartesian(ylim = c(0.4, 1.1)) # Adjust y-axis limits































# We have to rescale variables because otherwise the model doesn't converge. Doesn't change anything, 
# just makes your continuous variables around 0 (-1 to 1 for example)
BexFinal$IzDSI <- scale(BexFinal$IzDSI)
BexFinal$AgeDiff <- scale(BexFinal$AgeDiff)
BexFinal$IzELO <- scale(BexFinal$IzELO)
BexFinal$AgeDiffDir <- scale(BexFinal$AgeDiffDir)
BexFinal$IzELODir <- scale(BexFinal$IzELODir)

?scale

BexFinal

m0 <- glmer(Tol ~ DyadDay * (IzDSI + IzELO + AgeDiff + AgeDir + HigherElo) + VervetVervetSeason + BirthGp + BB +
              (1|Date), 
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
            family = binomial(link = "logit"),
            data = BexFinal)



summary(m0)
Anova(m0)











# Calculate proportions and standard deviations per dyad
dyad_summary <- BexFinal %>%
  group_by(Dyad  ) %>%  # Create a unique dyad identifier
  summarize(
    Proportion = mean(Tol, na.rm = TRUE),
    SD = sd(Tol, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  ) %>%
  arrange(Proportion)  # Arrange dyads by increasing proportion
# Load RColorBrewer for pastel palettes
library(RColorBrewer)

# Add pastel colors for each bar
barplot_dyad_pastel <- ggplot(dyad_summary, aes(x = reorder(Dyad, Proportion), y = Proportion, fill = Dyad)) +
  geom_col(alpha = 0.8) +  # Colored bars with pastel shades
  geom_errorbar(aes(ymin = Proportion - SD, ymax = Proportion + SD), width = 0.2, color = "black") +
  geom_hline(yintercept = mean(dyad_summary$Proportion, na.rm = TRUE), 
             linetype = "dashed", color = "red", size = 1) +  # Mean tolerance line
  geom_text(aes(label = round(Proportion, 2)), 
            vjust = -0.5, size = 4) +  # Add proportion labels on top of bars
  scale_fill_manual(
    values = brewer.pal(n = nrow(dyad_summary), "Pastel1")  # Pastel color palette
  ) +
  labs(
    title = "Proportion of Tolerance per Dyad",
    x = "Dyad",
    y = "Proportion of Tolerance",
    fill = "Dyad"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),  # Rotate dyad labels
    axis.text.y = element_text(size = 12),
    legend.position = "none"  # Remove legend for simplicity
  )

# Display the updated plot
print(barplot_dyad_pastel)



