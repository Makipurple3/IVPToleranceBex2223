#Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(gt)
packageVersion("vctrs")
library(lubridate) 




#OPEN BEXSTATs
# Import the exported file to verify
BexStats <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexStats.csv")
# Print the first few rows of the new dataset to confirm everything looks correct
colnames(BexStats)


# DATA CHECK
# TOTAL TRIAL SUMMARY
# Check and convert TotalTrials to numeric if needed
BexStats <- BexStats %>%
  mutate(TotalTrials = as.numeric(TotalTrials)) # Ensure TotalTrials is numeric
# Create the summary table
TrialSummary <- BexStats %>%
  dplyr::select(Dyad, Male, Female, BirthGp, TotalTrials) %>% # Explicitly use dplyr's select
  distinct() %>% # Remove duplicate rows
  arrange(desc(TotalTrials)) # Sort by TotalTrials in descending order
# Display the summary table in the console
TrialSummary



# AGE DIFF
# Calculate range
age_diff_range <- range(BexStats$AgeDiff, na.rm = TRUE)
# Calculate mean
age_diff_mean <- mean(BexStats$AgeDiff, na.rm = TRUE)
# Calculate standard deviation
age_diff_sd <- sd(BexStats$AgeDiff, na.rm = TRUE)
# Display results
list(
  Range = age_diff_range,
  Mean = age_diff_mean,
  StandardDeviation = age_diff_sd
)


# AGE DIR
# Calculate range
table(BexStats$Dyad, BexStats$AgeDir)

#ELO
range(BexStats$IzELO)
mean(BexStats$IzELO)
sd(BexStats$IzELO)
table(BexStats$Dyad, BexStats$IzELO)
table(BexStats$Dyad, BexStats$HigherElo) 

# DSI
range(BexStats$IzDSI)
mean(BexStats$IzDSI)
sd(BexStats$IzDSI)

#TENURE YEARS
range(BexStats$TenureYears)
mean(BexStats$TenureYears)
sd(BexStats$TenureYears)

#BB
table(BexStats$Dyad,BexStats$BB)





# START OF GRAPHS

# SUMMARY OF TRIALS CONDUCTED PER DYAD
# Ensure TotalTrials is numeric
BexStats <- BexStats %>%
  mutate(TotalTrials = as.numeric(TotalTrials)) # Ensure TotalTrials is numeric

# Calculate mean and SD for TotalTrials and DyadDay
trial_summary_stats <- BexStats %>%
  summarise(
    TotalTrialsMean = mean(TotalTrials, na.rm = TRUE),
    TotalTrialsSD = sd(TotalTrials, na.rm = TRUE),
    DyadDayMean = mean(DyadDay, na.rm = TRUE),
    DyadDaySD = sd(DyadDay, na.rm = TRUE)
  )

# Extract
TrialSummary <- BexStats %>%
  dplyr::select(Dyad, Male, Female, BirthGp, TotalTrials, DyadDay) %>% # Select relevant columns
  group_by(Dyad, Male, Female, BirthGp) %>% # Group by Dyad
  summarise(
    TotalTrials = max(TotalTrials, na.rm = TRUE),
    DyadDay = max(DyadDay, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(TotalTrials)) %>% # Sort by TotalTrials
  mutate(BirthGp = ifelse(Dyad == "Kom Oort", "BD", BirthGp)) # Change BirthGp for Kom Oort to BD

# Create the styled table with the correct statistics
TrialSummary %>%
  gt() %>%
  tab_header(
    title = "Summary of Trials conducted per Dyad"
  ) %>%
  cols_label(
    Dyad = "Dyad",
    Male = "Male",
    Female = "Female",
    BirthGp = "Birth Group",
    TotalTrials = "Total Trials",
    DyadDay = "Days of experiment"
  ) %>%
  tab_source_note(
    source_note = paste0(
      "Total Trials (Mean = 339.88, Standard Deviation = 167.54)\n",
      "Days of experiment (Mean = 34.25, Standard Deviation = 12.17)"
    )
    
  )






# VERVET SEASON<
# REPRESENTATION OF TOLERANCE X ECOLOGICAL SEASONS

# Bar plot for VervetSeason and Tolerance
library(dplyr)
unique(BexStats$VervetSeason)

# Calculate mean and SE for each season
season_summary <- BexStats %>%
  group_by(VervetSeason) %>%
  summarise(
    MeanTol = mean(Tol),
    SETol = sd(Tol) / sqrt(n())
  )

# Boxplot with Jitter for Vervet Season
ggplot(BexStats, aes(x = VervetSeason, y = Tol, fill = VervetSeason)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +  # Add transparency to boxplot
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Overlay individual points
  labs(x = "Vervet Season", y = "Tolerance", title = "Tolerance by Vervet Season") +
  theme_minimal() +
  theme(legend.position = "none")
geom_point(stat = "summary", fun = "mean", color = "black", size = 3)
  

# Violin Plot with Jitter for Vervet Season
ggplot(BexStats, aes(x = VervetSeason, y = Tol, fill = VervetSeason)) +
  geom_violin(alpha = 0.7) +  # Add violin plot for density
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Overlay individual points
  labs(x = "Vervet Season", y = "Tolerance", title = "Tolerance by Vervet Season") +
  theme_minimal() +
  theme(legend.position = "none")

# Bar plot for counts with `Tol` as numeric (0 = No Tolerance, 1 = Tolerance)
ggplot(BexStats, aes(x = VervetSeason, fill = as.factor(Tol))) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(
    x = "Vervet Ecological Seasons",
    y = "Count",
    fill = "Tolerance Levels",
    title = "Count of Tolerance per Vervet Ecological Seasons",
    caption = "Seasonal context categorized as: Summer (Jan-Mar), Mating Season (Apr-Jun), Winter (Jul-Sep), Birth Period (Oct-Dec)"
  ) +
  scale_fill_manual(
    values = c("0" = "#D55E00", "1" = "#0072B2"),
    labels = c("No Tolerance", "Tolerance")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0)
  )









# INTERACTION MODEL EFFECT PLOT: AGE DIFF X VERVET SEASON + AGE DIFF X IZELO

library(ggplot2)

# Filter data for IzELO levels 1, 2, 3
BexFiltered <- BexStats %>% filter(IzELO %in% c(1, 2, 3))

# Line plot of AgeDiff vs. Tolerance with IzELO as lines
ggplot(BexFiltered, aes(x = AgeDiff, y = Tol, color = as.factor(IzELO), group = IzELO)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Interaction Between Age Difference and IzELO",
    x = "Age Difference",
    y = "Tolerance",
    color = "IzELO Level"
  ) +
  theme_minimal()

# Line plot for AgeDiff and IzELO interaction (straight lines)
ggplot(BexStats, aes(x = AgeDiff, y = Tol, color = factor(IzELO))) +
  geom_line(stat = "smooth", method = "lm", aes(group = IzELO), se = FALSE) +
  labs(x = "Age Difference", y = "Tolerance", color = "IzELO Level", title = "Interaction Between Age Difference and IzELO") +
  theme_minimal()


#  v season boxplot

# Calculate proportion of tolerance per season
tolerance_summary <- BexStats %>%
  group_by(VervetSeason) %>%
  summarise(
    ProportionTol = mean(Tol, na.rm = TRUE),
    .groups = "drop"
  )

# Plot proportion of tolerance
ggplot(tolerance_summary, aes(x = VervetSeason, y = ProportionTol, fill = VervetSeason)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(
    aes(label = scales::percent(ProportionTol, accuracy = 0.1)),
    vjust = -0.5, size = 4
  ) +
  labs(
    title = "Proportion of Tolerance by Vervet Ecological Season",
    x = "Vervet Ecological Seasons",
    y = "Proportion of Tolerance",
    caption = "Proportion calculated as mean tolerance (Tol) per season."
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Baby" = "#F0E442",
      "Mating" = "#56B4E9",
      "Summer" = "#E69F00",
      "Winter" = "#009E73"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.caption = element_text(size = 10)
  )





# AGE DIFF X VERVET SEASON PLOT
# Filter data for AgeDiff and VervetSeason


# PLOT ,1 LINE PER SEASON, SMOOTH
ggplot(BexStats, aes(x = AgeDiff, y = Tol, color = VervetSeason, group = VervetSeason)) +
  geom_smooth(method = "loess", se = TRUE, size = 1.2) +  # Smoothed trend lines with confidence intervals
  labs(
    title = "Interaction Between Age Difference and Vervet Season",
    x = "Age Difference (years)",
    y = "Tolerance Probability (0 to 1)",
    color = "Vervet Season"
  ) +
  scale_color_manual(
    values = c("Summer" = "#F4A582", "Mating" = "#92C5DE", "Winter" = "#0571B0", "Baby" = "#F7DC6F")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  facet_wrap(~VervetSeason, ncol = 2)  # Facet the plot by season


# PLOT OF ALL SEASONS IN SAME GRAPH
ggplot(BexFinal, aes(x = AgeDiff, y = Tol, color = VervetSeason)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  labs(
    title = "Interaction Between Age Difference and Vervet Season on Tolerance",
    x = "Age Difference (years)",
    y = "Tolerance Probability (0 to 1)",
    color = "Vervet Season"
  ) +
  scale_color_manual(
    values = c("Baby" = "#E69F00", "Mating" = "#56B4E9", "Summer" = "#009E73", "Winter" = "#F0E442"),
    guide = guide_legend(override.aes = list(alpha = 1))
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0)
    
  )


# PREDICTION AGE DIFFERENCE AND SEASON
# Assuming `predictions` contains AgeDiff, VervetSeason, and PredictedTol
# Create a grid of AgeDiff and VervetSeason
age_season_grid <- expand.grid(
  AgeDiff = seq(0, 10, by = 0.1),
  VervetSeason = factor(c("Baby", "Mating", "Summer", "Winter"))
)

# Generate predicted probabilities
age_season_grid$PredictedTol <- predict(Final_Model, newdata = age_season_grid, type = "response")

# Plot the predictions
ggplot(age_season_grid, aes(x = AgeDiff, y = PredictedTol, color = VervetSeason, group = VervetSeason)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = PredictedTol - 0.05, ymax = PredictedTol + 0.05, fill = VervetSeason), alpha = 0.2) +  # Adjust SE if available
  labs(
    title = "Predicted Tolerance Probability by Age Difference and Season on Tolerance",
    x = "Age Difference (years)",
    y = "Predicted Tolerance Probability",
    color = "Vervet Season",
    fill = "Vervet Season"
  ) +
  scale_color_manual(
    values = c("Baby" = "#E69F00", "Mating" = "#56B4E9", "Summer" = "#009E73", "Winter" = "#F0E442")
  ) +
  scale_fill_manual(
    values = c("Baby" = "#E69F00", "Mating" = "#56B4E9", "Summer" = "#009E73", "Winter" = "#F0E442")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )



# AGE DIFFERENCE X IZELO


ggplot(data = BexFinal, aes(x = AgeDiff, y = Tol)) +
  geom_point(alpha = 0.6, size = 2, color = "black") +
  geom_smooth(method = "loess", se = TRUE, aes(color = factor(IzELO)), size = 1) +
  facet_wrap(~IzELO, labeller = labeller(IzELO = c("1" = "Low Rank (1)", "2" = "Middle Rank (2)", "3" = "High Rank (3)"))) +
  labs(
    title = "Interaction effect of Age Difference and Dominance Rank on Tolerance",
    x = "Age Difference (years)",
    y = "Tolerance Probability",
    color = "Rank difference"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold")
  )



ggplot(data = BexFinal, aes(x = AgeDiff, y = Tol, color = factor(IzELO), group = IzELO)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  labs(
    title = "Interaction effect of Age Difference and Dominance Rank on Tolerance ",
    x = "Age Difference (years)",
    y = "Tolerance Probability",
    color = "Rank difference"
  ) +
  scale_color_manual(
    values = c("1" = "#D55E00", "2" = "#56B4E9", "3" = "#009E73"),
    labels = c("Low Rank (1)", "Middle Rank (2)", "High Rank (3)")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )




# TIME AND TOLERANCE EVOLTUION



# Aggregate data to calculate daily mean tolerance
overall_tolerance <- BexFinal %>%
  group_by(Date) %>%
  summarise(MeanTolerance = mean(Tol, na.rm = TRUE), .groups = "drop")

# Calculate overall mean tolerance
overall_mean <- mean(overall_tolerance$MeanTolerance, na.rm = TRUE)

# Plot the overall evolution of tolerance
ggplot(overall_tolerance, aes(x = Date, y = MeanTolerance)) +
  geom_line(color = "#0072B2", size = 1.2, alpha = 0.8) +  # Blue smoothed line
  geom_point(color = "#0072B2", size = 2, alpha = 0.7) +   # Blue points
  geom_hline(yintercept = overall_mean, color = "red", linetype = "dashed", size = 1) +  # Red mean line
  labs(
    title = "Smoothed Overall Evolution of Tolerance Over Time",
    x = "Date",
    y = "Mean Tolerance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.25)
  )


# Aggregate data to calculate daily mean tolerance
overall_tolerance <- BexFinal %>%
  group_by(Date) %>%
  summarise(MeanTolerance = mean(Tol, na.rm = TRUE), .groups = "drop")

# Calculate overall mean tolerance
overall_mean <- mean(overall_tolerance$MeanTolerance, na.rm = TRUE)

# Plot the overall evolution of tolerance with smoothed lines
ggplot(overall_tolerance, aes(x = Date, y = MeanTolerance)) +
  geom_smooth(color = "#0072B2", size = 1.5, se = FALSE, method = "loess") +  # Smoothed line
  geom_point(color = "#0072B2", size = 2, alpha = 0.7) +  # Points
  geom_hline(yintercept = overall_mean, color = "red", linetype = "dashed", size = 1) +  # Red mean line
  labs(
    title = "Smoothed Overall Evolution of Tolerance Over Time",
    x = "Date",
    y = "Mean Tolerance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.25)
  )





# Aggregate data to calculate mean tolerance per dyad per day
dyad_tolerance <- BexFinal %>%
  group_by(Date, Dyad) %>%
  summarise(MeanTolerance = mean(Tol, na.rm = TRUE), .groups = "drop")

# Use a lighter color palette for the dyads
dyad_colors <- scales::hue_pal()(length(unique(dyad_tolerance$Dyad)))

# Plot tolerance evolution per dyad with smoothed lines
ggplot(dyad_tolerance, aes(x = Date, y = MeanTolerance, color = Dyad, group = Dyad)) +
  geom_smooth(size = 1.2, alpha = 0.7, se = FALSE, method = "loess") +  # Smoothed lines
  scale_color_manual(values = dyad_colors) +  # Light colors for dyads
  labs(
    title = "Smoothed Evolution of Tolerance Per Dyad Over Time",
    x = "Date",
    y = "Mean Tolerance",
    color = "Dyad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.25)
  )

# Table Dyad x DyadResponse frequency

# Graph Dyad X DyadResponse

# Table Dyad, IzElo (rename to IELO), HigherElo, Age Diff, Age Dir, IzDSI (rename to IDSI)

# Graph evolution date, dyad + display seasons

#TOLERANCE
# BAR PLOTS OF TOLERANCE VS NO TOLERANCE PER DYAD




# Distance x Tolerance
