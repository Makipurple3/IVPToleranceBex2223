# Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(gt)
packageVersion("vctrs")
library(lubridate) # For date manipulation



#OPEN BEXFINAL
BexFinal <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexFinal.csv")
colnames(BexFinal)

# Convert Date column to Date format if not already done
BexFinal <- BexFinal %>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
str(BexFinal)


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





# TOTAL TRIAL SUMMARY


# Check and convert TotalTrials to numeric if needed
BexFinal <- BexFinal %>%
  mutate(TotalTrials = as.numeric(TotalTrials)) # Ensure TotalTrials is numeric

# Create the summary table
TrialSummary <- BexFinal %>%
  dplyr::select(Dyad, Male, Female, BirthGp, TotalTrials) %>% # Explicitly use dplyr's select
  distinct() %>% # Remove duplicate rows
  arrange(desc(TotalTrials)) # Sort by TotalTrials in descending order

# Display the summary table in the console
TrialSummary

# Create a table plot




# AMount of days of trial
# Load required libraries
library(dplyr)
library(gt)

# Ensure TotalTrials is numeric
BexFinal <- BexFinal %>%
  mutate(TotalTrials = as.numeric(TotalTrials)) # Ensure TotalTrials is numeric

# Calculate mean and SD for TotalTrials and DyadDay
trial_summary_stats <- BexFinal %>%
  summarise(
    TotalTrialsMean = mean(TotalTrials, na.rm = TRUE),
    TotalTrialsSD = sd(TotalTrials, na.rm = TRUE),
    DyadDayMean = mean(DyadDay, na.rm = TRUE),
    DyadDaySD = sd(DyadDay, na.rm = TRUE)
  )

# Extract maximum TotalTrials and DyadDay for each Dyad
TrialSummary <- BexFinal %>%
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





# Table Dyad x DyadResponse frequency

# Graph Dyad X DyadResponse

# Table Dyad, IzElo (rename to IELO), HigherElo, Age Diff, Age Dir, IzDSI (rename to IDSI)

# Graph evolution date, dyad + display seasons

#TOLERANCE
  # BAR PLOTS OF TOLERANCE VS NO TOLERANCE PER DYAD











# VERVET SEASON

# Bar plot for VervetSeason and Tolerance
library(dplyr)

# Calculate mean and SE for each season
season_summary <- BexFinal %>%
  group_by(VervetSeason) %>%
  summarise(
    MeanTol = mean(Tol),
    SETol = sd(Tol) / sqrt(n())
  )

# Boxplot with Jitter for Vervet Season
ggplot(BexFinal, aes(x = VervetSeason, y = Tol, fill = VervetSeason)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +  # Add transparency to boxplot
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Overlay individual points
  labs(x = "Vervet Season", y = "Tolerance", title = "Tolerance by Vervet Season") +
  theme_minimal() +
  theme(legend.position = "none")
geom_point(stat = "summary", fun = "mean", color = "black", size = 3)
  

# Violin Plot with Jitter for Vervet Season
ggplot(BexFinal, aes(x = VervetSeason, y = Tol, fill = VervetSeason)) +
  geom_violin(alpha = 0.7) +  # Add violin plot for density
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Overlay individual points
  labs(x = "Vervet Season", y = "Tolerance", title = "Tolerance by Vervet Season") +
  theme_minimal() +
  theme(legend.position = "none")







# Distance x Tolerance



# INTERACTION MODEL EFFECT PLOT: AGE DIFF X VERVET SEASON + AGE DIFF X IZELO

library(ggplot2)

# Filter data for IzELO levels 1, 2, 3
BexFiltered <- BexFinal %>% filter(IzELO %in% c(1, 2, 3))

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
ggplot(BexFinal, aes(x = AgeDiff, y = Tol, color = factor(IzELO))) +
  geom_line(stat = "smooth", method = "lm", aes(group = IzELO), se = FALSE) +
  labs(x = "Age Difference", y = "Tolerance", color = "IzELO Level", title = "Interaction Between Age Difference and IzELO") +
  theme_minimal()

# Aggregate raw data to calculate mean Tolerance for each combination of AgeDiff and VervetSeason
library(dplyr)

raw_data_summary <- BexFinal %>%
  group_by(VervetSeason, AgeDiff) %>%
  summarise(MeanTolerance = mean(Tol), .groups = "drop")

# Plot raw data
ggplot(raw_data_summary, aes(x = AgeDiff, y = MeanTolerance, color = VervetSeason)) +
  geom_line(size = 1) +
  labs(x = "Age Difference", y = "Mean Tolerance", color = "Vervet Season", 
       title = "Interaction Between Age Difference and Vervet Season (Raw Data)") +
  theme_minimal()




# Line plot of AgeDiff vs. Tolerance with VervetSeason as lines
ggplot(BexFinal, aes(x = AgeDiff, y = Tol, color = VervetSeason, group = VervetSeason)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Interaction Between Age Difference and Vervet Season",
    x = "Age Difference",
    y = "Tolerance",
    color = "Vervet Season"
  ) +
  theme_minimal()


# Line plot for AgeDiff and VervetSeason interaction (straight lines)
ggplot(BexFinal, aes(x = AgeDiff, y = Tol, color = VervetSeason)) +
  geom_line(stat = "smooth", method = "lm", aes(group = VervetSeason), se = FALSE) +
  labs(x = "Age Difference", y = "Tolerance", color = "Vervet Season", title = "Interaction Between Age Difference and Vervet Season") +
  theme_minimal()
