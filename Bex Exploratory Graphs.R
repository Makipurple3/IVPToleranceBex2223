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


# Create a styled table
# Create a styled table
TrialSummary %>%
  gt() %>%
  tab_header(
    title = "Trial Summary",
    subtitle = "Total Trials per Dyad"
  ) %>%
  cols_label(
    Dyad = "Dyad",
    Male = "Male",
    Female = "Female",
    BirthGp = "Birth Group",
    TotalTrials = "Total Trials"
  )

# Check the mean value
mean_trials <- mean(TrialSummary$TotalTrials, na.rm = TRUE) # Recalculate mean ignoring NAs

# Create the corrected bar chart
ggplot(TrialSummary, aes(x = reorder(Dyad, -TotalTrials), y = TotalTrials, fill = Dyad)) +
  geom_bar(stat = "identity", show.legend = FALSE) + # Create bar chart
  geom_text(aes(label = TotalTrials), vjust = -0.3, size = 4) + # Add labels on bars
  geom_hline(yintercept = mean_trials, color = "red", linetype = "dashed", size = 0.8) + # Add mean line
  annotate("text", x = nrow(TrialSummary) - 1, y = mean_trials + 10, 
           label = paste("Mean:", round(mean_trials, 2)), color = "red") + # Add mean annotation
  labs(
    title = "Total Trials per Dyad",
    x = "Dyad",
    y = "Total Trials"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# AMount of days of trial

# Create the summary table with max TrialDay




# Table Dyad x DyadResponse frequency

# Graph Dyad X DyadResponse

# Table Dyad, IzElo (rename to IELO), HigherElo, Age Diff, Age Dir, IzDSI (rename to IDSI)

# Graph evolution date, dyad + display seasons

#TOLERANCE
  # BAR PLOTS OF TOLERANCE VS NO TOLERANCE PER DYAD






# Distance x Tolerance
