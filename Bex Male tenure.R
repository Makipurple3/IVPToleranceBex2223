# You need your own dataframe with a column MaleID (code of the male), Date (date you want to calculate his tenure on),
# and a column Group. Group needs to be in the format AK; BD; NH etc, not Ankhase, Baie Dankie, etc. You can use the function below
# (change group names) to change the group names automatically.

# In this example your own dataframe is called df. You will also need to have life history data loaded, which I call LHdata in this script.

# Open library
library(dplyr)

# Open datset & prepare data
DyadSummary <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/dyad_summary_clean.csv")
LHdata <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/IVP Life history_180424.csv")


colnames(DyadSummary)
colnames(LHdata)

# Keep only the "Male" column and rename it to "MaleID"
DyadSummary <- DyadSummary %>%
  select(Male) %>%
  rename(MaleID = Male)

# Create a column group where
# Sho, Buk will have AK
# Sey, Xia, Xin, Nge, Kom will have BD
# Pom will have NH

# Create group
# Add a Group column based on MaleID
DyadSummary <- DyadSummary %>%
  mutate(
    Group = case_when(
      MaleID %in% c("Sho", "Buk") ~ "AK",
      MaleID %in% c("Sey", "Xia", "Xin", "Nge", "Kom") ~ "BD",
      MaleID == "Pom" ~ "NH",
      TRUE ~ NA_character_  # Handle unexpected values if any
    )
  )

# Create date
# Create a column date to see how long the Male has been in the group
# Date will be set at the first day of the experiment to calculate male tenure on that date
# Dates will be for
# Sho:27.09.2022
# Buk:29.09.2022

# Sey: 14.09.2022
# Xia: 16.09.2022
# Xin: 27.09.2022
# Nge: 22.09.2022
# Kom: 12.12.2022

# Pom : 10.03.2023


# Add a Date column based on MaleID
DyadSummary <- DyadSummary %>%
  mutate(
    Date = case_when(
      MaleID == "Sho" ~ as.Date("2022-09-27"),
      MaleID == "Buk" ~ as.Date("2022-09-29"),
      MaleID == "Sey" ~ as.Date("2022-09-14"),
      MaleID == "Xia" ~ as.Date("2022-09-16"),
      MaleID == "Xin" ~ as.Date("2022-09-27"),
      MaleID == "Nge" ~ as.Date("2022-09-22"),
      MaleID == "Kom" ~ as.Date("2022-12-12"),
      MaleID == "Pom" ~ as.Date("2023-03-10"),
      TRUE ~ as.Date(NA)  # Handle unexpected values if any
    )
  )

MaleSummary <- DyadSummary

# Display the updated DyadSummary
str(MaleSummary)
# Filter in lhdata/ life history to keep only box experiment indivdiduals
LHdata <- LHdata %>% filter(Code %in% c("Buk", "Sho", "Sey", "Xin", "Xia", "Nge", "Kom", "Pom"))
# Select columns in Male Tenure
LHdata <- LHdata %>%
  select(Code, PresentGp, BirthGp,DOB, FirstRecorded, DateAdult, DepartureNatalGp, DateImmigration1, DateImmigration2, DateImmigration3, DateImmigration4, LastSeen1, LastSeen2, LastSeen3, LastSeen4,ImmigrationGp1, ImmigrationGp2, ImmigrationGp3,ImmigrationGp4)

# List of variables to convert (excluding BirthGp)
date_vars <- c("FirstRecorded", "DepartureNatalGp", 
               "DateImmigration1", "DateImmigration2", "DateImmigration3", "DateImmigration4", 
               "LastSeen1", "LastSeen2", "LastSeen3", "LastSeen4")

# Convert and format each variable to date format "%Y-%m-%d"
LHdata <- LHdata %>%
  mutate(across(all_of(date_vars), ~ format(as.Date(.x, format = "%d/%m/%Y"), "%Y-%m-%d")))






## Calculating tenure





# TENURE CODE
# This is the function to calculate tenure. It needs an individual (MaleID), a date (end_two_weeks_date), your dataframe (df) and life history data (LHdata).
calculate_duration_in_group <- function(individual, end_two_weeks_date, MaleSummary, LHdata) {
  # Filter LHdata for the individual
  LH_individual <- LHdata[LHdata$Code == individual, ]
  
  
  if (nrow(LH_individual) == 0) {
    return(NA)  # Return NA if individual is not found in LHdata
  }
  
  # Get the group for this individual from df
  current_group <- MaleSummary$Group[MaleSummary$MaleID == individual]
  
  if (length(current_group) == 0) {
    return(NA)  # If group not found in df for this male, return NA
  }
  
  current_group <- current_group[1]  # In case there are multiple rows for the same individual
  
  # Initialize variables to track immigration dates and consecutive stays
  immigration_date_to_use <- NA
  found_later_immigration <- FALSE
  
  # Print the current individual and group for debugging
  print(paste("Processing:", individual, "in group:", current_group))
  
  # Loop over ImmigrationGpX columns
  for (i in 1:4) {
    imm_gp_col <- paste0("ImmigrationGp", i)
    date_imm_col <- paste0("DateImmigration", i)
    last_seen_col <- paste0("LastSeen", i)
    
    # Check if group matches the individual's group in df
    if (!is.na(LH_individual[[imm_gp_col]]) && LH_individual[[imm_gp_col]] == current_group) {
      # Get immigration date and last seen date
      immigration_date <- LH_individual[[date_imm_col]]
      last_seen <- ifelse(!is.na(LH_individual[[last_seen_col]]), as.Date(LH_individual[[last_seen_col]], format = "%d-%m-%Y"), NA)
      
      print(paste("Immigration date:", immigration_date, "Last seen date:", last_seen))
      
      # Return NA if the immigration date is NA
      if (is.na(immigration_date)) {
        print("Immigration date is NA, returning NA")
        return(NA)
      }
      
      if (!is.na(immigration_date)) {
        # If no later immigration date has been found, assign this immigration date
        if (!found_later_immigration) {
          immigration_date_to_use <- immigration_date
        }
        
        # Check if there is a later immigration for the same group and combine stays if needed
        for (j in (i+1):4) {
          next_imm_gp_col <- paste0("ImmigrationGp", j)
          next_date_imm_col <- paste0("DateImmigration", j)
          next_last_seen_col <- paste0("LastSeen", j)
          
          # Check if consecutive immigrations are in the same group
          if (!is.na(LH_individual[[next_imm_gp_col]]) && LH_individual[[next_imm_gp_col]] == current_group) {
            next_immigration_date <- LH_individual[[next_date_imm_col]]
            if (!is.na(last_seen) && !is.na(next_immigration_date)) {
              # Calculate the gap between last seen and next immigration date
              gap_duration <- as.numeric(difftime(next_immigration_date, last_seen, units = "days"))
              
              # If the gap is less than 4 months (120 days), combine the stays
              if (gap_duration < 120) {
                immigration_date_to_use <- immigration_date  # Keep the original immigration date
                found_later_immigration <- TRUE
              }
            }
            break  # Stop once the next matching immigration is found
          }
        }
        
        # If no later immigration was found, use the last seen date (if available)
        if (!found_later_immigration && !is.na(last_seen)) {
          immigration_date_to_use <- immigration_date
          break
        }
      }
    }
  }
  
  # Calculate the duration based on the immigration date to use and the specific End_TwoWeeks date
  print(paste("Immigration date to use:", immigration_date_to_use))
  
  if (!is.na(immigration_date_to_use)) {
    # Ensure end_two_weeks_date is a Date object
    end_two_weeks_date <- as.Date(end_two_weeks_date, format = "%Y-%m-%d")
    
    print(paste("End two weeks date:", end_two_weeks_date))
    
    # Check if both dates are valid
    if (!is.na(immigration_date_to_use) && !is.na(end_two_weeks_date)) {
      total_duration <- as.numeric(difftime(end_two_weeks_date, immigration_date_to_use, units = "days"))
      print(paste("Calculated duration:", total_duration))
    } else {
      print("Missing date for duration calculation")
      return(NA)  # Return NA if either date is missing
    }
  } else {
    return(NA)  # Return NA if no valid immigration date was found
  }
  
  return(total_duration)
}

# Here's how you run the function. Instead of (MaleID, Date, df, LHdata), fill in your own column names/dataframe names (or changes your names into these).
# Again, df stands for the name of your dataframe.
# Apply the function to each row in df and create a new column "Tenure"

sapply(LHdata, class)



MaleSummary <- MaleSummary %>%
  rowwise() %>%
  mutate(Tenure = calculate_duration_in_group(MaleID, Date, MaleSummary, LHdata),
         TenureYears = Tenure/365) %>%
  ungroup()


print(MaleSummary)




write.csv(MaleSummary, file = "/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/MaleSummary.csv", row.names = FALSE)










