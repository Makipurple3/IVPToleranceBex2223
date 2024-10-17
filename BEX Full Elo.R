# FULL ELO 



# AGE & CLASS

# Load necessary packages
library(data.table)

# Import data
LHdata <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/IVP Life history_180424.csv", 
                   header = TRUE, stringsAsFactors = FALSE, na.strings = c('NA', 'Not yet'))

# Format date columns properly
LHdata$DOB <- as.Date(LHdata$DOB, format = "%d/%m/%Y")
LHdata$FirstRecorded <- as.Date(LHdata$FirstRecorded, format = "%d/%m/%Y")
LHdata$DepartureNatalGp <- as.Date(LHdata$DepartureNatalGp, format = "%d/%m/%Y")

# Filter out rows where Code is NA
LHdata <- LHdata %>% filter(!is.na(Code))

# Function to calculate age based on either DOB or FirstRecorded date
# Adds 4 years to males with no DOB
calculate_age <- function(date_of_birth, first_recorded, sex, target_date = Sys.Date()) {
  # Ensure sex is not missing
  if (is.na(sex)) {
    return(NA)
  }
  
  # Handle missing DOB with valid FirstRecorded, adding 4 years for males
  if (is.na(date_of_birth) & !is.na(first_recorded)) {
    if (sex == "M") {
      first_recorded <- first_recorded - (4 * 365.25)  # Subtract 4 years for males (4 years in days)
    }
    return(as.numeric(difftime(target_date, first_recorded, units = "weeks")) / 52.143)
  }
  
  # Return age based on DOB if available
  return(as.numeric(difftime(target_date, date_of_birth, units = "weeks")) / 52.143)
}

# Function to assign age class
get_age_class <- function(sex, age) {
  # Ensure sex and age are not missing
  if (is.na(sex) || is.na(age)) {
    return(NA)
  }
  
  # Adjust for age class thresholds
  if (sex == "F" && age >= 4) return("AF")  # Adult Female
  if (sex == "M" && age >= 5) return("AM")  # Adult Male
  return("Juvenile")  # Default to Juvenile if criteria are not met
}

# Filter out unrealistic ages (e.g., > 25 years) and update Age column and AgeClass
LHdata <- LHdata %>%
  rowwise() %>%
  mutate(Age = calculate_age(DOB, FirstRecorded, Sex),
         AgeClass = get_age_class(Sex, Age)) %>%
  filter(Age < 25 | is.na(Age))  # Allow NA ages and set the 25-year threshold

# Add 5 years to Ouli's age
LHdata <- LHdata %>%
  mutate(Age = if_else(Code == "Ouli", Age + 5, Age))

# Save age and class data to CSV
write.csv(LHdata[, c("Code", "Sex", "DOB", "Age", "AgeClass")], "age_class_data.csv", row.names = FALSE)

# Print a summary after filtering out unrealistic ages
print(summary(LHdata$Age))
print(table(LHdata$AgeClass))

# List of dyads for which you want to print Age and AgeClass
dyad_ids <- c("Buk", "Ginq", "Sho", "Ndaw", "Kom", "Oort", "Xin", "Ouli", "Xia", 
              "Piep", "Sey", "Sirk", "Oerw", "Nge", "Xian", "Pom")

# Print the Age and AgeClass for the specific dyads without filtering or saving
LHdata %>%
  filter(Code %in% dyad_ids) %>%
  select(Code, Age, AgeClass) %>%
  print()













# FINAL AGONISTIC

# Load necessary packages
library(tidyr)
library(stringr)
library(lubridate)

# Load old and new agonistic data
# This dataframe contains agonistic ad lib data from 2016 to May 2023.
old <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/Agonistic2016-2023.csv")

#Old inspection and date handling
# Check the column names in old
print("Columns in 'old' dataset:")
print(colnames(old))
# Check the class of the Date column
class(old$Date)
# Convert Date from character to Date format
old$Date <- as.Date(old$Date, format= "%Y-%m-%d")
# Check for NA and Remove 
sum(is.na(old$Date))
old <- old[!is.na(old$Date), ]
# Check of min and max date in Old
max((old$Date))
min((old$Date))



#I have old and new agonsitic file but,
# because I only need data from sept 2022 
# to march 2023 I will only use "old" agnostic with data from 2016 to may 2023 

#treating NA in old (agnostic 2016-2023)
# Check for missing values in old
missing_values_summary <- sapply(old, function(x) sum(is.na(x)))
print("Missing values per column:")
print(missing_values_summary)




# not that it has already been done in "old" but if more data is included in agonistic final
# a) import dataset to merge (names:new) and select column of interest (Date	Time	Group	Aggressor	AggressorBehaviour	Victim	VictimBehaviour)
# b) put same name in columns if necessary for merge and assure same format as "old" is respected
# c) merge old and new into "elo" df, make sure they are no overlapping between both datsets (old, new)
# d) this file now should contain all ad lib recorded interactions that had no support

# Save the cleaned dataset
write.csv(old, "FinalAgonistic_Clean.csv", row.names = FALSE)
print("Final agonistic data saved to 'FinalAgonistic_Clean.csv' after removing missing values.")







# WINNER-LOSER DATA



# Load old and new agonistic data
final_agnostic <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/FinalAgonistic_Clean.csv")



# Remove rows with missing values and save a cleaned version of the dataset
final_agnostic<- final_agnostic %>% drop_na()


# Create 'winner' and 'loser' columns
final_agonistic <- old %>%
  mutate(winner = Aggressor,
         loser = Victim)



# Check for missing values
missing_values_summary <- sapply(final_agonistic, function(x) sum(is.na(x)))
print("Missing values per column:")
print(missing_values_summary)

# Remove rows with missing values in critical columns
final_agonistic_clean <- final_agonistic %>%
  drop_na(Date, Time, Group, winner, loser)


# Save the cleaned dataset
write.csv(final_agonistic_clean, "FinalAgonistic_Clean.csv", row.names = FALSE)
print("Final agonistic data saved to 'FinalAgonistic_Clean.csv' after removing missing values.")

































# ELO RATING

# Load Presence Data
AKpres <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/pres_ak.csv", stringsAsFactors = FALSE)
BDpres <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/pres_bd.csv", stringsAsFactors = FALSE)
NHpres <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/pres_nh.csv", stringsAsFactors = FALSE)


# Rename the first column to "Date"
colnames(AKpres)[1] <- "Date"
colnames(BDpres)[1] <- "Date"
colnames(NHpres)[1] <- "Date"


# Convert 'Date' columns to Date type
AKpres$Date <- as.Date(AKpres$Date, format = "%Y-%m-%d")
BDpres$Date <- as.Date(BDpres$Date, format = "%Y-%m-%d")
NHpres$Date <- as.Date(NHpres$Date, format = "%Y-%m-%d")


colnames(AKpres)
colnames(BDpres)
colnames(NHpres)




# Filter LHdata for females and males (AF = Adult Female, AM = Adult Male)
females <- LHdata %>%
  filter(AgeClass == "AF") %>%
  select(Code)

males <- LHdata %>%
  filter(AgeClass == "AM") %>%
  select(Code)

# Extract female and male IDs as vectors
female_ids <- females$Code
male_ids <- males$Code

# Check if female and male IDs were correctly extracted
print(female_ids)
print(male_ids)



# Filter AK, BD, and NH presence data by female and male IDs

# AK group
AKpres_females <- AKpres %>% select(Date, one_of(intersect(female_ids, colnames(AKpres))))

AKpres_males <- AKpres %>% select(Date, one_of(intersect(male_ids, colnames(AKpres))))

# BD group
BDpres_females <- BDpres %>% select(Date, one_of(intersect(female_ids, colnames(BDpres))))
BDpres_males <- BDpres %>% select(Date, one_of(intersect(male_ids, colnames(BDpres))))

# NH group
NHpres_females <- NHpres %>% select(Date, one_of(intersect(female_ids, colnames(NHpres))))
NHpres_males <- NHpres %>% select(Date, one_of(intersect(male_ids, colnames(NHpres))))






# Check the filtered data to verify

# AK Group
print("AK females:")
print(colnames(AKpres_females))

print("AK males:")
print(colnames(AKpres_males))

# BD Group
print("BD females:")
print(colnames(BDpres_females))

print("BD males:")
print(colnames(BDpres_males))

# NH Group
print("NH females:")
print(colnames(NHpres_females))

print("NH males:")
print(colnames(NHpres_males))






#AK Group Filtering
# AK Females: Just filter by IDs
AKpres_females <- AKpres %>% select(Date, one_of(intersect(female_ids, colnames(AKpres))))

# AK Males: Filter and then remove unwanted individuals (Nak, Guz, Nda, Gil, Gub, Tch, Nca)
ak_males_remove <- c("Nak", "Guz", "Nda", "Gil", "Gub", "Tch", "Nca")
AKpres_males <- AKpres %>% 
  select(Date, one_of(intersect(male_ids, colnames(AKpres)))) %>%
  select(-one_of(ak_males_remove))

# Keep only Sho, Buk, and Vla
ak_males_keep <- c("Sho", "Buk", "Vla")
AKpres_males <- AKpres %>% 
  select(Date, one_of(ak_males_keep))


# NH Group Filtering
# NH Females: Remove 'Gree'
NHpres_females <- NHpres %>%
  select(Date, one_of(intersect(female_ids, colnames(NHpres)))) %>%
  select(-one_of("Gree"))

# NH Males: Remove unwanted individuals (Roc, Uls, Ram, Rim, Xar, Bet, Xin)
nh_males_remove <- c("Roc", "Uls", "Ram", "Rim", "Xar", "Bet", "Xin")
NHpres_males <- NHpres %>%
  select(Date, one_of(intersect(male_ids, colnames(NHpres)))) %>%
  select(-one_of(nh_males_remove))

# Check the filtered data to verify


# BD Group Filtering
# BD Males : Romve unwanted individuals ("Boo", "Dri", "Bra", "Win", "Tot", "Bob", "Ott", "Ros", "Pie","Nuk", "PlainJane", "Apa", "Oup", "War", "Pix", "Dal", "Aal","Pom", "Dok", "Ted", "Pro")

bd_males_remove <- c("Boo", "Dri", "Bra", "Win", "Tot", "Bob", "Ott", "Ros", "Pie",
                    "Nuk", "PlainJane", "Apa", "Oup", "War", "Pix", "Dal", "Aal",
                    "Pom", "Dok", "Ted", "Pro")


BDpres_males <- BDpres %>%
  select(Date, one_of(intersect(male_ids, colnames(BDpres)))) %>%
  select(-one_of(bd_males_remove))



# AK Group
print("AK females:")
print(colnames(AKpres_females))

print("AK males (after removal):")
print(colnames(AKpres_males))

# NH Group
print("NH females (after removal):")
print(colnames(NHpres_females))

print("NH males (after removal):")
print(colnames(NHpres_males))

# BD Group
print("BD females (after removal):")
print(colnames(BDpres_females))

print("BD males (after removal):")
print(colnames(BDpres_males))


# ELO RATIGNGNN



# Rename the first column to "Date" fo Elo
colnames(AKpres)[1] <- "Date"
colnames(BDpres)[1] <- "Date"
colnames(NHpres)[1] <- "Date"











# Loading Interaction Data
# Load interaction data 

# Load interaction data (WinnerLoser.csv)
d <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/WinnerLoser.csv", stringsAsFactors = FALSE)

head(d$Date)

d$Date <- as.Date(d$Date, format = "%Y-%m-%d")  # Adjust the format if necessary


# Check the Structure of d:
str(d)


# Ensure that d has columns such as Date, Time, Group, winner, loser, AgeClassWinner, AgeClassLoser, etc.
min(d$Date)
max(d$Date)














# GROUP FILTERING

# Filter interaction data for AK group females
AK_AF_interactions <- d %>%
  filter(Group == "Ankhase") %>%
  filter(winner %in% female_ids & loser %in% female_ids) %>%
  filter(winner != loser)  # Remove self-interactions

# Filter interaction data for AK group males
AK_AM_interactions <- d %>%
  filter(Group == "Ankhase") %>%
  filter(winner %in% c("Sho", "Buk", "Vla") & loser %in% c("Sho", "Buk", "Vla")) %>%
  filter(winner != loser)



# Filter interaction data for NH group females
NH_AF_interactions <- d %>%
  filter(Group == "Noha") %>%  # Replace with actual group name if different
  filter(winner %in% female_ids & loser %in% female_ids) %>%
  filter(winner != loser)

# Filter interaction data for NH group males
NH_AM_interactions <- d %>%
  filter(Group == "Noha") %>%
  filter(winner %in% male_ids & loser %in% male_ids) %>%
  filter(winner != loser)




# Filter interaction data for BD group females
BD_AF_interactions <- d %>%
  filter(Group == "Baie Dankie") %>%  # Use the actual group name
  filter(winner %in% female_ids & loser %in% female_ids) %>%
  filter(winner != loser)

# Filter interaction data for BD group males
BD_AM_interactions <- d %>%
  filter(Group == "Baie Dankie") %>%
  filter(winner %in% male_ids & loser %in% male_ids) %>%
  filter(winner != loser)



# Interaction data date range for AK females
interaction_dates_AF <- AK_AF_interactions$Date
print(range(interaction_dates_AF))

# Presence data date range for AK females
presence_dates_AF <- AKpres_females$Date
print(range(presence_dates_AF))




library(EloRating)



# AK FEMALE ELO
# Check presence data alignment
seqcheck(winner = AK_AF_interactions$winner,
         loser = AK_AF_interactions$loser,
         Date = AK_AF_interactions$Date,
         presence = AKpres_females)

# Calculate Elo ratings
AKELO_AF <- elo.seq(winner = AK_AF_interactions$winner,
                    loser = AK_AF_interactions$loser,
                    Date = AK_AF_interactions$Date,
                    presence = AKpres_females, runcheck = FALSE)




#AK MALE ELO
# Check presence data alignment
seqcheck(winner = AK_AM_interactions$winner,
         loser = AK_AM_interactions$loser,
         Date = AK_AM_interactions$Date,
         presence = AKpres_males)

# Calculate Elo ratings
AKELO_AM <- elo.seq(winner = AK_AM_interactions$winner,
                    loser = AK_AM_interactions$loser,
                    Date = AK_AM_interactions$Date,
                    presence = AKpres_males, runcheck = FALSE)




# Interaction data date range for AK females
interaction_dates_AF <- AK_AF_interactions$Date
print(range(interaction_dates_AF))

# Presence data date range for AK females
presence_dates_AF <- AKpres_females$Date
print(range(presence_dates_AF))







# Check presence data alignment
seqcheck(winner = AK_AF_interactions$winner,
         loser = AK_AF_interactions$loser,
         Date = AK_AF_interactions$Date,
         presence = AKpres_females)

# Calculate Elo ratings
AKELO_AF <- elo.seq(winner = AK_AF_interactions$winner,
                    loser = AK_AF_interactions$loser,
                    Date = AK_AF_interactions$Date,
                    presence = AKpres_females, runcheck = FALSE)


# Check presence data alignment
seqcheck(winner = AK_AM_interactions$winner,
         loser = AK_AM_interactions$loser,
         Date = AK_AM_interactions$Date,
         presence = AKpres_males)

# Calculate Elo ratings
AKELO_AM <- elo.seq(winner = AK_AM_interactions$winner,
                    loser = AK_AM_interactions$loser,
                    Date = AK_AM_interactions$Date,
                    presence = AKpres_males, runcheck = FALSE)

# Plot Elo ratings for AK group females
eloplot(AKELO_AF)

# Extract Elo scores
elo_scores_AF <- extract_elo(AKELO_AF)
print(elo_scores_AF)

