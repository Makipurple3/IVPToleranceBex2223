

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
library(dplyr)
library(tidyverse)

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


# Check colnames old 
colnames(old)
# Rename old into elo
elo <- old
# check colnames of elo (before old)
colnames(elo)

# Elo is now ready, we are now do the same with the focal data





# Since we already have data from the wanted periods in focal 2022-2023 (2022 to march 2023)
# I will only upload "old" agonistic focal and after a few manipulations and check, save it as final df
# and as a new csv file final agnostic, for elo calculations


# his file is from May 2022 (when we started focalling) until May 2023 basically

### Now we do the same for focals ####
foc_old <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/Focal_agonistic2022-2023.csv")

### Structure of old focal
str(foc_old)

### changing Date from chr to date format
foc_old$Date <- as.Date(foc_old$Date)

# This file contains all the agonistic interactions recorded during focals without support
# Check the time period:
hist(foc_old$Date, breaks = "weeks")

# Check for NA in foc_old and Remove 
sum(is.na(foc_old$Date))
foc_old <- foc_old[!is.na(foc_old), ]

# Check of min and max date in foc_old
max((foc_old$Date))
min((foc_old$Date))


# If possible import and merge with data from sept 2021 (=1y of data for elo) or from beginning of 2022 (approx 9 month elo)
# Since i dont have this data for now, I am going to use focal starting from June 2022 to May 2023

# If new_focal/ "f"to import, import, covert data, and merge with old_focal before renaming "focal"
# also keep the 7 columns of interest (Date	Time	Group	Aggressor	AggressorBehaviour	Victim	VictimBehaviour)
# in short bind both focal list, make sure no duplicates are present, before binfing elo and focal together



# since we did not bind two files into a df called focal we are going to rename foc_old into focal
#check colnames of foc old
colnames(foc_old)
# rename foc_old into focals
focals <- foc_old
# check colnames of focal 
colnames(focals)
# Now we bind both ad lib and focals together
final <- bind_rows(elo, focals)

#Remove remaingin NA's
# Remove all rows with any NA values
final <- na.omit(final)




min(final$Date)
max(final$Date)

sum(is.na(final))


# Save this datafile as FinalAgonistic
write.csv(final, "/Users/maki/Desktop/Master Thesis/Josies Codes/FinalAgonistic.csv", row.names = F)
print("Final agonistic data saved to 'FinalAgonistic.csv' after removing missing values.")

# However, although the column names are Aggressor and Victim, we do not know yet which 
# individual won the interaction, and which one lost.
# Go to Create WinnerLoser to calculate the winners and losers :)

























# WINNER-LOSER DATA
library(hms)

# Load FinalAgonistic made with Create FinalAgonistic.R script
d <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/FinalAgonistic.csv")

# check structure of d
str(d)
sum(is.na(d))



# transform date and time in date and time format
# Combine 'Date' and 'Time' into 'DateTime'
d$Date <- as.Date(d$Date, format="%Y-%m-%d")
d$Time <- as_hms(d$Time)




min(d$Date)
max(d$Date)

min(d$Time)
max(d$Time)

str(d)







## Assumptions Elo-rating package:
# File is ordered by date
# There are no empty cells
# IDs occur more than once
# Loser ID is different from winner ID



# Order by date
d <- dplyr::arrange(d, Date)
head(d$Date)
# Omit NA's
d <- na.omit(d)
sum(is.na(d))
# set everything to lower case
d$AggressorBehaviour <- tolower(d$AggressorBehaviour)
d$VictimBehaviour <- tolower(d$VictimBehaviour)
# Exclude dates from before 2021
d <- d[!d$Date<"2021-01-01",]





str(d)








#### Winner/loser ####
## Based on script by Stephanie Mercier ##

# In order to add the social rank of initiators & targets and thus get their social rank differences, 
# we first need to create the "fight.data" from data which include all agonistic interactions in which 
# there is a clear winner (defined as the individual being the most aggressive, i.e. who used the most 
# intense aggressive behaviour: 
# 1. approach -> approach.data, 
# 2. aggressive -> st,at,vo,ag+tp/dp, 
# 3. chase -> ch & 
# 4. physical contact -> bi.gb.hi.fi) 
# and a clear loser (defined as the individual showing the most submissive behaviours and/or 
# ending the conflict by moving away from the opponent -> rt,av,fl,le,re,ja,cr). 




fight.data <- d


### Write a function to decide for each obs who is the winner (decide.win) based on categories of behaviours specific to vic or agg
# Victim = individual ending up the conflict being the loser, thus considered as the most submissive one
ret_beh <- c('fl', 'rt', 'av', 'ja', 'cr', 'ss', 'gu')

# Aggressor = from the least (cat_1) to the most aggressive behaviours (cat_3), the animal performing the most intense aggressive behaviour is the winner
agg_cat <- list(cat_3=c('bi', 'gb', 'hi', 'fi', 'hh', 'so'), 
                cat_2='ch', cat_1=c('ac', 'at', 'dp', 'tp', 'st', 'su', 'fh', 'sf', 'hb', 'bd'))



# write the function which defines winner/loser by looking at which individual ends the conflict and/or 
# which one performs the most aggressive behaviours
decide.win <- function(beh_x, beh_y){
  x <- 0
  y <- 0
  
  for(beh_ in ret_beh){
    x <- x + lengths(regmatches(beh_x, gregexpr(beh_, beh_x)))
    y <- y + lengths(regmatches(beh_y, gregexpr(beh_, beh_y)))
  }
  if(x < y) return(1)
  if(y < x) return(2)
  
  for(cat_ in agg_cat){
    x <- 0
    y <- 0
    
    for(beh_ in cat_){
      x <- x + lengths(regmatches(beh_x, gregexpr(beh_, beh_x)))
      y <- y + lengths(regmatches(beh_y, gregexpr(beh_, beh_y)))
    }
    
    if(x > y) return(1)
    if(y > x) return(2)
  }
  return(0)
}

# add win/lose to fight.data
fight.data$win_lose <- mapply(FUN = decide.win, 
                              fight.data$AggressorBehaviour,
                              fight.data$VictimBehaviour)

# write the function which allows to calculate for each sequence of obs between two individuals, 
# how many aggressive behaviours of the different severity each of them performed 
# (starting with the most intense one, i.e., involving physical contact), and as soon as one of the individuals
# has a greater number of aggressive behaviours performed, we declare this individual as the winner 
# (ignoring the rest of the sequence, but making sure that this one is not the individual also ending up the conflict by moving away...)

compute.wins <- function(l.data, ind_x, ind_y){
  
  fight.vec <- l.data$win_lose
  
  return(c(sum(c(fight.vec[l.data$Aggressor==ind_x]==1,
                 fight.vec[l.data$Victim==ind_x]==2)),
           sum(c(fight.vec[l.data$Aggressor==ind_y]==1,
                 fight.vec[l.data$Victim==ind_y]==2))))  
}

# Rename lol don't know why but you save your data
seq.data <- fight.data

# define winner & loser using win_lose as 1 means that Aggressor is the most aggressive ind so the winner, 
# whereas 2 means that Victim is the most aggressive ind and thus the winner!
seq.data$winner <- NA
seq.data$BehaviourW <- NA
seq.data$loser <- NA
seq.data$BehaviourL <- NA

i=1
for(i in 1:nrow(seq.data)){
  if (seq.data$win_lose[i]=="1") seq.data$winner[i] <- as.character(seq.data$Aggressor[i])
  if (seq.data$win_lose[i]=="1") seq.data$BehaviourW[i] <- as.character(seq.data$AggressorBehaviour[i])
  if (seq.data$win_lose[i]=="1") seq.data$loser[i] <- as.character(seq.data$Victim[i])
  if (seq.data$win_lose[i]=="1") seq.data$BehaviourL[i] <- as.character(seq.data$VictimBehaviour[i])
  if (seq.data$win_lose[i]=="2") seq.data$winner[i] <- as.character(seq.data$Victim[i])
  if (seq.data$win_lose[i]=="2") seq.data$BehaviourW[i] <- as.character(seq.data$VictimBehaviour[i])
  if (seq.data$win_lose[i]=="2") seq.data$loser[i] <- as.character(seq.data$Aggressor[i])
  if (seq.data$win_lose[i]=="2") seq.data$BehaviourL[i] <- as.character(seq.data$AggressorBehaviour[i])
}


# add intensity: mild, chase, severe
seq.data$intensity <- NA
mild <- which(grepl("ap|ac|ag|at|dp|tp|st|vo|gu|sc|ap0|ap2|ap10|hb",seq.data$BehaviourW)=="TRUE")
seq.data[mild,13] <- "mild"
chase <- which(grepl("ch",seq.data$BehaviourW)=="TRUE")
seq.data[chase,13] <- "chase" 
severe <- which(grepl("bi|gb|hi|fi|hh|so",seq.data$BehaviourW)=="TRUE")
seq.data[severe,13] <- "severe"


# Exclude the interactions that had no clear winners
seq.data <- seq.data[!is.na(seq.data$intensity),]  

rownames(seq.data) <- NULL

seq.data$winner <- as.factor(seq.data$winner)
nlevels(seq.data$winner)
seq.data$loser <- as.factor(seq.data$loser)
nlevels(seq.data$loser)

d <- seq.data
d$Date <- as.Date(d$Date)
d$loser <- as.character(d$loser)
str(d)






# Convert winner and loser columns to character to avoid factor replacement issues
d$winner <- as.character(d$winner)
d$loser <- as.character(d$loser)

# Replace specific names with standardized ones
d$winner[d$winner == "Chiseled nose"] <- "Bra"
d$loser[d$loser == "Chiseled nose"] <- "Bra"
d$winner[d$winner == "White dot"] <- "Tot"
d$loser[d$loser == "White dot"] <- "Tot"
d$winner[d$winner == "Fluffy"] <- "Win"
d$loser[d$loser == "Fluffy"] <- "Win"
d$winner[d$winner == "Knee scab"] <- "Dal"
d$loser[d$loser == "Knee scab"] <- "Dal"
d$winner[d$winner == "Round lips"] <- "Bob"
d$loser[d$loser == "Round lips"] <- "Bob"
d$winner[d$winner == "Kap"] <- "PlainJane"
d$loser[d$loser == "Kap"] <- "PlainJane"




## Link life history to individuals ####
# For this part you need the functions from Age and Ageclass.R
# calculate_age and get_age_class
# Import data


# Import necessary packages
library(dplyr)

# Load life history data (LHdata) and clean it
LHdata <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/IVP Life history_180424.csv", 
                   header = TRUE, stringsAsFactors = FALSE, na.strings = c('NA', 'Not yet'))

# Format date columns properly
LHdata$DOB <- as.Date(LHdata$DOB, format = "%d/%m/%Y")
LHdata$FirstRecorded <- as.Date(LHdata$FirstRecorded, format = "%d/%m/%Y")
LHdata$DepartureNatalGp <- as.Date(LHdata$DepartureNatalGp, format = "%d/%m/%Y")
LHdata$DateImmigration1 <- as.Date(LHdata$DateImmigration1, format = "%d/%m/%Y")


#Agressor
# Subset the necessary columns and filter out NAs
LHage <- LHdata[, c("Code", "Sex", "DOB", "FirstRecorded", "DepartureNatalGp", "DateImmigration1", "BirthGp")]
colnames(LHage)[1] <- "IDIndividual1"  # Rename for join consistency
LHage <- LHage %>% filter(!is.na(IDIndividual1))


# Aggressor first
LHAgg <- LHage
colnames(LHAgg)[1] <- "winner"
join_d <- left_join(d, LHAgg, by = "winner", multiple = "all")
colnames(join_d)
colnames(join_d)[14:19] <- c("WinnerSex", "DOBAgg", "FRAgg", "DNAgg", "DIAgg", "BirthGPAgg")


# Age function
# Run the function
calculate_age <- function(date_of_birth, first_recorded, departure_date, target_date, departure_natal, birthgp) {
  if (is.na(date_of_birth)) {
    age_in_years <- as.numeric(difftime(target_date, first_recorded, units = "weeks")) / 52.143
    if (!is.na(departure_date) && departure_date <= target_date) {
      if(is.na(departure_natal) && is.na(birthgp)){
        age_in_years <- age_in_years + 5
      } 
    }
  } else {
    age_in_years <- as.numeric(difftime(target_date, date_of_birth, units = "weeks")) / 52.143
  }
  return(age_in_years)
}





# Winner Age
join_d <- join_d %>%
  rowwise() %>%
  mutate(WinnerAge = calculate_age(DOBAgg, FRAgg, DIAgg, Date, DNAgg, BirthGPAgg))


# Now victim
LHVic <- LHage
colnames(LHVic)[1] <- "loser"



# Check how many times each loser appears in both datasets
join_d_loser_counts <- join_d %>% count(loser)
LHVic_loser_counts <- LHVic %>% count(loser)

# Identify which rows have more than one match
join_d_loser_multiple <- join_d_loser_counts %>% filter(n > 1)
LHVic_loser_multiple <- LHVic_loser_counts %>% filter(n > 1)

# View the problematic rows
print(join_d_loser_multiple)
print(LHVic_loser_multiple)










# Perform a many-to-many left join
join_d2 <- left_join(join_d, LHVic, by = "loser", relationship = "many-to-many")

colnames(join_d2)[21:26] <- c("LoserSex", "DOBVic", "FRVic", "DNVic", "DIVic", "BirthGPVic")
join_d2 <- join_d2 %>%
  rowwise() %>%
  mutate(LoserAge = calculate_age(DOBVic, FRVic, DIVic, Date, DNVic, BirthGPVic))

colnames(join_d2)



# Select columns of interest
d <- join_d2[, c(1:4,6,9:12,14,18,20,21,24,26,27)]
str(d)

# Ensure sex columns are characters, not numbers
d$WinnerSex <- as.character(d$WinnerSex)
d$LoserSex <- as.character(d$LoserSex)

str(d$WinnerSex)
str(d$LoserSex)
str(d$WinnerAge)
str(d$LoserAge)


# Function to assign age class based on sex and age
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

# Apply the function to the winners
d$AgeClassWinner <- mapply(get_age_class, d$WinnerSex, d$WinnerAge)

# Apply the function to the losers
d$AgeClassLoser <- mapply(get_age_class, d$LoserSex, d$LoserAge)

# Check the first few rows to verify the results
head(d$AgeClassWinner)
head(d$AgeClassLoser)


colnames(d)


# WHAT COLUMNS SHOULD I KEEP; WHAT IS DN?

# Get rid of DN column
d <- d[,c("Date","Time","Group","Aggressor","Victim","winner","loser","AgeClassWinner","AgeClassLoser")  ]

colnames(d)
# Delete NA's
d <- na.omit(d)
# Create WinnerLoser.csv ###
write.csv(d, "/Users/maki/Desktop/Master Thesis/Josies Codes/WinnerLoser.csv", row.names = F)



















# FINALLY : ELO CALCULATIONS FROM PREVIOUS DATASETS CREATED
# UPDATE FROM HERE

## Create Elo *normal* ####
library(EloRating)
library(data.table)

# Load presence files
AKpres <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/pres_ak.csv")
colnames(AKpres)[1] <- "Date"
AKpres$Date <- as.Date(format(as.POSIXct(AKpres$Date, format = "%Y-%m-%d"), "%Y-%m-%d"))
AKpres[is.na(AKpres)] <- 0

BDpres <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/pres_bd.csv")
BDpres$Date <- as.Date(format(as.POSIXct(BDpres$Date, format = "%d/%m/%Y"), "%Y-%m-%d"))
BDpres[is.na(BDpres)] <- 0

NHpres <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/pres_ak.csv")
NHpres$Date <- as.Date(format(as.POSIXct(NHpres$Date, format = "%Y-%m-%d"), "%Y-%m-%d"))
NHpres[is.na(NHpres)] <- 0


d <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/WinnerLoser.csv")
d$Date <- as.Date(format(as.POSIXct(d$Date, format = "%d/%m/%Y"), "%Y-%m-%d"))
d$Time <- format(as.POSIXct(d$Time,format="%H:%M:%S"),"%H:%M:%S")
d <- d[order(d$Time), ]
d <- d[order(d$Date), ]


str(AKpres)
str(BDpres)
str(NHpres)
str(d)



# Select here the date from where you want to use agonistic data


# In my case for each group I will have to set different dates for elo calculations
# BD Elo, end date is 13.09.2022 (maybe add 11.12.2023 for Kom Oort)
# AK Elo, end date is 26.09.2022
# NH Elo, end date is 09.03.2023
# Since I won't need any data after the 10.03.2023 I will remove all data after this date: 
# Max date = 10.03.2023

min(d$Date)
max(d$Date)

# Here we can see that d Date is from 02.01.2021 to 23.05.2023
# Now lets check the date range for the presence data in AK, BD, NH
# AK pres data range
min(AKpres$Date)
max(AKpres$Date)
# BD pres data range
min(BDpres$Date)
max(BDpres$Date)
# NH pres data range
min(NHpres$Date)
max(NHpres$Date)

# AK, BD, Nh, pres min is 01.01.2021 and max 20.04.2024
# I will select data from 02.01.2021 to 09.03.2023 for elo calculations filterning in pres and elo files

# Min Date: 2021-01-02
# Max Date: 2023-03-09

# Filter date in d
d <- d[d$Date >= "2021-01-02" & d$Date <= "2023-03-09",]
AKpres <- d[d$Date >= "2021-01-02" & d$Date <= "2023-03-09",]
BDpres <- d[d$Date >= "2021-01-02" & d$Date <= "2023-03-09",]
NHpres <- d[d$Date >= "2021-01-02" & d$Date <= "2023-03-09",]

# Check dates
min(d$Date)
max(d$Date)

min(AKpres$Date)
max(AKpres$Date)

min(BDpres$Date)
max(BDpres$Date)

min(NHpres$Date)
max(NHpres$Date)


# I will have to filter again before doing the Elo for each group

# In general, the more data you have the more accurate scores should be
# However, its worth reading Borgeaud et al., 2017: The influence of demographic variation 
# on social network stability in wild vervet monkeys. Here they discuss stability of male and female hierarchies
# on which you can base how much data you need to get an accurate estimate of your individuals.
# According to the article male stability fluctuates more , reason why it may be relevant to calculat Elo fluctuations 
# 3 month to 3 months. and ideally on min 6 month to a year



# Female Elo Calculations
# Only select adult females: ####
AF <- subset(d, d$AgeClassWinner%in%c("AF"))
AF <- subset(AF, AF$AgeClassLoser%in%c("AF"))

AK <- subset(AF,AF$Group%in%("Ankhase"))
BD <- subset(AF,AF$Group%in%("Baie Dankie"))
NH <- subset(AF,AF$Group%in%("Noha"))


# Check whether data looks good
seqcheck(winner=AK$winner, loser=AK$loser, Date=AK$Date, draw = NULL, presence=AKpres)
seqcheck(winner=BD$winner, loser=BD$loser, Date=BD$Date, draw = NULL, presence=BDpres)
seqcheck(winner=KB$winner, loser=KB$loser, Date=KB$Date, draw = NULL, presence=KBpres)
seqcheck(winner=NH$winner, loser=NH$loser, Date=NH$Date, draw = NULL, presence=NHpres)
# These will give you many warnings, but as long as there are no errors it should be fine

## AK
AKELO <- elo.seq(winner = AK$winner, loser=AK$loser, Date=AK$Date, presence = AKpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker
summary(AKELO)

# Specific IDs over a specific time period:
eloplot(AKELO, ids=c("Ginq", "Gubh", "Ndaw", "Nkos", "Ghid", "Ndon", "Ncok"), from="2021-01-02", to = " 2022-09-26")

#to get an average of the eloscore over a period 
# It counts from the date you give forwards with the amount of days in daterange
# However, the last date possible is the last day there was an agonistic interaction recorded in that group
extract_elo(AKELO, "2023-03-09", daterange=76, standardize = T)





## BD
BDELO <- elo.seq(winner = BD$winner, loser = BD$loser, Date = BD$Date, presence = BDpres, runcheck = F)

eloplot(BDELO, from = "2021-01-02", to = "2022-09-13")

extract_elo(BDELO, "2022-09-13", daterange = 87, standardize = T)


## NH
NHELO <- elo.seq(winner = NH$winner, loser = NH$loser, Date = NH$Date, presence = NHpres, runcheck = F)

eloplot(NHELO, from = "2021-01-02", to = "2023-03-09")

extract_elo(NHELO, "202-03-09", daterange = 92, standardize = T)















# Only select adult and subadult males: ####
AM <- subset(d, d$AgeClassWinner%in%c("AM", "SM"))
AM <- subset(AM, AM$AgeClassLoser%in%c("AM", "SM"))

BD <- subset(AM,AM$Group%in%("Baie Dankie"))
NH <- subset(AM,AM$Group%in%("Noha"))
AK <- subset(AM,AM$Group%in%("Ankhase"))

# Check whether data looks good
seqcheck(winner=AK$winner, loser=AK$loser, Date=AK$Date, draw = NULL, presence=AKpres)
seqcheck(winner=BD$winner, loser=BD$loser, Date=BD$Date, draw = NULL, presence=BDpres)
seqcheck(winner=KB$winner, loser=KB$loser, Date=KB$Date, draw = NULL, presence=KBpres)
seqcheck(winner=NH$winner, loser=NH$loser, Date=NH$Date, draw = NULL, presence=NHpres)

## AK
AKELO <- elo.seq(winner = AK$winner, loser=AK$loser, Date=AK$Date, presence = AKpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker
summary(AKELO)










# Specific IDs over a specific time period:
eloplot(AKELO, from="2024-01-20", to = "2024-04-20")

#to get an average of the eloscore over a period 
# It counts from the date you give forwards with the amount of days in daterange
extract_elo(AKELO, "2024-01-20", daterange=76, standardize = T)

## BD
BDELO <- elo.seq(winner = BD$winner, loser = BD$loser, Date = BD$Date, presence = BDpres, runcheck = F)

eloplot(BDELO, from = "2024-01-20", to = "2024-04-20")

extract_elo(BDELO, "2024-01-20", daterange = 87, standardize = T)

## KB
KBELO <- elo.seq(winner = KB$winner, loser = KB$loser, Date = KB$Date, presence = KBpres, runcheck = F)

eloplot(KBELO, from = "2024-01-20", to = "2024-04-20")

extract_elo(KBELO, "2024-01-20", daterange = 15, standardize = T)

## NH
NHELO <- elo.seq(winner = NH$winner, loser = NH$loser, Date = NH$Date, presence = NHpres, runcheck = F)

eloplot(NHELO, from = "2024-01-20", to = "2024-04-20")

extract_elo(NHELO, "2023-12-30", daterange = 92, standardize = T)

