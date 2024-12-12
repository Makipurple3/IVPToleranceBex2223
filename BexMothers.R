#load library
library(dplyr)
library(lubridate)

#Import dataset
lh <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/IVP Life history_180424.csv")
colnames(lh)

# I want to see if "Mother" = Ginq, Ndaw, Xian, Ouli, Oerw, Oort, Sirk, Piep and IF yes, ID and DOB for these individuals
# Checking by mothers did not owkr, I will finally uédate by BB names after manual check of the lh dataset
 

# Define the list of BB
# Vector of long names to filter for
BB <- c("BBOort22", "Olyf", "BBNdaw22", "Otter", "Pikkewyn", "Gona", "Silhoeet", "BBXian22")

# Filter the data for rows where Individual matches the long names
BB <- lh%>%
  filter(Individual %in% BB)

BB <- BB %>%
  select(Code, FirstRecorded, BirthGp, Mother, LastSeen1) 

# Convert DOB to standard Date format
# Clean and standardize FirstRecorded column
BB <- BB %>%
  mutate(
    FirstRecorded = dmy(FirstRecorded), # Assuming dd/mm/yyyy format
    LastSeen1 = case_when(
      grepl("Still present", LastSeen1, ignore.case = TRUE) ~ as.Date(NA),
      TRUE ~ dmy(LastSeen1)
    )
  )

# Add mother code, Oortjies = Oort, Oulik = Ouli, Ndawonya = Ndaw, Oerwoud = Oerw, Pieperig = Piep, Ginqika = Ginq, Sirku = Sirk
# Map mother codes
mother_mapping <- c(
  "Xian" = "Xian",
  "Oortjies" = "Oort",
  "Oulik" = "Ouli",
  "Ndawonya" = "Ndaw",
  "Oerwoud" = "Oerw",
  "Pieperig" = "Piep",
  "Ginqika" = "Ginq",
  "Sirkus" = "Sirk"
)

# Add MotherCode column
BB <- BB %>%
  mutate(MotherCode = mother_mapping[Mother])


# MotherWhileBex

# Adapt code for date during experiment: The box experiment was conducted for a year with different starting
# dates for the different dyads. I will check if the females from the box experiment had a baby during the period
# they were tested

# If FirstRecorded for the baby which name is in code is during the Bexperiod of the mother, then it wills say Yes
# IF the FirstRecorded of the BB is instead out of the experiment period, then no

# 1)First create BexStart using  these different starting dates for each mother:
# Sirk	14.09.2022, Piep	16.09.2022, Oerw 22.09.2022, Ouli		27.09.2022, Oort		12.12.2022
# Ginq		27.09.2022, Ndaw		29.09.2022
# Xian		10.03.2023

# 2)The create BexEnd setting the end at these date
#Dyad     Min_Date   Max_Date   Min_DyadDay Max_DyadDay
#<chr>    <date>     <date>           <int>       <int>
# 1 Buk Ndaw 2022-09-29 2023-09-13           1          39
# 2 Kom Oort 2022-12-13 2023-09-13           1          31
# 3 Nge Oerw 2022-09-22 2022-12-21           1          22
# 4 Pom Xian 2023-03-10 2023-09-11           1          19
# 5 Sey Sirk 2022-09-14 2023-09-13           1          53
# 6 Sho Ginq 2022-09-27 2023-09-13           1          34
# 7 Xia Piep 2022-09-16 2023-09-11           1          49
# 8 Xin Ouli 2022-09-27 2023-09-13           1          27


# Define BexStart and BexEnd dates for each mother
bex_periods <- data.frame(
  MotherCode = c("Sirk", "Piep", "Oerw", "Ouli", "Oort", "Ginq", "Ndaw", "Xian"),
  BexStart = as.Date(c("2022-09-14", "2022-09-16", "2022-09-22", "2022-09-27", 
                       "2022-12-12", "2022-09-27", "2022-09-29", "2023-03-10")),
  BexEnd = as.Date(c("2023-09-13", "2023-09-11", "2022-12-21", "2023-09-13",
                     "2023-09-13", "2023-09-13", "2023-09-13", "2023-09-11"))
)



# Merge Bex periods with the filtered dataset
BB <- BB %>%
  left_join(bex_periods, by = "MotherCode")

# Check if the baby was recorded during the Bex period
BB <- BB %>%
  mutate(BBDuringBex = ifelse(FirstRecorded >= BexStart & FirstRecorded <= BexEnd, "Yes", "No"))


print(BB)



# BB ALive (adapt in future df)
# Add a column BBAlive where if date = Na or Still present Yes, otherwise No
# Then a colum MotherWhileBex




