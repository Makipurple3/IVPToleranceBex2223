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

# Display the filtered data
print(BB)


BB <- BB %>%
  select(Code, DOB, FirstRecorded, BirthGp, Mother, LastSeen1) 

print(BB)


# Add mother code, Oortjies = Oort, Oulik = Ouli, Ndawonya = Ndaw, Oerwoud = Oerw, Pieperig = Piep, Ginqika = Ginq, Sirku = Sirk
# Map mother codes
mother_mapping <- c(
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



# Addapt code for date during expereiment
# Add Mother Yes / No, depending if date, Na or Still present
# Update the BB dataframe with refined MotherDuringBex logic
BB <- BB %>%
  mutate(
    MotherCode = mother_mapping[Mother],     # Add MotherCode
    MotherYesNo = "Yes",                     # Add MotherYesNo
    MotherDuringBex = if_else(!is.na(LastSeen1) & LastSeen1 != "Still present", "Yes", "No")  # Refined logic
  )

# Display the updated dataframe
print(BB)

# Add a column  in Mother where MotherDuringBex

