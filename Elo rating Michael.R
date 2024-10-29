# Elo Michael
# FINALLY : ELO CALCULATIONS FROM PREVIOUS DATASETS CREATED
# UPDATE FROM HERE


## Create Elo *normal* ####
library(EloRating)
library(data.table)

# Load presence files and date formatting (YYYY-MM-DD)
AKpres <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/AK2020-2024.csv")
colnames(AKpres)[1] <- "Date"
AKpres$Date <- as.Date(AKpres$Date, format = "%Y-%m-%d")
AKpres[is.na(AKpres)] <- 0

BDpres <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/BD2020-2024.csv")
colnames(BDpres)[1] <- "Date"  
BDpres$Date <- as.Date(format(as.POSIXct(BDpres$Date, format = "%d/%m/%Y"), "%Y-%m-%d")) # This date was different then the rest, check whether it's good now!
BDpres[is.na(BDpres)] <- 0

# Correct the file path for NHpres
NHpres <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/NH2020-2024.csv")
colnames(NHpres)[1] <- "Date"
NHpres$Date <- as.Date(NHpres$Date, format = "%Y-%m-%d")
NHpres[is.na(NHpres)] <- 0

# Load WinnerLoser dataset and ensure consistent date formatting (YYYY-MM-DD)
d <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/WinnerLoser.csv") # For some reason your winner loser file doesn't go beyond May 23?
d$Date <- as.Date(format(as.POSIXct(d$Date, format = "%d/%m/%Y"), "%Y-%m-%d"))

range(d$Date)
range(AKpres$Date)
range(BDpres$Date)
range(NHpres$Date)

# Select here the date from where you want to use agonistic data
# In your case, filter from 6 months before the first experiment until the end of the experiment.

# >Finally I choose to use 12 month since it takes a few month for elo to stabilize 
# >Also I will run 3 elo for males on 3*4months of period to check for elo stability

# The elo rating function has a built in function to calculate the elo on a given date, so best to keep everything.
# You filter the same dates for the presence file.


# Filter date 
d <- d[d$Date >= "2021-09-01" & d$Date <= "2023-10-01",] # We started in September 22 so I'm using data starting in March (doesn't have to be very precise). We ended in Sept 23 so taking data until Oct 23
AKpres <- AKpres[AKpres$Date >= "2021-09-01" & AKpres$Date <= "2023-10-01",] # Do the same for presence
BDpres <- BDpres[BDpres$Date >= "2021-09-01" & BDpres$Date <= "2023-10-01",]
NHpres <- NHpres[NHpres$Date >= "2021-09-01" & NHpres$Date <= "2023-10-01",]

# Check dates
range(d$Date)
range(AKpres$Date)
range(BDpres$Date)
range(NHpres$Date)
# Now all date ranges are the same

# In general, the more data you have the more accurate scores should be
# However, its worth reading Borgeaud et al., 2017: The influence of demographic variation 
# on social network stability in wild vervet monkeys. Here they discuss stability of male and female hierarchies
# on which you can base how much data you need to get an accurate estimate of your individuals.
# According to the article male stability fluctuates more , reason why it may be relevant to calculat Elo fluctuations 
# 3 month to 3 months. and ideally on min 6 month to a year
# > For these reasons I choose 1 year of data and will, instead of 3 month periods, calculate on 
# > 4 months periods for males

# Since Female and Male Vervet's have sex differentiated hierarchy I will calculate elo separately for males and females
# Also, I will have to calculate it differently for each group and select the extraction date from the first day of experiment of each
# dyad
# Once this is done, I will print a boxplot and table placing each female and male respectively to 
# It's group, into the quartile of their hierarchy, and make a table and graph out of it



# ELO PER SEX AND GROUP
# FEMALE ELO
# Remove cases where winner = looser: d <- d[d$winner != d$loser, ]
d <- d[d$winner != d$loser, ] # Since you filter from d after this (into AF, then AM), you need to run this line before you filter

# Female Elo Calculations
# Only select adult females: 
AF <- subset(d, d$AgeClassWinner%in%c("AF"))
AF <- subset(AF, AF$AgeClassLoser%in%c("AF"))

AKF <- subset(AF,AF$Group%in%("Ankhase"))
BDF <- subset(AF,AF$Group%in%("Baie Dankie"))
NHF <- subset(AF,AF$Group%in%("Noha"))

# Check whether data looks good
seqcheck(winner=AKF$winner, loser=AKF$loser, Date=AKF$Date, draw = NULL, presence=AKpres) 
seqcheck(winner=BDF$winner, loser=BDF$loser, Date=BDF$Date, draw = NULL, presence=BDpres)
seqcheck(winner=NHF$winner, loser=NHF$loser, Date=NHF$Date, draw = NULL, presence=NHpres)

# No errors, just warnings and that's fine.
# So we can run elo.seq

## AK
AKELOF <- elo.seq(winner = AKF$winner, loser=AKF$loser, Date=AKF$Date, presence = AKpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

# If you want to plot it to see how it evolved you can run the eloplot
# Specific IDs over a specific time period:
print(unique(AKF$winner))
eloplot(AKELOF, ids=c("Ginq","Godu","Gubh", "Ndaw", "Nkos", "Ghid", "Ndon", "Ncok"), from="2021-09-01", to = "2023-10-01")

# to get an average of the eloscore over a period 
# It counts from the date you give forwards with the amount of days in daterange
# However, the last date possible is the last day there was an agonistic interaction recorded in that group


# Extract Elo scores for a specific date using 365 days 
extract_elo(AKELOF, "2022-09-01", daterange = 365, standardize = T)
# So this gives you the average standardized (between 1 and 0) elo scores of the females in AK from the start of the experiment for three months
# But please check the elo rating tutorial to see what's best for you!



AKELOF2 <-extract_elo(AKELOF, "2022-09-01", daterange = 365, standardize = T)
# TEST FOR QUARTILE
# Print the extracted Elo scores to confirm extraction
print(AKELOF2)
# Calculate quartiles for the Elo scores of females in AK
AK_quartiles <- quantile(AKELOF2, probs = c(0.25, 0.5, 0.75))
# Assign each individual in AKELOF2 to a quartile (ensure 1st Quartile is highest rank)
AKFQ <- cut(AKELOF2,
            breaks = c(-Inf, AK_quartiles, Inf),
            labels = c("4th Quartile", "3rd Quartile", "2nd Quartile", "1st Quartile"),  # Reversed labels for correct assignment
            include.lowest = TRUE)
# Create a dataframe to hold individual names, Elo scores, and their respective quartiles
AKFQ2 <- data.frame(Individual = names(AKELOF2), Elo_Score = AKELOF2, Quartile = AKFQ)
# Print the results
print(AKFQ2)
# Plot the boxplot for AK Females
par(mar = c(6, 6, 4, 2) + 0.1)  # Increase margins for readability
boxplot(AKELOF2,
        main = "Boxplot of Elo Scores for AK Females",
        ylab = "Elo Scores",
        col = "#87CEEB",
        border = "darkblue",
        notch = FALSE,    # Disable notches to avoid warnings and visual clutter
        cex.main = 1.5,   # Increase the size of the title
        cex.lab = 1.2,    # Increase the size of the axis labels
        cex.axis = 1.1)   # Increase the size of the axis numbers

# Adding individual labels to visualize each individual's score within the quartiles
text(x = 1.2, y = AKELOF2, labels = names(AKELOF2), cex = 0.9, col = "black", pos = 4)
# Adding grid lines for better visualization
grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")
# Adding points to make individual scores stand out
points(rep(1, length(AKELOF2)), AKELOF2, pch = 19, col = "red")






## BD
BDELOF <- elo.seq(winner=BDF$winner,loser=BDF$loser, Date=BDF$Date,presence=BDpres,runcheck=F)
# Extract BD Elo
extract_elo(BDELOF, "2022-09-01", daterange=90, standardize = T)
# Eloplot BD
eloplot(BDELOF,ids=c("Obse","Oort","Ouli","Puol","Aapi","Sirk","Miel","Asis","Piep","Skem","Heer","Reen","Oerw","Lewe","Naal","Rede","Hond","Numb","Nooi","Gese","Sari","Riss","Enge","Pann","Nurk","Eina"),from="2021-09-01", to = "2023-10-01")



# BD Group Elo Calculation and Quartile Analysis
BDELOF2 <- extract_elo(BDELOF, "2022-09-01", daterange = 365, standardize = T)
# Print the extracted Elo scores for BD
print(BDELOF2)
# Calculate quartiles for the Elo scores of females in BD
BD_quartiles <- quantile(BDELOF2, probs = c(0.25, 0.5, 0.75))
# Assign each individual in BDELOF2 to a quartile (ensure 1st Quartile is highest rank)
BDFQ <- cut(BDELOF2,
            breaks = c(-Inf, BD_quartiles, Inf),
            labels = c("4th Quartile", "3rd Quartile", "2nd Quartile", "1st Quartile"),
            include.lowest = TRUE)
# Create a dataframe to hold individual names, Elo scores, and their respective quartiles
BDFQ2 <- data.frame(Individual = names(BDELOF2), Elo_Score = BDELOF2, Quartile = BDFQ)
# Print the results
print(BDFQ2)
# Plot the boxplot for BD Females
par(mar = c(6, 6, 4, 2) + 0.1)  # Increase margins for readability

boxplot(BDELOF2,
        main = "Boxplot of Elo Scores for BD Females",
        ylab = "Elo Scores",
        col = "#87CEEB",
        border = "darkblue",
        notch = FALSE,    # Disable notches to avoid warnings and visual clutter
        cex.main = 1.5,   # Increase the size of the title
        cex.lab = 1.2,    # Increase the size of the axis labels
        cex.axis = 1.1)   # Increase the size of the axis numbers

# Adding individual labels to visualize each individual's score within the quartiles
text(x = 1.2, y = BDELOF2, labels = names(BDELOF2), cex = 0.9, col = "black", pos = 4)
# Adding grid lines for better visualization
grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")
# Adding points to make individual scores stand out
points(rep(1, length(BDELOF2)), BDELOF2, pch = 19, col = "red")





## NH
NHELOF <- elo.seq(winner=NHF$winner,loser=NHF$loser, Date=NHF$Date,presence=NHpres,runcheck=F)
# Extract BD Elo
extract_elo(NHELOF, "2022-09-01", daterange=90, standardize = T)
# Eloplot BD
eloplot(NHELOF,ids=c("Gran","Guat","Prai","Upps","Gaya","Xala","Pret","Xinp","Gris","Beir","Prat","Regi","Xian","Bela","Raba","Rioj"),from="2021-09-01", to = "2023-10-01")


# NH Group Elo Calculation and Quartile Analysis
NHELOF2 <- extract_elo(NHELOF, "2022-09-01", daterange = 365, standardize = T)
# Print the extracted Elo scores for NH
print(NHELOF2)
# Calculate quartiles for the Elo scores of females in NH
NH_quartiles <- quantile(NHELOF2, probs = c(0.25, 0.5, 0.75))
# Assign each individual in NHELOF2 to a quartile (ensure 1st Quartile is highest rank)
NHFQ <- cut(NHELOF2,
            breaks = c(-Inf, NH_quartiles, Inf),
            labels = c("4th Quartile", "3rd Quartile", "2nd Quartile", "1st Quartile"),
            include.lowest = TRUE)

# Create a dataframe to hold individual names, Elo scores, and their respective quartiles
NHFQ2 <- data.frame(Individual = names(NHELOF2), Elo_Score = NHELOF2, Quartile = NHFQ)
# Print the results
print(NHFQ2)
# Plot the boxplot for NH Females
par(mar = c(6, 6, 4, 2) + 0.1)  # Increase margins for readability

boxplot(NHELOF2,
        main = "Boxplot of Elo Scores for NH Females",
        ylab = "Elo Scores",
        col = "#87CEEB",
        border = "darkblue",
        notch = FALSE,    # Disable notches to avoid warnings and visual clutter
        cex.main = 1.5,   # Increase the size of the title
        cex.lab = 1.2,    # Increase the size of the axis labels
        cex.axis = 1.1)   # Increase the size of the axis numbers

# Adding individual labels to visualize each individual's score within the quartiles
text(x = 1.2, y = NHELOF2, labels = names(NHELOF2), cex = 0.9, col = "black", pos = 4)
# Adding grid lines for better visualization
grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")
# Adding points to make individual scores stand out
points(rep(1, length(NHELOF2)), NHELOF2, pch = 19, col = "red")







## MALE
# Male Elo Calculations
# Only select adult females: 
AM <- subset(d, d$AgeClassWinner%in%c("AM"))
AM <- subset(AM, AM$AgeClassLoser%in%c("AM"))

AKM <- subset(AM,AM$Group%in%("Ankhase"))
BDM <- subset(AM,AM$Group%in%("Baie Dankie"))
NHM <- subset(AM,AM$Group%in%("Noha"))

# Check date range for Males
range(AKM$Date)
range(BDM$Date)
range(NHM$Date)

# Filter for correct ID's in AKM, BDM, NHM
unique(AKM$winner)
unique(AKM$loser)
  # Remove "Tch", "Yan", consider "Vla" depending analysis


unique(BDM$winner)
unique(BDM$loser)
  # Remove "Pro","Pal","Mat", "Ted", "Dok","Bra","Win"

unique(NHM$winner)
unique(NHM$loser)
  # Remove "War","Sio"



range(AKpres$Date)








# Step 8: Run Sequence Check Before Elo Calculation (Males)
seqcheck(winner = AKM$winner, loser = AKM$loser, Date = AKM$Date, draw = NULL, presence = AKpres)
seqcheck(winner = BDM$winner, loser = BDM$loser, Date = BDM$Date, draw = NULL, presence = BDpres)
seqcheck(winner = NHM$winner, loser = NHM$loser, Date = NHM$Date, draw = NULL, presence = NHpres)




# Filter presence dataset to include only individuals involved in interactions
# Assuming the presence data has an ID column named 'ID'



### ANKHASE MALE - AKM
## AK
AKELOM <- elo.seq(winner = AKM$winner, loser=AKM$loser, Date=AKM$Date, presence = AKpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker
# Specific IDs over a specific time period:
print(unique(AKM$winner))

#ELO PLOT - AKM
# FULL AKM ELO (WILL CUT 3 MONTH PER 3 MONTH AND SET PERIOD FROM BORGEAUD AND AL 2017)
eloplot(AKELOM, ids=c("Sho","Vla","Buk"), from="2021-09-01", to = "2023-10-01")


# Plot Elo ratings over different time intervals manually
eloplot(AKELOM, ids=c("Sho","Vla","Buk"), from="2021-09-01", to = "2023-10-01")
eloplot(AKELOM, ids=c("Sho","Vla","Buk"), from="2021-09-01", to = "2022-03-01")
eloplot(AKELOM, ids=c("Sho","Vla","Buk"), from="2022-03-01", to = "2022-06-01")
eloplot(AKELOM, ids=c("Sho","Vla","Buk"), from="2022-06-01", to = "2022-09-01")


# Full period for Sho, Vla, Buk

# EXTRACT ELO AKM
extract_elo(AKELOM, "2022-09-01", daterange=90, standardize = T)
# So this gives you the average standardized (between 1 and 0) elo scores of the females in AK from the start of the experiment for three months
# But please check the elo rating tutorial to see what's best for you!

# Extract ELO AKM 4 periods of 3 month
# Extract Elo for different 4-month intervals
AKELOM1 <- extract_elo(AKELOM, "2021-09-01", daterange=120, standardize=T)  # Sep 2021 - Jan 2022
AKELOM2 <- extract_elo(AKELOM, "2022-01-01", daterange=120, standardize=T)  # Jan 2022 - May 2022
AKELOM3 <- extract_elo(AKELOM, "2022-05-01", daterange=120, standardize=T)  # May 2022 - Sep 2022
AKELOM4 <- extract_elo(AKELOM, "2022-09-01", daterange=120, standardize=T)  # Sep 2022 - Jan 2023




extract_elo(AKELOM, "2021-09-01", daterange=120, standardize=T)  # Sep 2021 - Jan 2022
extract_elo(AKELOM, "2022-01-01", daterange=120, standardize=T)  # Jan 2022 - May 2022
extract_elo(AKELOM, "2022-05-01", daterange=120, standardize=T)  # May 2022 - Sep 2022
extract_elo(AKELOM, "2022-09-01", daterange=120, standardize=T)  # Sep 2022 - Jan 2023




