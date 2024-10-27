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
d <- d[d$Date >= "2022-03-01" & d$Date <= "2023-10-01",] # We started in September 22 so I'm using data starting in March (doesn't have to be very precise). We ended in Sept 23 so taking data until Oct 23
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
extract_elo(AKELOF, "2022-09-01", daterange=90, standardize = T)
# So this gives you the average standardized (between 1 and 0) elo scores of the females in AK from the start of the experiment for three months
# But please check the elo rating tutorial to see what's best for you!












## BD
BDELOF <- elo.seq(winner=BDF$winner,loser=BDF$loser, Date=BDF$Date,presence=BDpres,runcheck=F)
# Extract BD Elo
extract_elo(BDELOF, "2022-09-01", daterange=90, standardize = T)
# Eloplot BD
eloplot(BDELOF,ids=c("Obse","Oort","Ouli","Puol","Aapi","Sirk","Miel","Asis","Piep","Skem","Heer","Reen","Oerw","Lewe","Naal","Rede","Hond","Numb","Nooi","Gese","Sari","Riss","Enge","Pann","Nurk","Eina"),from="2021-09-01", to = "2023-10-01")







## NH
NHELOF <- elo.seq(winner=NHF$winner,loser=NHF$loser, Date=NHF$Date,presence=NHpres,runcheck=F)
# Extract BD Elo
extract_elo(NHELOF, "2022-09-01", daterange=90, standardize = T)
# Eloplot BD
eloplot(NHELOF,ids=c("Gran","Guat","Prai","Upps","Gaya","Xala","Pret","Xinp","Gris","Beir","Prat","Regi","Xian","Bela","Raba","Rioj"),from="2021-09-01", to = "2023-10-01")















## MALE
# Male Elo Calculations
# Only select adult females: 
AM <- subset(d, d$AgeClassWinner%in%c("AM"))
AM <- subset(AM, AM$AgeClassLoser%in%c("AM"))

AKM <- subset(AM,AM$Group%in%("Ankhase"))
BDM <- subset(AM,AM$Group%in%("Baie Dankie"))
NHM <- subset(AM,AM$Group%in%("Noha"))




### ANKHASE MALE - AKM
## AK
AKELOM <- elo.seq(winner = AKM$winner, loser=AKM$loser, Date=AKM$Date, presence = AKpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker
# Specific IDs over a specific time period:
print(unique(AKM$winner))

#ELO PLOT - AKM
# FULL AKM ELO (WILL CUT 3 MONTH PER 3 MONTH AND SET PERIOD FROM BORGEAUD AND AL 2017)
eloplot(AKELOM, ids=c("Sho","Vla","Buk"), from="2021-09-01", to = "2023-10-01")

# EXTRACT ELO AKM
extract_elo(AKELOM, "2022-09-01", daterange=90, standardize = T)
# So this gives you the average standardized (between 1 and 0) elo scores of the females in AK from the start of the experiment for three months
# But please check the elo rating tutorial to see what's best for you!


