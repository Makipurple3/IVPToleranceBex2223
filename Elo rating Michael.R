# Elo Michael
# FINALLY : ELO CALCULATIONS FROM PREVIOUS DATASETS CREATED
# UPDATE FROM HERE



dev.off()  # Close any open graphics devices
dev.new()  # Open a new graphics window
par(mfrow = c(1, 1))  # Single plot layout
par(mar = c(5, 4, 4, 2) + 0.1)  # Default margins




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


# In general, the more data you have the more accurate scores should be
# However, its worth reading Borgeaud et al., 2017: The influence of demographic variation 
# on social network stability in wild vervet monkeys. Here they discuss stability of male and female hierarchies
# on which you can base how much data you need to get an accurate estimate of your individuals.
# According to the article male stability fluctuates more , reason why it may be relevant to calculat Elo fluctuations 
# 3 month to 3 months. and ideally on min 6 month to a year

# In your case, filter from 6 months before the first experiment until the end of the experiment.

# Finally I choose to use 6 month since it takes a few month for elo to stabilize 

# The elo rating function has a built in function to calculate the elo on a given date, so best to keep everything.
# You filter the same dates for the presence file.


# Filter date 
d <- d[d$Date >= "2022-01-20" & d$Date <= "2023-09-13",] # We started in September 22 so I'm using data starting in March (doesn't have to be very precise). We ended in Sept 23 so taking data until Oct 23
# For better reliability I will choose data from a year before experiment and because some presence data end in spetember choose last date of experiment
# as end date (In the code you gave me I replaced 2023-09-13 with 2023-09-13)
AKpres <- AKpres[AKpres$Date >= "2022-01-20" & AKpres$Date <= "2023-09-13",] # Do the same for presence
BDpres <- BDpres[BDpres$Date >= "2022-01-20" & BDpres$Date <= "2023-09-13",]
NHpres <- NHpres[NHpres$Date >= "2022-01-20" & NHpres$Date <= "2023-09-13",]

# Check dates
range(d$Date)
range(AKpres$Date)
range(BDpres$Date)
range(NHpres$Date)
# Now all date ranges are the same




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
eloplot(AKELOF, ids=c("Ginq","Godu","Gubh", "Ndaw", "Nkos", "Ghid", "Ndon", "Ncok"), from="2022-01-20", to = "2023-09-13")

# to get an average of the eloscore over a period 
# It counts from the date you give forwards with the amount of days in daterange
# However, the last date possible is the last day there was an agonistic interaction recorded in that group


# Extract Elo scores for a specific date using 365 days 
extract_elo(AKELOF, "2022-09-01", daterange = 365, standardize = T)
# So this gives you the average standardized (between 1 and 0) elo scores of the females in AK from the start of the experiment for three months
# But please check the elo rating tutorial to see what's best for you!



AKELOF2 <-extract_elo(AKELOF, extractdate = "2022-09-15", standardize=T)
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
eloplot(BDELOF,ids=c("Obse","Oort","Ouli","Puol","Aapi","Sirk","Miel","Asis","Piep","Skem","Heer","Reen","Oerw","Lewe","Naal","Rede","Hond","Numb","Nooi","Gese","Sari","Riss","Enge","Pann","Nurk","Eina"),from="2022-01-20", to = "2023-09-13")



# BD Group Elo Calculation and Quartile Analysis
BDELOF2 <- extract_elo(BDELOF, extractdate = "2022-09-15", standardize=T)
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




## BDELOF2 - Kom Oort, 2022-12-11 

## BD
BDELOF3 <- elo.seq(winner=BDF$winner,loser=BDF$loser, Date=BDF$Date,presence=BDpres,runcheck=F)
# Extract BD Elo
extract_elo(BDELOF3, "2022-12-11", daterange=90, standardize = T)
# Eloplot BD
eloplot(BDELOF3,ids=c("Obse","Oort","Ouli","Puol","Aapi","Sirk","Miel","Asis","Piep","Skem","Heer","Reen","Oerw","Lewe","Naal","Rede","Hond","Numb","Nooi","Gese","Sari","Riss","Enge","Pann","Nurk","Eina"),from="2022-12-11", to = "2023-09-13")



# BD Group Elo Calculation and Quartile Analysis
BDELOF4 <- extract_elo(BDELOF3, extractdate = "2022-12-11", standardize=T)
# Print the extracted Elo scores for BD
print(BDELOF4)
# Calculate quartiles for the Elo scores of females in BD
BD_quartiles <- quantile(BDELOF4, probs = c(0.25, 0.5, 0.75))
# Assign each individual in BDELOF2 to a quartile (ensure 1st Quartile is highest rank)
BDFQ3 <- cut(BDELOF4,
            breaks = c(-Inf, BD_quartiles, Inf),
            labels = c("4th Quartile", "3rd Quartile", "2nd Quartile", "1st Quartile"),
            include.lowest = TRUE)
# Create a dataframe to hold individual names, Elo scores, and their respective quartiles
BDFQ4 <- data.frame(Individual = names(BDELOF4), Elo_Score = BDELOF4, Quartile = BDFQ3)
# Print the results
print(BDFQ4)
# Plot the boxplot for BD Females
par(mar = c(6, 6, 4, 2) + 0.1)  # Increase margins for readability

boxplot(BDELOF4,
        main = "Boxplot of Elo Scores for BD Females",
        ylab = "Elo Scores",
        col = "#87CEEB",
        border = "darkblue",
        notch = FALSE,    # Disable notches to avoid warnings and visual clutter
        cex.main = 1.5,   # Increase the size of the title
        cex.lab = 1.2,    # Increase the size of the axis labels
        cex.axis = 1.1)   # Increase the size of the axis numbers

# Adding individual labels to visualize each individual's score within the quartiles
text(x = 1.2, y = BDELOF4, labels = names(BDELOF4), cex = 0.9, col = "black", pos = 4)
# Adding grid lines for better visualization
grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")
# Adding points to make individual scores stand out
points(rep(1, length(BDELOF4)), BDELOF4, pch = 19, col = "red")




str(BDELOF2)
str(BDELOF4)









## NH
NHELOF <- elo.seq(winner=NHF$winner,loser=NHF$loser, Date=NHF$Date,presence=NHpres,runcheck=F)
# Extract BD Elo
extract_elo(NHELOF, "2023-03-09", daterange=365, standardize = T)
# Eloplot BD
eloplot(NHELOF,ids=c("Gran","Guat","Prai","Upps","Gaya","Xala","Pret","Xinp","Gris","Beir","Prat","Regi","Xian","Bela","Raba","Rioj"),from="2022-01-20", to = "2023-09-13")


# NH Group Elo Calculation and Quartile Analysis
NHELOM2 <- extract_elo(NHELOF, extractdate = "2023-03-09", standardize=T)
# Print the extracted Elo scores for NH
print(NHELOF2)
sum(is.na(NHELOM2))  # Count NA values
sum(is.nan(NHELOM2)) # Count NaN values


# Calculate quartiles for the Elo scores of females in NH
quantile(NHELOM2, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
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
range(AKpres$Date)
range(BDpres$Date)
range(NHpres$Date)

colnames(BDpres)
colnames(NHpres)





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

# Example: Remove unwanted individuals from AKM (IDs that should not be present)
unwanted_ids_akm <- c("Tch", "Yan")  # Add more if necessary
AKM <- AKM[!AKM$winner %in% unwanted_ids_akm & !AKM$loser %in% unwanted_ids_akm, ]

unwanted_ids_bdm <- c("Pro", "Pal", "Mat", "Ted", "Dok", "Bra", "Win", "Pom")
BDM <- BDM[!BDM$winner %in% unwanted_ids_bdm & !BDM$loser %in% unwanted_ids_bdm, ]

unwanted_ids_nhm <- c("War", "Sio")
NHM <- NHM[!NHM$winner %in% unwanted_ids_nhm & !NHM$loser %in% unwanted_ids_nhm, ]

# Seqcheck
# Run Sequence Check Before Elo Calculation (Males)
seqcheck(winner = AKM$winner, loser = AKM$loser, Date = AKM$Date, draw = NULL, presence = AKpres)
seqcheck(winner = BDM$winner, loser = BDM$loser, Date = BDM$Date, draw = NULL, presence = BDpres)
seqcheck(winner = NHM$winner, loser = NHM$loser, Date = NHM$Date, draw = NULL, presence = NHpres)





# RUN ELO
# Calculate Elo ratings for AK males
AKELOM <- elo.seq(winner = AKM$winner, loser = AKM$loser, Date = AKM$Date, presence = AKpres, runcheck = F)
# Calculate Elo ratings for BD males
BDELOM <- elo.seq(winner = BDM$winner, loser = BDM$loser, Date = BDM$Date, presence = BDpres, runcheck = F)
# Calculate Elo ratings for NH males
NHELOM <- elo.seq(winner = NHM$winner, loser = NHM$loser, Date = NHM$Date, presence = NHpres, runcheck = F)





#UPDATES FROM HERE, CHECK I WANTED RISE/DECLINE ELO AND SET DATE AND ADAPT FOR FEMALE
#AK
# For set dateAK
extract_elo(AKELOM, extractdate = "2022-09-15", standardize=T)


#Extract quartile and boxplot AK
AKELOM2 <-extract_elo(AKELOM, extractdate = "2022-09-15", standardize=T)
# TEST FOR QUARTILE
# Print the extracted Elo scores to confirm extraction
print(AKELOM2)
# Calculate quartiles for the Elo scores of females in AK
AK_quartiles <- quantile(AKELOM2, probs = c(0.25, 0.5, 0.75))
# Assign each individual in AKELOF2 to a quartile (ensure 1st Quartile is highest rank)
AKMQ <- cut(AKELOM2,
            breaks = c(-Inf, AK_quartiles, Inf),
            labels = c("4th Quartile", "3rd Quartile", "2nd Quartile", "1st Quartile"),  # Reversed labels for correct assignment
            include.lowest = TRUE)
# Create a dataframe to hold individual names, Elo scores, and their respective quartiles
AKMQ2 <- data.frame(Individual = names(AKELOM2), Elo_Score = AKELOM2, Quartile = AKMQ)
# Print the results
print(AKMQ2)
# Plot the boxplot for AK Females
par(mar = c(6, 6, 4, 2) + 0.1)  # Increase margins for readability
boxplot(AKELOM2,
        main = "Boxplot of Elo Scores for AK Males",
        ylab = "Elo Scores",
        col = "#87CEEB",
        border = "darkblue",
        notch = FALSE,    # Disable notches to avoid warnings and visual clutter
        cex.main = 1.5,   # Increase the size of the title
        cex.lab = 1.2,    # Increase the size of the axis labels
        cex.axis = 1.1)   # Increase the size of the axis numbers

# Adding individual labels to visualize each individual's score within the quartiles
text(x = 1.2, y = AKELOM2, labels = names(AKELOM2), cex = 0.9, col = "black", pos = 4)
# Adding grid lines for better visualization
grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")
# Adding points to make individual scores stand out
points(rep(1, length(AKELOM2)), AKELOM2, pch = 19, col = "red")





#BD

# For set dateBD 1 , BDELOM"
extract_elo(BDELOM, extractdate = "2022-09-15", standardize=T)

#Extract quartile and boxplot BD
BDELOM2 <-extract_elo(BDELOM, extractdate = "2022-09-15", standardize=T)
# TEST FOR QUARTILE
# Print the extracted Elo scores to confirm extraction
print(BDELOM2)
# Calculate quartiles for the Elo scores of females in AK
BD_quartiles <- quantile(BDELOM2, probs = c(0.25, 0.5, 0.75))
# Assign each individual in AKELOF2 to a quartile (ensure 1st Quartile is highest rank)
BDMQ <- cut(BDELOM2,
            breaks = c(-Inf, BD_quartiles, Inf),
            labels = c("4th Quartile", "3rd Quartile", "2nd Quartile", "1st Quartile"),  # Reversed labels for correct assignment
            include.lowest = TRUE)
# Create a dataframe to hold individual names, Elo scores, and their respective quartiles
BDMQ2 <- data.frame(Individual = names(BDELOM2), Elo_Score = BDELOM2, Quartile = BDMQ)
# Print the results
print(BDMQ2)
# Plot the boxplot for AK Females
par(mar = c(6, 6, 4, 2) + 0.1)  # Increase margins for readability
boxplot(BDELOM2,
        main = "Boxplot of Elo Scores for BD Males",
        ylab = "Elo Scores",
        col = "#87CEEB",
        border = "blue",
        notch = FALSE,    # Disable notches to avoid warnings and visual clutter
        cex.main = 1.5,   # Increase the size of the title
        cex.lab = 1.2,    # Increase the size of the axis labels
        cex.axis = 1.1)   # Increase the size of the axis numbers

# Adding individual labels to visualize each individual's score within the quartiles
text(x = 1.2, y = BDELOM2, labels = names(BDELOM2), cex = 0.9, col = "black", pos = 4)
# Adding grid lines for better visualization
grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")
# Adding points to make individual scores stand out
points(rep(1, length(BDELOM2)), BDELOM2, pch = 19, col = "red")



# BD2



# # For set dateBD2 ( Kom Oort) / BDELOM3
extract_elo(BDELOM, extractdate = "2022-12-11", standardize=T)

#Extract quartile and boxplot BD
BDELOM3 <-extract_elo(BDELOM, extractdate = "2022-12-11", standardize=T)
# TEST FOR QUARTILE
# Print the extracted Elo scores to confirm extraction
print(BDELOM3)
# Calculate quartiles for the Elo scores of females in AK
BD_quartiles <- quantile(BDELOM3, probs = c(0.25, 0.5, 0.75))
# Assign each individual in AKELOF2 to a quartile (ensure 1st Quartile is highest rank)
BDMQ3 <- cut(BDELOM3,
            breaks = c(-Inf, BD_quartiles, Inf),
            labels = c("4th Quartile", "3rd Quartile", "2nd Quartile", "1st Quartile"),  # Reversed labels for correct assignment
            include.lowest = TRUE)
# Create a dataframe to hold individual names, Elo scores, and their respective quartiles
BDMQ4 <- data.frame(Individual = names(BDELOM3), Elo_Score = BDELOM3, Quartile = BDMQ3)
# Print the results
print(BDMQ4)
# Plot the boxplot for AK Females
par(mar = c(6, 6, 4, 2) + 0.1)  # Increase margins for readability
boxplot(BDELOM3,
        main = "Boxplot of Elo Scores for BD2 Males",
        ylab = "Elo Scores",
        col = "#87CEEB",
        border = "darkblue",
        notch = FALSE,    # Disable notches to avoid warnings and visual clutter
        cex.main = 1.5,   # Increase the size of the title
        cex.lab = 1.2,    # Increase the size of the axis labels
        cex.axis = 1.1)   # Increase the size of the axis numbers

# Adding individual labels to visualize each individual's score within the quartiles
text(x = 1.2, y = BDELOM3, labels = names(BDELOM2), cex = 0.9, col = "black", pos = 4)
# Adding grid lines for better visualization
grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")
# Adding points to make individual scores stand out
points(rep(1, length(BDELOM3)), BDELOM3, pch = 19, col = "red")






#NH

# For set date NH
extract_elo(NHELOM, extractdate = "2023-03-09", standardize=T)
extract_elo(NHELOM, extractdate = "2023-03-13" , standardize=T)

#Extract quartile anx boxplot NH
NHELOM2 <-extract_elo(NHELOM, extractdate = "2023-03-09", standardize=T)
# TEST FOR QUARTILE
# Print the extracted Elo scores to confirm extraction
print(NHELOM2)
str(NHM)
colnames(NHM)
# Calculate quartiles for the Elo scores of females in AK
# Extract quartiles for the Elo scores
NHELOM2 <- NHELOM2[!is.na(NHELOM2)]  # Remove NA values if any
NH_quartiles <- quantile(NHELOM2, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Print the quartiles for debugging
print(NH_quartiles)

# Cut Elo scores into quartiles
NHMQ <- cut(
  NHELOM2,
  breaks = c(-Inf, NH_quartiles, Inf),  # Ensure the correct number of breaks
  labels = c("4th Quartile", "3rd Quartile", "2nd Quartile", "1st Quartile"),  # Labels for each quartile
  include.lowest = TRUE
)

# Proceed to create the data frame and plot as planned
NHMQ2 <- data.frame(Individual = names(NHELOM2), Elo_Score = NHELOM2, Quartile = NHMQ)
print(NHMQ2)

# Print the results
print(NHMQ2)
# Plot the boxplot for AK Females
par(mar = c(6, 6, 4, 2) + 0.1)  # Increase margins for readability
boxplot(NHELOM2,
        main = "Boxplot of Elo Scores for NH Males",
        ylab = "Elo Scores",
        col = "#87CEEB",
        border = "darkblue",
        notch = FALSE,    # Disable notches to avoid warnings and visual clutter
        cex.main = 1.5,   # Increase the size of the title
        cex.lab = 1.2,    # Increase the size of the axis labels
        cex.axis = 1.1)   # Increase the size of the axis numbers

# Adding individual labels to visualize each individual's score within the quartiles
text(x = 1.2, y = NHELOM2, labels = names(NHELOM2), cex = 0.9, col = "black", pos = 4)
# Adding grid lines for better visualization
grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")
# Adding points to make individual scores stand out
points(rep(1, length(NHELOM2)), NHELOM2, pch = 19, col = "red")





# Extract Elo from 1st September 2022 for the previous 365 days 
# modify functions that counts forward not backwards
extract_elo(AKELOM, "2022-09-02", daterange=345, standardize = T)
extract_elo(BDELOM, "2022-09-02", daterange=345, standardize = T)
extract_elo(NHELOM, "2022-09-02", daterange=345, standardize = T)
# I did not have data on 365days like for female so reduced it to 345 days
# So this gives you the average standardized (between 1 and 0) elo scores of the females in AK from the start of the experiment for three months
# But please check the elo rating tutorial to see what's best for you!




# Elo TABLE

#I wil want a df with the coluwing columns Dyad, Male, Female, MEloQ, FEloQ, ZMFEloQ, EloDate
#I want to extract the elo value and quartile information in

#AK
#AKMQ2 for Buk, Sho, date is 2022-15-09
#AKFQ2 for Ginq, Buk, date is 2022-15-09

#BD
#BDMQ2 for Sey, Xia, Nge, Xin, date us 2022-15-09
#BDMF2 for Sirk, Piep, Oerw, Ouli, date is 2022-09-15

#BD2
#BDMQ4 for Kom, date is 2022-12-11
#BDFQ4 fo Oort, date is 2022-12-11

#NH
#NHMQ2 for Pom, date is 2023-03-09
#HFQ2 for Xian, date is 2023-03-09







print(AKMQ2)
print(AKFQ2)
print(BDMQ2)
print(BDMQ4)
print(BDFQ2)
print(BDFQ4)
print(NHMQ2)
print(NHFQ2)

str(dyads)
print(dyads)


# Create an empty data frame for the output
BexElo <- data.frame(
  Dyad = character(),
  Male = character(),
  Female = character(),
  MEloQ = character(),
  FEloQ = character(),
  ZMFEloQ = numeric(),
  MElo = numeric(),
  FElo = numeric(),
  EloDate = character(),
  stringsAsFactors = FALSE
)

# Example: Assign Elo datasets (AKMQ2, AKFQ2, etc.) as a named list
elo_datasets <- list(
  "AKM" = AKMQ2,
  "AKF" = AKFQ2,
  "BDM" = BDMQ2,
  "BDF" = BDFQ2,
  "NHM" = NHMQ2,
  "NHF" = NHFQ2
)

# Assuming `dyads` contains dyad names like "Buk Ndaw", "Kom Oort", etc.
for (dyad in dyads) {
  # Split dyad into male and female names
  individuals <- unlist(strsplit(dyad, " "))
  male <- individuals[1]
  female <- individuals[2]
  
  # Look up male and female information in the respective datasets
  male_data <- do.call(rbind, lapply(elo_datasets[c("AKM", "BDM", "NHM")], function(df) df[df$Individual == male, ]))
  female_data <- do.call(rbind, lapply(elo_datasets[c("AKF", "BDF", "NHF")], function(df) df[df$Individual == female, ]))
  
  # Ensure both male and female data are found
  if (nrow(male_data) > 0 & nrow(female_data) > 0) {
    MElo <- male_data$Elo_Score[1]
    FElo <- female_data$Elo_Score[1]
    MEloQ <- male_data$Quartile[1]
    FEloQ <- female_data$Quartile[1]
    
    # Calculate absolute difference in quartiles
    ZMFEloQ <- abs(as.numeric(gsub("\\D", "", MEloQ)) - as.numeric(gsub("\\D", "", FEloQ)))
    
    # Append to the output data frame
    BexElo <- rbind(BexElo, data.frame(
      Dyad = dyad,
      Male = male,
      Female = female,
      MEloQ = MEloQ,
      FEloQ = FEloQ,
      ZMFEloQ = ZMFEloQ,
      MElo = MElo,
      FElo = FElo,
      EloDate = Sys.Date(), # Replace with actual date if available
      stringsAsFactors = FALSE
    ))
  } else {
    warning(paste("Missing data for dyad:", dyad))
  }
}

# Remove duplicates, if any
BexElo <- BexElo[!duplicated(BexElo), ]

# Print the resulting data frame
print(BexElo)

# Export the data frame to CSV
output_path <- "//Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexElo.csv"
write.csv(BexElo, file = output_path, row.names = FALSE)

# Print confirmation message
cat("CSV file successfully saved at:", output_path, "\n")
