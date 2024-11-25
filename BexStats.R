


# Import DSI ELO and Bex For Maerge




BexDSI <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexDSI.csv")

BexElo <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexElo.csv")

DyadSummary <- read.csv("/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/dyad_summary_clean.csv")

BexStat <-read.csv("//Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexStat.csv")




print(BexDSI)
print(BexElo)
print(DyadSummary)
head(BexStat)



colnames(BexDSI)
colnames(BexElo)
colnames(DyadSummary)
colnames(BexStat)


#Variables to select for 
#BexFinal <- Dyad, MeloQ, FEloQ, ZMFEloQ (rename to IELO), IzDSI, FzDSI, TrialDay, DyadDay, Trial, Month, Date, DyadResponse, Tol, Agg, NotApp, Intrud


# Step 1: Standardize column names
colnames(BexDSI)[colnames(BexDSI) == "dyad"] <- "Dyad"

# Step 2: Convert `Dyad` in BexElo to character
BexElo$Dyad <- as.character(BexElo$Dyad)

# Step 3: Select relevant variables from BexStat
BexStat_selected <- BexStat %>%
  select(Dyad, TrialDay, DyadDay, Trial, Date, DyadResponse, Tol, Agg, NotApp, Intrud)

# Step 4: Merge datasets by Dyad
# Merge BexDSI
BexFinal <- BexStat_selected %>%
  left_join(BexDSI %>% select(Dyad, IzDSI, FzDSI), by = "Dyad")

# Merge BexElo
BexFinal <- BexFinal %>%
  left_join(BexElo %>% select(Dyad, MEloQ, FEloQ, ZMFEloQ), by = "Dyad")

# Merge DyadSummary
BexFinal <- BexFinal %>%
  left_join(DyadSummary %>% select(Dyad, AgeDiff, AgeDir), by = "Dyad")

# Step 5: Rename ZMFEloQ to IELO
colnames(BexFinal)[colnames(BexFinal) == "ZMFEloQ"] <- "IzELO"

# Step 6: Retain only the required variables in BexFinal
BexFinal <- BexFinal %>%
  select(Dyad, MEloQ, FEloQ, IzELO, IzDSI, FzDSI, TrialDay, DyadDay, Trial, Date, 
         DyadResponse, Tol, Agg, NotApp, Intrud)

# Step 7: Verify variables in BexFinal
cat("Column names in BexFinal:\n")
print(colnames(BexFinal))

cat("\nPreview of the merged dataset BexFinal:\n")
print(head(BexFinal))

# Step 8: Check for any missing values in the dataset
cat("\nSummary of missing values in BexFinal:\n")
print(colSums(is.na(BexFinal)))




str(BexFinal)
View(BexFinal)



# Step 9: Export BexFinal to a CSV file
output_path <- "/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexFinal.csv"
write.csv(BexFinal, output_path, row.names = FALSE)

cat("\nBexFinal has been successfully exported to:", output_path, "\n")







# RESEARCH QUESTION
#Variations in Age, Rank, IDSI, and IEloDiff and time spent together (amount of trials on a set period/in general) can explain differneces in tolerance (aggression/not approaching?!) rates in a forced proximity box experiment conducted in wild vervet monkeys
#The effects may be moderated by /> Seasonality, Male tenure, Female pregnancy

#>Lower variations in age and rank and higher inital social bonds and overall amount of time spent otgether leads to higher rates of tolerance





# STATISTICAL TEST  
#glmer
#{lmerTest} / pTol ~ AmountTrial (totalNtrial  vs fixed amount each dyad) + AgeDiff + IEloDiff + IDsi +(Day| Dyad)



