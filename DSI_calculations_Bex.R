#Install package of social indices: socialindices_0.28.tar.gz
# Replace the path with the actual path to the script
source("/Users/maki/Desktop/Master Thesis/Josies Codes/socialindices/R/DSI.R")
source("/Users/maki/Desktop/Master Thesis/Josies Codes/socialindices/R/varname.R")
source("/Users/maki/Desktop/Master Thesis/Josies Codes/socialindices/R/mergeactrec.R")
source("/Users/maki/Desktop/Master Thesis/Josies Codes/socialindices/R/makedyads.R")
source("/Users/maki/Desktop/Master Thesis/Josies Codes/socialindices/R/createnulldata.R")
source("/Users/maki/Desktop/Master Thesis/Josies Codes/socialindices/R/dot.R")
source("/Users/maki/Desktop/Master Thesis/Josies Codes/socialindices/R/checkpresence.R")
source("/Users/maki/Desktop/Master Thesis/Josies Codes/socialindices/R/checkbehaviour.R")



library(dplyr)

# Calculate monthly DSI scores based on DSI file, OT and presence
# Read all files
# Work PC:
dsi <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/File_DSI.csv")
OT <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/OT.csv")
pres_ak <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/pres_ak.csv")
pres_bd <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/pres_bd.csv")
pres_nh <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/pres_nh.csv")


# Properly name columns
# date focal actor receiver beh dur
#In dsi
dsi <- dsi %>% 
  rename(date = Date, focal = Focal, actor = Actor, receiver = Receiver, beh = BehaviourFocal, dur = Duration)
#In OT
OT <- OT %>%
  rename(focal = Focal, date = Date)



# Convert date format from chr to Date YYYY-MM-DD
# dsi, OT, pres_ak, pres_bd, pres_nh Date

dsi$date <- as.Date(dsi$date, format = "%Y-%m-%d")
OT$date <- as.Date(OT$date, format = "%Y-%m-%d")
pres_ak$date <- as.Date(pres_ak$date, format = "%Y-%m-%d")
pres_bd$date <- as.Date(pres_bd$date, format = "%Y-%m-%d")
pres_nh$date <- as.Date(pres_nh$date, format = "%Y-%m-%d")

# Check date range of dsi, OT, pres_ak/bd/nh
range(dsi$date)
range(OT$date)
range(pres_ak$date)
range(pres_bd$date)
range(pres_nh$date)
  # All date ranges start the 3rd of jan 2022 except for OT that starts in june 2nd
  # I start data 2022-06-02 and finish it on 





# Select only applicable behaviours
dsi <- dsi %>%
  filter(beh %in% c("Groom", "Groom.focal", "Approach", "Contact", "Contact.focal", "Two", "Five"))

# Extract the column names from each presence matrix except for the first one (Date)
individuals_ak <- colnames(pres_ak)[-1]
individuals_bd <- colnames(pres_bd)[-1]
individuals_nh <- colnames(pres_nh)[-1]

# Now create separate files for ak, bd and nh
dsi_ak <- dsi %>% 
  filter(Group == "Ankhase",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(!actor %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         !receiver %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         actor %in% individuals_ak,
         receiver %in% individuals_ak,
         focal %in% individuals_ak)
dsi_bd <- dsi %>% 
  filter(Group == "Baie Dankie",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(!actor %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         !receiver %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         actor %in% individuals_bd,
         receiver %in% individuals_bd,
         focal %in% individuals_bd)
dsi_nh <- dsi %>% 
  filter(Group == "Noha",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(!actor %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         !receiver %in% c("Unk", "UnkAF", "UnkAM", "UnkA", "UnkJ", "UnkJF", "UnkJM", "UnkBB"),
         actor %in% individuals_nh,
         receiver %in% individuals_nh,
         focal %in% individuals_nh)

# Create separate files for ak, bd and nh of OT
ot_ak <- OT %>% 
  filter(Group == "Ankhase",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         focal = ifelse(focal == "GIl", "Gil", focal))
ot_bd <- OT %>% 
  filter(Group == "Baie Dankie",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))
ot_nh <- OT %>% 
  filter(Group == "Noha",
         date < "2023-11-20",
         date > "2021-06-02") %>%
  dplyr::select(!Group) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))


# AK PRE DSI CHECK
range(ot_ak$date)
range(dsi_ak$date)
# Check column names in dsi_ak
colnames(dsi_ak)
# Check column names in ot_ak
colnames(ot_ak)
checkpresence(presence=pres_ak, b.source=dsi_ak)


# BD PRE DSI CHECK
range(ot_bd$date)
range(dsi_bd$date)
# Check column names in dsi_ak
colnames(dsi_bd)
# Check column names in ot_ak
colnames(ot_bd)
checkpresence(presence=pres_bd, b.source=dsi_bd)


# NH PRE DSI CHECK
range(ot_nh$date)
range(dsi_nh$date)
# Check column names in dsi_ak
colnames(dsi_nh)
# Check column names in ot_ak
colnames(ot_nh)
checkpresence(presence=pres_nh, b.source=dsi_nh)












ak <- DSI(dsi_ak, ot.source = ot_ak, duration.NA.treatm = "count", onlyfocaldyads = T, limit2focalnonfocal = F)
bd <- DSI(dsi_bd, ot.source = ot_bd, duration.NA.treatm = "count", onlyfocaldyads = T, limit2focalnonfocal = F)
nh <- DSI(dsi_nh, ot.source = ot_nh, duration.NA.treatm = "count", onlyfocaldyads = T, limit2focalnonfocal = F)





# DSI Calculations with set dates for each groups depending of dyads first day of experiment
# Dates and dyads

#DYAD		FIRST EXPERIMENT DAY
#BD1		>	SEPT 2022
##Sey Sirk		14.09.2022
##Xia Piep		16.09.2022
##Nge Oerw		22.09.2022
##Xin Ouli		27.09.2022

#AK		>	SEPT 2022
##Sho Ginq		27.09.2022
##Buk Ndaw		29.09.2022

#BD2		>	DEC 2022	
##Kom Oort		12.12.2022

#NH		>	MAR 2023
##Pom Xian		10.03.2023


# For each dyad take data from 3 month before to first day of experiments to have same amount of data. First common data is 2022-06-02







# DSI Dates for AK
# AKShoGinq <- from 2022-06-27 to 2022-09-27
# AKBukNdaw <- from 2022-06-29 to 2022-09-29

# DSI Dates for BD
# BDSeySirk <- from 2022-06-14 to 2022-09-14
# BDXiaPiep <- from 2022-06-16 to 2022-09-16
# BDNgeOerw <- from 2022-06-22 to 2022-09-22
# BDXinOuli <- from 2022-06-27 to 2022-09-27
# BDKomOort <- from 2022-09-12 to 2022-12-12

# NHPomXian <- fromt 2023-01-09 to 2023-03-09


# Define dyads of interest
dyads_of_interest <- c("Buk_@_Ndaw", "Ginq_@_Sho", "Kom_@_Oort", "Nge_@_Oerw", "Ouli_@_Xin", "Piep_@_Xia", "Pom_@_Xian", "Sey_@_Sirk")

# Helper function to standardize dyads
standardize_dyad <- function(dyad) {
  dyad_parts <- unlist(strsplit(dyad, "_@_"))
  paste(sort(dyad_parts), collapse = "_@_")
}
dyads_of_interest <- sapply(dyads_of_interest, standardize_dyad)

# Define date ranges for each group
date_ranges <- list(
  AK = c("2022-06-15", "2022-09-15"),
  BD = c("2022-06-15", "2022-09-15"),
  BD2 = c("2022-09-11", "2022-12-11"),
  NH = c("2022-12-09", "2023-03-09")
)

# Helper function to calculate DSI
calculate_dsi <- function(dsi_data, ot_data, from_date, to_date) {
  dsi_subset <- dsi_data %>%
    filter(date >= as.Date(from_date) & date <= as.Date(to_date))
  
  # Ensure subset has data
  if (nrow(dsi_subset) == 0) {
    cat("No data for date range:", from_date, "-", to_date, "\n")
    return(NULL)
  }
  
  # Run DSI calculation
  tryCatch(
    {
      dsi_output <- DSI(
        b.source = dsi_subset,
        ot.source = ot_data,
        from = from_date,
        to = to_date,
        onlyfocaldyads = TRUE,
        limit2focalnonfocal = TRUE
      )
      
      # Ensure `date_extraction` column exists
      if (!"date_extraction" %in% colnames(dsi_output)) {
        dsi_output$date_extraction <- as.Date(to_date)
      }
      
      dsi_output
    },
    error = function(e) {
      cat("Error in DSI calculation:", e$message, "\n")
      return(NULL)
    }
  )
}

# Run DSI for all groups
dsi_results <- list(
  AK = calculate_dsi(dsi_ak, ot_ak, date_ranges$AK[1], date_ranges$AK[2]),
  BD = calculate_dsi(dsi_bd, ot_bd, date_ranges$BD[1], date_ranges$BD[2]),
  BD2 = calculate_dsi(dsi_bd, ot_bd, date_ranges$BD2[1], date_ranges$BD2[2]),
  NH = calculate_dsi(dsi_nh, ot_nh, date_ranges$NH[1], date_ranges$NH[2])
)

# Filter DSI results for dyads of interest
filter_dsi <- function(dsi_data) {
  if (is.null(dsi_data) || nrow(dsi_data) == 0) {
    return(data.frame(dyad = dyads_of_interest, IDSI = 0, IzDSI = 0, date_extraction = NA))
  }
  dsi_data %>%
    mutate(dyad = sapply(dyad, standardize_dyad)) %>%
    filter(dyad %in% dyads_of_interest) %>%
    select(i1, i2, dyad, DSI, zDSI, date_extraction) %>%
    rename(IDSI = DSI, IzDSI = zDSI)
}

filtered_dsi <- list(
  AK = filter_dsi(dsi_results$AK),
  BD = filter_dsi(dsi_results$BD),
  BD2 = filter_dsi(dsi_results$BD2),
  NH = filter_dsi(dsi_results$NH)
)

# Combine all filtered results
BexDSI <- do.call(rbind, filtered_dsi)

# Handle missing dyads
missing_dyads <- setdiff(dyads_of_interest, unique(BexDSI$dyad))
for (dyad_id in missing_dyads) {
  default_row <- data.frame(
    i1 = NA,
    i2 = NA,
    dyad = dyad_id,
    IDSI = 0,
    IzDSI = 0,
    date_extraction = NA
  )
  BexDSI <- rbind(BexDSI, default_row)
}

# Display final table
print(BexDSI)

# Add FDSI and FzDSI columns
BexDSI <- BexDSI %>%
  mutate(
    Male = sapply(strsplit(dyad, "_@_"), function(x) ifelse(nchar(x[1]) == 3, x[1], x[2])),
    Female = sapply(strsplit(dyad, "_@_"), function(x) ifelse(nchar(x[1]) == 4, x[1], x[2])),
    dyad = paste(Male, Female), # Update dyad column to be "Male Female"
    FDSI = IDSI, # Assuming FDSI takes the same value as IDSI
    FzDSI = IzDSI # Assuming FzDSI takes the same value as IzDSI
  ) %>%
  select(dyad, Male, Female, IDSI, IzDSI, FDSI, FzDSI, date_extraction) # Reorder columns

# Reset row names to ensure no `AK.1`, `BD.1`, etc.
rownames(BexDSI) <- NULL

# Set FDSI and FzDSI for the final date `2023-09-13`
BexDSI <- BexDSI %>%
  mutate(
    FDSI = ifelse(date_extraction == as.Date("2023-09-13"), FDSI, FDSI),
    FzDSI = ifelse(date_extraction == as.Date("2023-09-13"), FzDSI, FzDSI)
  )

## Remove rows 9, 10, 11, and 12
BexDSI <- BexDSI[-c(3, 9, 10, 11, 12), ]

# Add the final extraction date as a new column
BexDSI <- BexDSI %>%
  mutate(date_extraction_final = as.Date("2023-09-13"))

# Display the final table
print(BexDSI)



write.csv(BexDSI, file = "/Users/maki/Desktop/Master Thesis/BEX 2223 Master Thesis Maung Kyaw/IVPToleranceBex2223/BexDSI.csv", row.names = FALSE)



