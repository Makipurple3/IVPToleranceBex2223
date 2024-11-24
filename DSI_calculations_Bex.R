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





## Helper function to standardize dyad names
standardize_dyad <- function(dyad) {
  dyad_parts <- unlist(strsplit(dyad, "_@_"))
  paste(sort(dyad_parts), collapse = "_@_")
}

# Ensure dyads of interest are standardized
dyads_of_interest <- sapply(dyads_of_interest, standardize_dyad)

# Initialize an empty list to store DSI calculations for groups
group_DSI <- list()

# Function to calculate DSI for the entire group and extract dyads
calculate_group_DSI <- function(dsi_data, ot_data, final_date, days, onlyfocaldyads = TRUE, limit2focalnonfocal = TRUE) {
  # Calculate from_date based on final_date and days
  from_date <- as.Date(final_date) - days
  
  cat("Processing group from", from_date, "to", final_date, "\n")
  
  # Filter the DSI data within the date range
  dsi_subset <- dsi_data %>%
    filter(date >= from_date & date <= as.Date(final_date))
  
  # Ensure there is data within the range
  if (nrow(dsi_subset) == 0) {
    cat("No data available for this group and date range.\n")
    return(NULL)
  }
  
  # Calculate DSI
  group_dsi <- DSI(
    b.source = dsi_subset,
    ot.source = ot_data,
    from = as.character(from_date),
    to = as.character(final_date),
    onlyfocaldyads = onlyfocaldyads,
    limit2focalnonfocal = limit2focalnonfocal
  )
  
  # Check if DSI output contains data
  if (nrow(group_dsi) > 0) {
    cat("Number of rows in DSI output:", nrow(group_dsi), "\n")
  } else {
    cat("No data generated for this group and date range.\n")
    return(NULL)
  }
  
  # Standardize dyad names and add date_extraction
  group_dsi <- group_dsi %>%
    dplyr::mutate(
      dyad = sapply(dyad, standardize_dyad),
      date_extraction = as.Date(final_date)
    )
  
  return(group_dsi)
}

# Set parameters for analysis
days_to_analyze <- 90  # Number of days to include in analysis

# Process each group's DSI data for specified date ranges
group_DSI[[1]] <- calculate_group_DSI(dsi_bd, ot_bd, final_date = "2022-09-14", days = days_to_analyze)
group_DSI[[2]] <- calculate_group_DSI(dsi_bd, ot_bd, final_date = "2022-12-12", days = days_to_analyze)
group_DSI[[3]] <- calculate_group_DSI(dsi_ak, ot_ak, final_date = "2022-09-27", days = days_to_analyze)
group_DSI[[4]] <- calculate_group_DSI(dsi_nh, ot_nh, final_date = "2023-03-10", days = days_to_analyze)

# Combine all extracted DSI data into a single table
group_DSI <- group_DSI[!sapply(group_DSI, is.null)]  # Remove NULL elements

if (length(group_DSI) > 0) {
  combined_dsi <- do.call(rbind, group_DSI)
  
  # Standardize dyads again
  combined_dsi <- combined_dsi %>%
    dplyr::mutate(dyad = sapply(dyad, standardize_dyad))
  
  # Aggregate DSI and zDSI by dyad
  aggregated_dsi <- combined_dsi %>%
    dplyr::group_by(dyad) %>%
    dplyr::summarise(
      DSI = mean(DSI, na.rm = TRUE),
      zDSI = mean(zDSI, na.rm = TRUE),
      date_extraction = max(date_extraction, na.rm = TRUE)  # Latest date
    )
  
  # Filter for the dyads of interest
  filtered_dsi <- aggregated_dsi %>%
    dplyr::filter(dyad %in% dyads_of_interest)
  
  # Handle missing dyads
  missing_dyads <- setdiff(dyads_of_interest, unique(filtered_dsi$dyad))
  for (dyad_id in missing_dyads) {
    cat("No data for dyad:", dyad_id, "- assigning default values.\n")
    default_row <- data.frame(
      dyad = dyad_id,
      DSI = 0,
      zDSI = 0,
      date_extraction = NA
    )
    filtered_dsi <- rbind(filtered_dsi, default_row)
  }
  
  # Save the final table
  BexZDSI <- filtered_dsi
  print(BexZDSI)
} else {
  cat("No DSI data processed.\n")
}
