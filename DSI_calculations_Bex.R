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


ls()
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


range(ot_ak$date)
range(dsi_ak$date)
# Check column names in dsi_ak
colnames(dsi_ak)

# Check column names in ot_ak
colnames(ot_ak)


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


# Calculate DSI for dyads between specified dates
AKShoGinq <- DSI(dsi_ak, from = "2022-06-27", to = "2022-09-27", ot.source = ot_ak)
# Extract dyad identifiers and their DSI values
dyads_DSI <- AKShoGinq %>%
  dplyr::select(i1, i2, dyad, DSI, zDSI)
# Sort the dyads based on zDSI values in descending order and rank them
sorted_dyads_DSI <- dyads_DSI %>%
  filter(!is.na(zDSI)) %>%  # Remove rows with NA zDSI values
  arrange(desc(zDSI)) %>%
  mutate(rank = row_number())  # Add rank based on zDSI values

# Define the dyad identifier for "Ginq Sho"
ginq_sho_dyad <- "Ginq_@_Sho"
# Extract the rank of "Ginq Sho" based on zDSI values
rank_ginq_sho <- sorted_dyads_DSI %>%
  filter(dyad == ginq_sho_dyad) %>%
  select(dyad, rank, zDSI)

# Print the relative rank of "Ginq Sho"
print(rank_ginq_sho)

# Filter rows where Sho is involved, including interaction with Ginq
sho_dyads <- sorted_dyads_DSI %>%
  filter(i1 == "Sho" | i2 == "Sho")
# Print the zDSI values for Sho with others
print(sho_dyads %>% select(i1, i2, dyad, zDSI))
# Filter rows where Ginq is involved, including interaction with Sho
ginq_dyads <- sorted_dyads_DSI %>%
  filter(i1 == "Ginq" | i2 == "Ginq")
# Print the zDSI values for Ginq with others
print(ginq_dyads %>% select(i1, i2, dyad, zDSI))



# Load necessary libraries
library(ggplot2)

# Create the boxplot for zDSI values with a uniform color scheme
p <- ggplot(sorted_dyads_DSI, aes(x = '', y = zDSI)) +
  geom_boxplot(outlier.color = 'black', fill = 'lightgray', alpha = 0.6) +
  geom_jitter(color = 'black', width = 0.2, alpha = 0.6) +  # Add jitter for individual dyads with consistent color
  theme_minimal() +
  labs(title = 'Distribution of zDSI Values for Dyads',
       y = 'zDSI',
       x = 'All Dyads') +
  theme(legend.position = 'none')  # Hide the legend for simplicity

# Highlight specific dyads involving Sho and Ginq
highlight_dyads <- c("Ginq_@_Sho")  # Highlight only the "Ginq Sho" dyad
highlight_data <- sorted_dyads_DSI %>% filter(dyad %in% highlight_dyads)

# Add highlighted points for "Ginq Sho" dyad in red
p <- p + geom_point(data = highlight_data, aes(x = '', y = zDSI), color = 'red', size = 3, shape = 18)

# Print the plot
print(p)









# AKShoGinq <- from 2022-06-27 to 2022-09-27
# AKBukNdaw <- from 2022-06-29 to 2022-09-29

# DSI Dates for BD
# BDSeySirk <- from 2022-06-14 to 2022-09-14
# BDXiaPiep <- from 2022-06-16 to 2022-09-16
# BDNgeOerw <- from 2022-06-22 to 2022-09-22
# BDXinOuli <- from 2022-06-27 to 2022-09-27
# BDKomOort <- from 2022-09-12 to 2022-12-12


# DSI Dates for NH
# NHPomXian <- from 2022-12-10 to 2023-03-10




# Load necessary libraries
library(dplyr)
library(ggplot2)

# Function to calculate and visualize DSI for each dyad pair
analyze_dyad <- function(data, from_date, to_date, ot_source, dyad_identifier) {
  # Calculate DSI for dyads between specified dates
  dyad_data <- DSI(data, from = from_date, to = to_date, ot.source = ot_source)
  
  # Extract dyad identifiers and their DSI values
  dyads_DSI <- dyad_data %>%
    dplyr::select(i1, i2, dyad, DSI, zDSI)
  
  # Sort the dyads based on zDSI values in descending order and rank them
  sorted_dyads_DSI <- dyads_DSI %>%
    filter(!is.na(zDSI)) %>%  # Remove rows with NA zDSI values
    arrange(desc(zDSI)) %>%
    mutate(rank = row_number())  # Add rank based on zDSI values
  
  # Extract the rank of the specified dyad
  rank_dyad <- sorted_dyads_DSI %>%
    filter(dyad == dyad_identifier) %>%
    select(dyad, rank, zDSI)
  
  # Print the relative rank of the specified dyad
  print(rank_dyad)
  
  # Filter rows where the first individual is involved, including interaction with the second
  i1 <- strsplit(dyad_identifier, "_@_")[[1]][1]
  i2 <- strsplit(dyad_identifier, "_@_")[[1]][2]
  
  i1_dyads <- sorted_dyads_DSI %>%
    filter(i1 == i1 | i2 == i1)
  # Print the zDSI values for first individual with others
  print(i1_dyads %>% select(i1, i2, dyad, zDSI))
  
  # Filter rows where the second individual is involved, including interaction with the first
  i2_dyads <- sorted_dyads_DSI %>%
    filter(i1 == i2 | i2 == i2)
  # Print the zDSI values for second individual with others
  print(i2_dyads %>% select(i1, i2, dyad, zDSI))
  
  # Create the boxplot for zDSI values with a uniform color scheme
  p <- ggplot(sorted_dyads_DSI, aes(x = '', y = zDSI)) +
    geom_boxplot(outlier.color = 'black', fill = 'lightgray', alpha = 0.6) +
    geom_jitter(color = 'black', width = 0.2, alpha = 0.6) +  # Add jitter for individual dyads with consistent color
    theme_minimal() +
    labs(title = paste('Distribution of zDSI Values for', dyad_identifier, 'Dyads'),
         y = 'zDSI',
         x = 'All Dyads') +
    theme(legend.position = 'none',
          axis.text.x = element_text(size = 12, face = 'bold'),
          axis.text.y = element_text(size = 12, face = 'bold'),
          plot.title = element_text(size = 14, face = 'bold', hjust = 0.5))  # Improve label readability
  
  # Highlight specific dyad
  highlight_data <- sorted_dyads_DSI %>% filter(dyad == dyad_identifier)
  
  # Add highlighted points for specific dyad in red
  p <- p + geom_point(data = highlight_data, aes(x = '', y = zDSI), color = 'red', size = 3, shape = 18)
  
  # Print the plot
  print(p)
}

# Define datasets and analyze each dyad
# AK Dataset
analyze_dyad(data = dsi_ak, from_date = "2022-06-27", to_date = "2022-09-27", ot_source = ot_ak, dyad_identifier = "Sho_@_Ginq") # Sho and Ginq from 2022-06-27 to 2022-09-27
analyze_dyad(data = dsi_ak, from_date = "2022-06-29", to_date = "2022-09-29", ot_source = ot_ak, dyad_identifier = "Buk_@_Ndaw") # Buk and Ndaw from 2022-06-29 to 2022-09-29

# BD Dataset
analyze_dyad(data = dsi_bd, from_date = "2022-06-14", to_date = "2022-09-14", ot_source = ot_bd, dyad_identifier = "Sey_@_Sirk") # Sey and Sirk from 2022-06-14 to 2022-09-14
analyze_dyad(data = dsi_bd, from_date = "2022-06-16", to_date = "2022-09-16", ot_source = ot_bd, dyad_identifier = "Xia_@_Piep") # Xia and Piep from 2022-06-16 to 2022-09-16
analyze_dyad(data = dsi_bd, from_date = "2022-06-22", to_date = "2022-09-22", ot_source = ot_bd, dyad_identifier = "Nge_@_Oerw") # Nge and Oerw from 2022-06-22 to 2022-09-22
analyze_dyad(data = dsi_bd, from_date = "2022-06-27", to_date = "2022-09-27", ot_source = ot_bd, dyad_identifier = "Xin_@_Ouli") # Xin and Ouli from 2022-06-27 to 2022-09-27
analyze_dyad(data = dsi_bd, from_date = "2022-09-12", to_date = "2022-12-12", ot_source = ot_bd, dyad_identifier = "Kom_@_Oort") # Kom and Oort from 2022-09-12 to 2022-12-12

# NH Dataset
analyze_dyad(data = dsi_nh, from_date = "2022-12-10", to_date = "2023-03-10", ot_source = ot_nh, dyad_identifier = "Pom_@_Xian") # Pom and Xian from 2022-12-10 to 2023-03-10
