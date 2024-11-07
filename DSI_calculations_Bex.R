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


> section 2.5 temporal changes, stq is my dsi ak and set from and to
> to as start date, and same for end date for DSI on set date





# See socialindices tutorial to see what other things you can do with the DSI function



unique_actors <- unique(dsi_nh$actor)
unique_receivers <- unique(dsi_nh$receiver)

print(unique_actors)
print(unique_receivers)




> DSI first and last date of experiment
> May be problems with Ghida because of presence but very active in other focals
> Josie sends observation time based on focals and amount of time spent in the grouping

> Z DSI, from 0 to 1 should be the one to use, score divided by the mean of the group
> a
















# Load necessary libraries
library(dplyr)
library(ggplot2)

# Define the start dates for each dyad
start_dates <- data.frame(
  group = c(rep("BD", 5), rep("AK", 2), "NH"),
  i1 = c("Sey", "Xia", "Nge", "Xin", "Kom", "Sho", "Buk", "Pom"),
  i2 = c("Sirk", "Piep", "Oerw", "Ouli", "Oort", "Ginq", "Ndaw", "Xian"),
  start_date = as.Date(c("2022-09-14", "2022-09-16", "2022-09-22", "2022-09-27", "2022-12-12",
                         "2022-09-27", "2022-09-29", "2023-03-10"))
)

# For each group, find the earliest start date
min_dates <- start_dates %>%
  group_by(group) %>%
  summarise(min_date = min(start_date))

# Filter interaction and observation time data for each group based on the earliest start date
dsi_bd_filtered <- dsi_bd %>%
  filter(date >= min_dates$min_date[min_dates$group == "BD"])
ot_bd_filtered <- ot_bd %>%
  filter(date >= min_dates$min_date[min_dates$group == "BD"])

dsi_ak_filtered <- dsi_ak %>%
  filter(date >= min_dates$min_date[min_dates$group == "AK"])
ot_ak_filtered <- ot_ak %>%
  filter(date >= min_dates$min_date[min_dates$group == "AK"])

dsi_nh_filtered <- dsi_nh %>%
  filter(date >= min_dates$min_date[min_dates$group == "NH"])
ot_nh_filtered <- ot_nh %>%
  filter(date >= min_dates$min_date[min_dates$group == "NH"])
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Define the start dates for each dyad
start_dates <- data.frame(
  group = c(rep("BD", 5), rep("AK", 2), "NH"),
  i1 = c("Sey", "Xia", "Nge", "Xin", "Kom", "Sho", "Buk", "Pom"),
  i2 = c("Sirk", "Piep", "Oerw", "Ouli", "Oort", "Ginq", "Ndaw", "Xian"),
  start_date = as.Date(c("2022-09-14", "2022-09-16", "2022-09-22", "2022-09-27", "2022-12-12",
                         "2022-09-27", "2022-09-29", "2023-03-10"))
)

# For each group, find the earliest start date
min_dates <- start_dates %>%
  group_by(group) %>%
  summarise(min_date = min(start_date))

# Filter interaction and observation time data for each group based on the earliest start date
dsi_bd_filtered <- dsi_bd %>%
  filter(date >= min_dates$min_date[min_dates$group == "BD"])
ot_bd_filtered <- ot_bd %>%
  filter(date >= min_dates$min_date[min_dates$group == "BD"])

dsi_ak_filtered <- dsi_ak %>%
  filter(date >= min_dates$min_date[min_dates$group == "AK"])
ot_ak_filtered <- ot_ak %>%
  filter(date >= min_dates$min_date[min_dates$group == "AK"])

dsi_nh_filtered <- dsi_nh %>%
  filter(date >= min_dates$min_date[min_dates$group == "NH"])
ot_nh_filtered <- ot_nh %>%
  filter(date >= min_dates$min_date[min_dates$group == "NH"])


# Calculate DSI for BD group
bd <- DSI(
  dsi_bd_filtered,
  ot.source = ot_bd_filtered,
  duration.NA.treatm = "count",
  onlyfocaldyads = TRUE
)

# Calculate DSI for AK group
ak <- DSI(
  dsi_ak_filtered,
  ot.source = ot_ak_filtered,
  duration.NA.treatm = "count",
  onlyfocaldyads = TRUE
)

# Calculate DSI for NH group
nh <- DSI(
  dsi_nh_filtered,
  ot.source = ot_nh_filtered,
  duration.NA.treatm = "count",
  onlyfocaldyads = TRUE
)





# Function to extract DSI for dyads and create graphs
create_dsi_plots <- function(dsi_data, group_name, dyads) {
  # Extract DSI scores for dyads of interest
  dyad_dsi <- dsi_data %>%
    filter((i1 %in% dyads$i1 & i2 %in% dyads$i2) | (i1 %in% dyads$i2 & i2 %in% dyads$i1))
  
  # Add a column indicating if the dyad is of interest
  dsi_data$dyad_of_interest <- ifelse(
    (paste(dsi_data$i1, dsi_data$i2, sep = "_") %in% paste(dyads$i1, dyads$i2, sep = "_")) |
      (paste(dsi_data$i2, dsi_data$i1, sep = "_") %in% paste(dyads$i1, dyads$i2, sep = "_")),
    "Dyad of Interest",
    "Other Dyads"
  )
  
  # Create boxplot
  p <- ggplot(dsi_data, aes(x = dyad_of_interest, y = DSI)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(color = dyad_of_interest), width = 0.2, size = 2) +
    scale_color_manual(values = c("Dyad of Interest" = "red", "Other Dyads" = "black")) +
    labs(
      title = paste("DSI Scores in", group_name, "Group"),
      x = "Dyad Category",
      y = "DSI Score",
      color = "Dyad Category"
    ) +
    theme_minimal()
  
  # Display the plot
  print(p)
  
  # Print DSI scores for dyads of interest
  print(dyad_dsi)
}

# Apply the function to each group
# For BD group
bd_dyads <- start_dates %>% filter(group == "BD")
create_dsi_plots(bd, "BD", bd_dyads)

# For AK group
ak_dyads <- start_dates %>% filter(group == "AK")
create_dsi_plots(ak, "AK", ak_dyads)

# For NH group
nh_dyads <- start_dates %>% filter(group == "NH")
create_dsi_plots(nh, "NH", nh_dyads)




