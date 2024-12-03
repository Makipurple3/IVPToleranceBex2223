lh <- read.csv("/Users/maki/Desktop/Master Thesis/Josies Codes/IVP Life history_180424.csv")
colnames(lh)


# I want to see if "Mother" = Ginq, Ndaw, Xian, Ouli, Oerw, Oort, Sirk, Piep and IF yes, ID and DOB for these individuals

 
# Define the list of mothers
mothers_to_check <- c("Ginq", "Ndaw", "Xian", "Ouli", "Oerw", "Oort", "Sirk", "Piep")
# Filter the dataset for matching mothers and DOB after 01-09-2024
filtered_lh <- lh[lh$Mother %in% mothers_to_check & as.Date(lh$DOB, format = "%d/%m/%Y") > as.Date("2022-09-01"), 
                  c("Individual", "DOB")]
# View the filtered dataset
print(filtered_lh)

      

