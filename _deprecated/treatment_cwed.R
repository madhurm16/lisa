##### INIT #####

# Define required packages
require(dplyr)
require(reshape2)

# Define functions
source("./function/interpol_group.R")
source("./function/iso3code_changer.R")

# Define paths
cwed_country_rename = function(cwed_df){
  
  # Switch to character
  cwed_df$Country = as.character(cwed_df$Country)
  
  # Rename
  cwed_df$Country[which(cwed_df$Country == "Korea")] = "South Korea"
  
  # Return to factor
  cwed_df$Country = as.factor(cwed_df$Country)
  
  return(cwed_df)
  
}
loc_csv_cwed = file.path("data", "csv", "cwed")
loc_final = file.path("data", "final")

# Define OECD countries
countryOECD_full = c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
                     "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                     "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", 
                     "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", 
                     "Portugal", "Slovakia", "Slovenia", "South Korea", "Spain", "Sweden",
                     "Switzerland", "Turkey", "United Kingdom", "United States")

# Identify datasets
files = list.files(loc_csv_cwed, pattern = "*.csv")
files_split = strsplit(files, "\\.")

# Load all *.csv files and assign their name
for(i in c(1:length(files))){
  assign(files_split[[i]][1], read.csv(file.path(loc_csv_cwed, files[i]), header = TRUE))
}

##### DATASET BASELINE #####

# Remove useless variables
cwed = cwed %>% 
  select(- COUNTRY.ABBREV, -CCODE)

# Index names
names(cwed)[c(1,2)] = c("Country", "Year")

##### BASIC TREATMENT #####

## Keep OECD countries
cwed = cwed %>% 
  cwed_country_rename() %>% 
  subset(Country %in% countryOECD_full)

## Variable of interest : unemployment replacement rates (i.e. US100 and UC1000)
cwed = cwed %>% 
  select(Country, Year, US100, UC1000)

# Four more columns for interpolations
cwed$US100.lin_inter = cwed$US100
cwed$US100.cst_inter = cwed$US100
cwed$UC1000.lin_inter = cwed$UC1000
cwed$UC1000.cst_inter = cwed$UC1000

# Linear interpolation
cwed[, c("Country", "Year", "US100.lin_inter")] = cwed[, c("Country", "Year", "US100.lin_inter")] %>% 
  interpol_group(method_use = "linear")
cwed[, c("Country", "Year", "UC1000.lin_inter")] = cwed[, c("Country", "Year", "UC1000.lin_inter")] %>% 
  interpol_group(method_use = "linear")

# Constant interpolation
cwed[, c("Country", "Year", "US100.cst_inter")] = cwed[, c("Country", "Year", "US100.cst_inter")] %>% 
  interpol_group(method_use = "constant")
cwed[, c("Country", "Year", "UC1000.cst_inter")] = cwed[, c("Country", "Year", "UC1000.cst_inter")] %>% 
  interpol_group(method_use = "constant")

# SAVE DATA
write.csv(cwed, file.path(loc_final, "cwed.csv"), row.names = FALSE)