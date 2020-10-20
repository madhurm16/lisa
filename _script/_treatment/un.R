##### INIT #####
# Load init script
source(file.path("_script", "init.R"))
# Functions
oecd_country_rename = function(oecd_df){
  
  # Switch to character
  oecd_df$Country = as.character(oecd_df$Country)
  
  # Rename
  oecd_df$Country[which(oecd_df$Country == "Korea")] = "South Korea"
  oecd_df$Country[which(oecd_df$Country == "Slovak Republic")] = "Slovakia"
  
  # Return to factor
  oecd_df$Country = as.factor(oecd_df$Country)
  
  return(oecd_df)
  
}
# Load functions
source(file.path(loc_function, "interpol_group.R"))
source(file.path(loc_function, "iso3code_changer.R"))
# Define paths
loc_csv_un = file.path(loc_data, "_csv", "_un")
loc_final = file.path(loc_data, "_final")
# Define OECD countries
countryOECD_full = c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
                     "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                     "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", 
                     "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", 
                     "Portugal", "Slovakia", "Slovenia", "South Korea", "Spain", "Sweden",
                     "Switzerland", "Turkey", "United Kingdom", "United States")
# Load data
esaun <- read.csv(file.path(loc_csv_un,"WPP2017_PopulationByAgeSex_Medium.csv"), sep = ',', header = TRUE)
##### TREATMENT #####
# Rename countries
esaun$Location = as.character(esaun$Location)
esaun$Location[which(esaun$Location == "Republic of Korea")] = "South Korea"
esaun$Location[which(esaun$Location == "Czechia")] = "Czech Republic"
esaun$Location[which(esaun$Location == "United States of America")] = "United States"
# Keep only OECD countries and useful columns
esaun = esaun %>% 
  subset(Location %in% countryOECD_full) %>% 
  select(c(2,5,7,12))
# Rename Location & Time
names(esaun)[c(1,2)] = c("Country", "Year")
# From long to wide format
esaun = esaun %>% setDT %>% dcast(Country + Year ~ AgeGrp, value.var = "PopTotal")
# Rename age some age groups for consistency
names(esaun)[names(esaun) %in% c("0-4","5-9")] = c("00-04","05-09")
# Aggregate 80+ after 1990 (see README in data/raw/un)
esaun$`80+` = ifelse(is.na(esaun$`80+`),
                     esaun$`80-84` + esaun$`85-89` + esaun$`90-94` 
                     + esaun$`95-99` + esaun$`100+`,
                     esaun$`80+`)
# Sort columns : Country / Year / AgeGroups // Remove useless columns
esaun = esaun[,c(1:3, 13, 4, 6:12, 14:20)] %>% ungroup
##### DEPENDENCY RATIO #####
## Aggregate age groups in three categories : child (<20), young (from 20 to 60) & old (60+)
# Column location of each group
child = c(3:6)
young = c(7:14)
old = c(15:19)
young_1564 = c(6:15)
old_65 = c(16:19)
# Compute population in each age groups
esaun$child <- rowSums(esaun[,..child], na.rm = TRUE)
esaun$young <- rowSums(esaun[,..young], na.rm = TRUE)
esaun$old <- rowSums(esaun[,..old], na.rm = TRUE)
esaun$young_1564 <- rowSums(esaun[,..young_1564], na.rm = TRUE)
esaun$old_65 <- rowSums(esaun[,..old_65], na.rm = TRUE)
# Compute old-age dependency ratio
esaun$dep <- esaun$old/esaun$young
esaun$dep_65 <- esaun$old_65/esaun$young_1564
# Define Country as factor
esaun$Country <- as.factor(esaun$Country)
# SAVE DATA
write.csv(esaun, file.path(loc_final, "esaun.csv"), row.names = FALSE)