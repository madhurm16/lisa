##### INIT #####

# Define required packages
require(dplyr)
require(reshape2)

# Define paths
loc_csv_un = file.path("data", "csv", "un")

# Define OECD countries
countryOECD_full = c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
                     "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                     "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", 
                     "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", 
                     "Portugal", "Slovakia", "Slovenia", "South Korea", "Spain", "Sweden",
                     "Switzerland", "Turkey", "United Kingdom", "United States")

# Load data
esaun <- read.csv(file.path(loc_csv_un,"WPP2017_PopulationByAgeSex_Medium.csv"), sep = ',', header = TRUE)

##### BASIC TREATMENT #####

# Rename countries
esaun$Location = as.character(esaun$Location)
esaun$Location[which(esaun$Location == "Republic of Korea")] = "South Korea"
esaun$Location[which(esaun$Location == "Czechia")] = "Czech Republic"
esaun$Location[which(esaun$Location == "United States of America")] = "United States"

# Keep only country and useful columns
esaun = esaun %>% 
  subset(Location %in% countryOECD_full) %>% 
  select(c(2,5,7,12))

# Rename Location & Time
names(esaun)[c(1,2)] = c("Country", "Year")

# From long to wide format
esaun = dcast(esaun, Country + Year ~ AgeGrp, value.var = "PopTotal")

# Rename age some age groups for consistency
names(esaun)[names(esaun) %in% c("0-4","5-9")] = c("00-04","05-09")

# Aggregate 80+ after 1990 (see README in data/raw/un)
esaun$`80+` = ifelse(is.na(esaun$`80+`),
                     esaun$`80-84` + esaun$`85-89` + esaun$`90-94` 
                     + esaun$`95-99` + esaun$`100+`,
                     esaun$`80+`)

# Remove useless columns
esaun = esaun[,-c(5,20,22:25)]

# Sort columns : Country / Year / AgeGroups
esaun = esaun[, c(1, 2, order(names(esaun)[-c(1,2)])+2)]

##### DEPENDENCY RATIO #####

## Aggregate age groups in three categories : child (<20), young (from 20 to 60) & old (60+)
# Column location of each group
child = c(3:6)
young = c(7:14)
old = c(15:19)

# Compute population in each age groups
esaun$child = rowSums(esaun[,child], na.rm = TRUE)
esaun$young <- rowSums(esaun[,young], na.rm = TRUE)
esaun$old <- rowSums(esaun[,old], na.rm = TRUE)

# Compute old-age dependency ratio
esaun$dep <- esaun$old/esaun$young

# Define Country as factor
esaun$Country <- as.factor(esaun$Country)

# SAVE DATA
write.csv(esaun, file.path(loc_csv_un, "esaun.csv"), row.names = FALSE)