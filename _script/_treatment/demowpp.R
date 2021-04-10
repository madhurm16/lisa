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
loc_csv_un = file.path(loc_data, "_csv", "_wpp")
loc_csv_populstat = file.path(loc_data, "_csv", "_populstat")
loc_final = file.path(loc_data, "_final")
# Define OECD countries
countryOECD_full = c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
                     "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                     "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", 
                     "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", 
                     "Portugal", "Slovakia", "Slovenia", "South Korea", "Spain", "Sweden",
                     "Switzerland", "Turkey", "United Kingdom", "United States")
# Load data
wpp <- read.csv(file.path(loc_csv_un,"WPP2017_PopulationByAgeSex_Medium.csv"))
populstat <- read.csv(file.path(loc_csv_populstat, "populstat_info.csv")) %>% 
  select(Country, Year, total = pop_total)
##### TREATMENT WPP #####
# Rename countries
wpp$Location = as.character(wpp$Location)
wpp$Location[which(wpp$Location == "Republic of Korea")] = "South Korea"
wpp$Location[which(wpp$Location == "Czechia")] = "Czech Republic"
wpp$Location[which(wpp$Location == "United States of America")] = "United States"
# Keep only OECD countries and useful columns
wpp = wpp %>% 
  subset(Location %in% countryOECD_full) %>% 
  select(c(2,5,7,12))
# Rename Location & Time into Country & Year
names(wpp)[c(1,2)] = c("Country", "Year")
# From long to wide format
wpp = wpp %>% setDT %>% dcast(Country + Year ~ AgeGrp, value.var = "PopTotal")
# Rename age some age groups for consistency
names(wpp)[names(wpp) %in% c("0-4","5-9")] = c("00-04","05-09")
# Aggregate 80+ after 1990
wpp$`80+` = ifelse(is.na(wpp$`80+`),
                     wpp$`80-84` + wpp$`85-89` + wpp$`90-94` 
                     + wpp$`95-99` + wpp$`100+`,
                     wpp$`80+`)
# Sort columns : Country / Year / AgeGroups // Remove useless columns
wpp = wpp[,c(1:3, 13, 4, 6:12, 14:20)] %>% ungroup
##### DEPENDENCY RATIO #####
## Aggregate age groups in three categories : child (<20), young (from 20 to 60) & old (60+)
# Column location of each group
child = c(3:6)
young = c(7:14)
old = c(15:19)
young_1564 = c(6:15)
old_65 = c(16:19)
# Compute population in each age groups
wpp$child <- rowSums(wpp[,..child], na.rm = TRUE)
wpp$young <- rowSums(wpp[,..young], na.rm = TRUE)
wpp$old <- rowSums(wpp[,..old], na.rm = TRUE)
wpp$young_1564 <- rowSums(wpp[,..young_1564], na.rm = TRUE)
wpp$old_65 <- rowSums(wpp[,..old_65], na.rm = TRUE)
# Compute total population
wpp$total <- wpp$child + wpp$young + wpp$old
# Compute old-age dependency ratio
wpp$dep <- wpp$old/wpp$young
wpp$dep_65 <- wpp$old_65/wpp$young_1564
# Define Country as factor
wpp$Country <- as.factor(wpp$Country)
# SAVE DATA
write.csv(wpp, file.path(loc_final, "wpp.csv"), row.names = FALSE)
#### TREATMENT DEMO ####
# Populstat for data between 1910 and 1949 // wpp for data between 1950 and 2100
demo = wpp %>% 
  select(Country, Year, child:dep_65) %>% 
  merge(subset(populstat, Year %in% c(1910:1949)), ., all = TRUE)
# Proportion of young in the total population
demo$young_prop = demo$young/demo$total
# Assumption : the young share before 1950 was equal to the average young share between [1950,1960)
# Average young_prop [1950, 1960) for each country
youngmean = demo %>% 
  subset(Year %in% c(1950:1959)) %>% 
  group_by(Country) %>% 
  summarise(young_prop_mean5059 = mean(young_prop))
# Merge average young_prop with demo dataset
demo = merge(demo, youngmean)
# Compute young population before 1950
demo$young[demo$Year < 1950] = demo$total[demo$Year < 1950]*demo$young_prop_mean5059[demo$Year < 1950]
# Create new variable : young population 40 years before
demo = merge(demo, demo %>% 
               select("Country", "Year", "young.40y_before" = "young") %>% 
               mutate(Year = Year + 40), all.x = TRUE)
# Compute n_t (population growth in the model)
demo$n = demo$young / demo$young.40y_before
# Compute p_t (survival rate in the model)
demo$p = demo$dep * demo$n
# SAVE DATA
write.csv(demo, file.path(loc_final, "demo.csv"), row.names = FALSE)