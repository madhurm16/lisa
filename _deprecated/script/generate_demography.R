##### INIT #####

# Define required packages
require(dplyr)
require(reshape2)

# Define functions

# Define paths
loc_csv_populstat = file.path("data", "csv", "populstat")
loc_final = file.path("data", "final")

# Load datasets
populstat = read.csv(file.path(loc_csv_populstat, "populstat_info.csv"), header = TRUE)
esaun = read.csv(file.path(loc_final, "esaun.csv"), header = TRUE)

# Rename populstat
names(populstat)[3] = "total"

##### DATA GENERATION #####

# Total population
esaun$total = esaun$child + esaun$young + esaun$old

# Remove AgeGroups from esaun
esaun = esaun %>% select(-contains("X"))

# Populstat for data between 1910 and 1949 // esaun for data between 1950 and 2100
demo = merge(populstat %>% subset(Year %in% c(1910:1949)), esaun, all = TRUE)

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