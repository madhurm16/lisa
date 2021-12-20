##### INIT #####
# Load init script
source(file.path("_script", "init.R"))
# Load functions
source(file.path(loc_function, "interpol_group.R"))
source(file.path(loc_function, "iso3code_changer.R"))
# Define paths
loc_csv_vvc= file.path(loc_data, "_csv", "_vvc")
loc_final = file.path(loc_data, "_final")
# Define OECD countries
countryOECD_full = c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
                     "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                     "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", 
                     "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", 
                     "Portugal", "Slovakia", "Slovenia", "South Korea", "Spain", "Sweden",
                     "Switzerland", "Turkey", "United Kingdom", "United States")
# Identify datasets
files = list.files(loc_csv_vvc, pattern = "*.csv")
files_split = strsplit(files, "\\.")
# Load all *.csv files and assign their name
for(i in c(1:length(files))){
  assign(files_split[[i]][1], read.csv(file.path(loc_csv_vvc, files[i]), header = TRUE))
}
# Melt
netUreprate_single = netUreprate_single %>% 
  setDT %>% melt(id.var = "Country", variable.name = "Year", value.name = "URRSingle") %>% 
  mutate(Year = as.integer(substr(Year, 2,5))) %>% 
  arrange(Country, Year)
netUreprate_family = netUreprate_family %>% 
  setDT %>% melt(id.var = "Country", variable.name = "Year", value.name = "URRFamily") %>% 
  mutate(Year = as.integer(substr(Year, 2,5))) %>% 
  arrange(Country, Year)
# Merge
vvc = merge(netUreprate_single, netUreprate_family, by = c("Country", "Year"))
# Four more columns for interpolations
vvc$URRSingle.lin_inter = vvc$URRSingle
vvc$URRSingle.cst_inter = vvc$URRSingle
vvc$URRFamily.lin_inter = vvc$URRFamily
vvc$URRFamily.cst_inter = vvc$URRFamily
# Linear interpolation
vvc[, c("Country", "Year", "URRSingle.lin_inter")] = vvc[, c("Country", "Year", "URRSingle.lin_inter")] %>% 
  interpol_group(method_use = "linear")
vvc[, c("Country", "Year", "URRFamily.lin_inter")] = vvc[, c("Country", "Year", "URRFamily.lin_inter")] %>% 
  interpol_group(method_use = "linear")
# Constant interpolation
vvc[, c("Country", "Year", "URRSingle.cst_inter")] = vvc[, c("Country", "Year", "URRSingle.cst_inter")] %>% 
  interpol_group(method_use = "constant")
vvc[, c("Country", "Year", "URRFamily.cst_inter")] = vvc[, c("Country", "Year", "URRFamily.cst_inter")] %>% 
  interpol_group(method_use = "constant")
# SAVE DATA
write.csv(vvc, file.path(loc_final, "vvc.csv"), row.names = FALSE)
