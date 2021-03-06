##### INIT #####
# Load init script
source(file.path("_script", "init.R"))
# Load functions
source(file.path(loc_function, "interpol_group.R"))
source(file.path(loc_function, "iso3code_changer.R"))
# Define paths
loc_csv_pwt = file.path(loc_data, "_csv", "_pwt")
loc_final = file.path(loc_data, "_final")
# Define OECD countries
countryOECD_full = c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
                     "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                     "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", 
                     "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", 
                     "Portugal", "Slovakia", "Slovenia", "South Korea", "Spain", "Sweden",
                     "Switzerland", "Turkey", "United Kingdom", "United States")
# Identify datasets
files = list.files(loc_csv_pwt, pattern = "*.csv")
files_split = strsplit(files, "\\.")
# Load all *.csv files and assign their name
for(i in c(1:length(files))){
  assign(files_split[[i]][1], read.csv(file.path(loc_csv_pwt, files[i]), header = TRUE))
}
##### MERGING #####
# Merge main dataset and labor detail
pwt91.final = merge(pwt91, pwt91_labor_detail, by = c("countrycode", "year"), suffixes = c("",".y"), all = TRUE)
pwt100.final = merge(pwt100, pwt100_labor_detail, by = c("countrycode", "year"), suffixes = c("",".y"), all = TRUE)
# Remove duplicated columns
pwt91.final = pwt91.final %>% select(- grep("*\\.y", names(.)))
pwt100.final = pwt100.final %>% select(- grep("*\\.y", names(.)))
# Reorder columns and keep only OECD countries // Rename Country and Year
pwt91.final = pwt91.final %>% 
  select(Country = country, Year = year, everything()) %>% 
  subset(Country %in% countryOECD_full)
pwt100.final = pwt100.final %>% 
  select(Country = country, Year = year, everything()) %>% 
  subset(Country %in% countryOECD_full)
# SAVE DATA
write.csv(pwt91.final, file.path(loc_final, "pwt91.csv"), row.names = FALSE)
write.csv(pwt100.final, file.path(loc_final, "pwt100.csv"), row.names = FALSE)
