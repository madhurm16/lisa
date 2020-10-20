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
loc_csv_oecd = file.path(loc_data, "_csv", "_oecd")
loc_final = file.path(loc_data, "_final")
# Define OECD countries
countryOECD_full = c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
                     "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                     "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", 
                     "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", 
                     "Portugal", "Slovakia", "Slovenia", "South Korea", "Spain", "Sweden",
                     "Switzerland", "Turkey", "United Kingdom", "United States")
# Identify datasets
files = list.files(loc_csv_oecd, pattern = "*.csv")
files_split = strsplit(files, "\\.")
# Load all *.csv files and assign their name
for(i in c(1:length(files))){
  assign(files_split[[i]][1], read.csv(file.path(loc_csv_oecd, files[i]), header = TRUE))
}
##### DATASET BASELINE #####
# Create
oecd = data.frame(rep(countryOECD_full, each = length(1950:2020)), rep(1950:2020, length(countryOECD_full)))
# Index names
names(oecd) = c("Country", "Year")
##### COLLECTIVE BARGAINING #####
# Keep useful columns // Change country name specfication // Keep only OECD countries
collective_bargaining_coverage = collective_bargaining_coverage %>% 
  select("Country", "Year", "Value") %>% 
  oecd_country_rename() %>% 
  subset(Country %in% countryOECD_full)
# Rename value column
names(collective_bargaining_coverage)[3] = "union_coverage"
# Two more columns for interpolations
collective_bargaining_coverage$union_coverage.lin_inter = collective_bargaining_coverage$union_coverage
collective_bargaining_coverage$union_coverage.cst_inter = collective_bargaining_coverage$union_coverage
# Merge with dataset baseline
collective_bargaining_coverage = merge(oecd, collective_bargaining_coverage, all = TRUE)
# Linear interpolation
collective_bargaining_coverage[,c("Country", "Year", "union_coverage.lin_inter")] = 
  interpol_group(collective_bargaining_coverage[,c("Country", "Year", "union_coverage.lin_inter")],
                 method_use = "linear")
# Constant interpolation
collective_bargaining_coverage[,c("Country", "Year", "union_coverage.cst_inter")] = 
  interpol_group(collective_bargaining_coverage[,c("Country", "Year", "union_coverage.cst_inter")],
                 method_use = "constant")
#### TRADE UNION DENSITY ####
# Keep useful columns // Change country name specfication // Keep only OECD countries
trade_union_density = trade_union_density %>%
  select("Country", "Year", "SOURCE", "Value") %>% 
  setDT %>% dcast(Country + Year ~ SOURCE, value.var = "Value") %>% 
  oecd_country_rename() %>% 
  subset(Country %in% countryOECD_full)
# Administrative data (1st best), Survey data otherwise
trade_union_density$union_density = ifelse(is.na(trade_union_density$ADM)==TRUE,
                                           trade_union_density$SVY, trade_union_density$ADM)
# Rename columns
names(trade_union_density)[c(3,4)] = paste0("union_density_", names(trade_union_density)[c(3,4)])
# Two more columns for interpolations
trade_union_density$union_density.lin_inter = trade_union_density$union_density
trade_union_density$union_density.cst_inter = trade_union_density$union_density
# Merge with dataset baseline
trade_union_density = merge(oecd, trade_union_density, all = TRUE)
# Linear interpolation
trade_union_density[,c("Country", "Year", "union_density.lin_inter")] = 
  interpol_group(trade_union_density[,c("Country", "Year", "union_density.lin_inter")],
                 method_use = "linear")
# Constant interpolation
trade_union_density[,c("Country", "Year", "union_density.cst_inter")] = 
  interpol_group(trade_union_density[,c("Country", "Year", "union_density.cst_inter")],
                 method_use = "constant")
#### TAX REVENUE ####
# Keep useful columns // Change country name specfication // Keep only OECD countries
tax_revenue = tax_revenue %>% select("ï..LOCATION", "TIME", "MEASURE", "Value")
# Rename index variables
names(tax_revenue)[c(1,2)] = c("Country", "Year")
# Change ISO3CODE to Country name
tax_revenue = iso3code_changer(tax_revenue)
# Create two variables // Rename countries // Keep only OECD countries
tax_revenue = tax_revenue %>% setDT %>% 
  dcast(Country + Year ~ MEASURE, value.var = "Value") %>% 
  oecd_country_rename() %>% 
  subset(Country %in% countryOECD_full)
# Rename columns
names(tax_revenue)[c(3,4)] = paste0("tax_rev_", names(tax_revenue)[c(3,4)])
##### LABOR SHARE #####
# Keep useful columns // Change country name specfication // Keep only OECD countries
labor_share = labor_share %>% 
  subset(SECTOR == 1) %>% 
  select("ï..LOCATION", "TIME", "Value")
# Rename index variables
names(labor_share) = c("Country", "Year", "labor_share")
# Change ISO3CODE to Country name
labor_share = iso3code_changer(labor_share)
##### MERGING #####
# Merge all
oecd_final = oecd %>% 
  merge(tax_revenue, by = c("Country", "Year")) %>% 
  merge(trade_union_density, by = c("Country", "Year")) %>% 
  merge(collective_bargaining_coverage, by = c("Country", "Year")) %>% 
  merge(labor_share, by = c("Country", "Year")) 
# SAVE DATA
write.csv(oecd_final, file.path(loc_final, "oecd.csv"), row.names = FALSE)