require(openxlsx)
require(haven)

loc_data = file.path(getwd(), "data")

# CSV DATA to CSV FORMAT : Function
csv_to_csv = function(loc_raw, loc_csv){
  
  files = list.files(loc_raw, pattern = "csv")
  files_split = strsplit(files, "\\.")
  
  for(i in c(1:length(files))){
    if(files_split[[i]][2] == "csv"){
      write.csv(read.csv(file.path(loc_raw, files[i])),
                file.path(loc_csv, paste0(files_split[[i]][1], ".csv")),
                row.names = FALSE)
    }
  }
}

##### PENN WORLD TABLE #####

# PWT DATA to CSV FORMAT : Function
pwt_to_csv = function(loc_raw, loc_csv){
  
  files = list.files(loc_raw, pattern = "xlsx|dta|csv")
  files_split = strsplit(files, "\\.")
  
  for(i in c(1:length(files))){
    if(files_split[[i]][2] == "xlsx"){
      write.csv(read.xlsx(file.path(loc_raw, files[i]), sheet = "Data"),
                file.path(loc_csv, paste0(files_split[[i]][1], ".csv")), 
                row.names = FALSE)
    }
    
    if(files_split[[i]][2] == "dta"){
      write.csv(read_dta(file.path(loc_raw, files[i])),
                file.path(loc_csv, paste0(files_split[[i]][1], ".csv")),
                row.names = FALSE)
    }
    
    if(files_split[[i]][2] == "csv"){
      write.csv(read.csv(file.path(loc_raw, files[i])),
                file.path(loc_csv, paste0(files_split[[i]][1], ".csv")),
                row.names = FALSE)
    }
  }
}

# Raw data location
loc_raw_pwt = file.path(loc_data, "raw", "pwt")
# Csv data location
loc_csv_pwt = file.path(loc_data, "csv", "pwt")

# Do
pwt_to_csv(loc_raw_pwt, loc_csv_pwt)

##### OECD #####

# Raw data location
loc_raw_oecd = file.path(loc_data, "raw", "oecd")
# Csv data location
loc_csv_oecd = file.path(loc_data, "csv", "oecd")

# Do
csv_to_csv(loc_raw_oecd, loc_csv_oecd)

##### UNITED NATIONS : World Population Prospect #####

# Raw data location
loc_raw_un = file.path(loc_data, "raw", "un")
# Csv data location
loc_csv_un = file.path(loc_data, "csv", "un")

# Do
csv_to_csv(loc_raw_un, loc_csv_un)

##### CWED #####

# Raw data location
loc_raw_cwed = file.path(loc_data, "raw", "cwed")
# Csv data location
loc_csv_cwed = file.path(loc_data, "csv", "cwed")

# Do
csv_to_csv(loc_raw_cwed, loc_csv_cwed)