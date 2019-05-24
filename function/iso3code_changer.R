iso3code_changer = function(df){
  
  # Load iso from data folder
  iso = read.csv(file.path("data", "iso.csv"), header = TRUE)
  # Merge dataframe with iso on country ISO3code
  df = merge(df, iso[, c(2,3)], by.x = names(df)[1], by.y = "countryiso3code")
  # Reorder columns and delete first
  df = df[, c("country", names(df)[2:(ncol(df)-1)])]
  names(df)[1] = "Country"
  return(df)
}