#### PACKAGES ####
# List all packages
packages <- c(
  "dplyr", "data.table", "ggplot2", "RColorBrewer", "ggrepel", "kableExtra",
  "texreg", "stringr", "tidyr", "gridExtra", "knitr"
  )
# Load packages
lapply(packages, require, character.only = TRUE)
# Clear
rm(packages)

#### PATHS ####
### Data
loc_data = file.path(getwd(), "_data")
### Script
loc_script = file.path(getwd(), "_script")
### Function
loc_function = file.path(getwd(), "_script", "function")

#### FUNCTIONS ####
# Load functions
sapply(list.files(pattern = "[.]R$", path = loc_function, full.names = TRUE), source)

#### SYS TIME ####
# Local sys time
Sys.setlocale("LC_TIME", "English")