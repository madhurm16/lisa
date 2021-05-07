#### PACKAGES ####
# List all packages
packages <- c(
  # Load Data
  "foreign", "haven",
  # Deal with Data
  "dplyr", "reshape2", "tidyr", "tidyselect", "data.table",
  "stringr", # String
  "lubridate", # Date format
  "zoo", # Time series
  # Graphics
  "ggplot2", "ggrepel", "RColorBrewer", "grid", "ggpubr", "gridExtra",
  # Tables
  "texreg",
  # Knitr and Kable
  "kableExtra", "knitr")
# Load packages
lapply(packages, require, character.only = TRUE)
# Clear
rm(packages)

#### LOCATIONS ####
# Standard file location
loc_data = file.path(getwd(), "_data")
loc_function = file.path(getwd(), "_function")
loc_graphic = file.path(getwd(), "_graphic")
loc_script = file.path(getwd(), "_script")
loc_tabular = file.path(getwd(), "_tabular")

#### FUNCTIONS ####
sapply(list.files(pattern = "[.]R$", path = loc_function, full.names = TRUE), source)

#### SYSTIME #####
Sys.setlocale("LC_TIME", "English")