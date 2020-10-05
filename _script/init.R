#### from summeR ####
# summeR location files
loc_summeR = file.path(dirname(getwd()), "summeR")

# Load packages from summeR
source(file.path(loc_summeR, "_script", "packages.R"))
source(file.path(loc_summeR, "_script", "loc_base.R"))
# Load functions from summeR
sapply(list.files(pattern = "[.]R$", path = file.path(loc_summeR, "_function"), 
                  full.names = TRUE), source)

# Systime
Sys.setlocale("LC_TIME", "English")

#### for values ####
### FUNCTIONS
# Empty so far
### PATHS to DATA
# Empty so far