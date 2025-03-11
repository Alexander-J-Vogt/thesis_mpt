# TARGET: Import population for each county
# INDATA: Population Data
# OUTDATA/ OUTPUT: MAINNAME, pop_state

################################################################################################################+
# INTRO	script-specific ####

#clear gobal environment of all but uppercase objects (globals, myfunctions, scalars)
CLEARCOND()

#get scriptname
MAINNAME <- current_filename()#returns path+name of sourced script (from currently executed master script)
if(is.null(MAINNAME)){
  MAINNAME <- rstudioapi::getActiveDocumentContext()$path #dto. of currently executed script
}
MAINNAME <- sub(".*/|^[^/]*$", "", MAINNAME)
MAINNAME <- substr(MAINNAME,1,nchar(MAINNAME)-2) #cut off .R
######################+
#release unused memory 
gc()

################################################################################################################+

# 1. County-Level Population ===================================================

## 1.1 Import Raw Datasets from U.S. Census ------------------------------------

# Population Estimates for the years 2000 to 2010
pop_00 <- read_csv(paste0(A, "j_us_census_bureau/", "co-est00int-tot.csv"), col_types = cols(.default = "c"))
setDT(pop_00)

# Population Estinates for the years 2010 t0 2019
pop_10 <- read_csv(paste0(A, "j_us_census_bureau/", "co-est2019-alldata.csv"), col_types = cols(.default = "c"))
setDT(pop_10)

# Population Estimates for the years 2020to 2023
pop_20 <- read_csv(paste0(A, "j_us_census_bureau/", "co-est2023-alldata.csv"), col_types = cols(.default = "c"))
setDT(pop_20)

## 1.2 Basic Data Manipulation of the Population Datasets ----------------------

# Create list for the population datasets
pop_list <- list(pop_00 = pop_00, pop_10 = pop_10, pop_20 = pop_20)

# Apply the same operations to each element in the list
pop_list <- lapply(pop_list, function(data) {
  
  # Create fips code
  data <- FIPSCREATOR(data, state_col = "STATE", county_col = "COUNTY")
  
  # Select relevant variables
  data <- EXTRACTVAR(data = data, state_col = "STATE", county_col = "fips", indicator = "POPESTIMATE")
  
  # Reshape into long format
  data <- RESHAPEPOP(data = data)
  
  # All variables to small letter
  colnames(data) <- str_to_lower(names(data))
  
  return(data)
})

# Delete the year 2010 in the population datset for the period 2000 to 2010
pop_list[[1]] <- subset(pop_list[[1]], year != "2010")

# Create population dataset over all periods for county and states
pop_data <- bind_rows(pop_list)
setDT(pop_data)

# Format as integer
pop_data <- pop_data[, year := as.integer(year)]
pop_data <- pop_data[, population := as.integer(population)]

# Restrict to the necessary year
pop_data <- pop_data[inrange(year, 2000, 2023)]

# Extract all state observations
pop_state_data <- pop_data[
  substr(get("fips"), nchar(get("fips")) - 2, nchar(get("fips"))) == "000", 
  .(fips, state, year, population)
]
pop_state_data <- pop_state_data[, c("state", "year", "population")]

# Extract all county population observatinons
pop_cnty_data <- pop_data[
  substr(get("fips"), nchar(get("fips")) - 2, nchar(get("fips"))) != "000", 
  .(fips, state, year, population)
]

# Adjust names for clarification
setnames(pop_cnty_data, old = c("population"), new = c("cnty_pop"))
setnames(pop_state_data, old = c("population"), new = c("state_pop"))

# SAVE
SAVE(dfx = pop_cnty_data, namex = MAINNAME)
SAVE(dfx = pop_state_data, namex = "pop_state")



################################### END #######################################+