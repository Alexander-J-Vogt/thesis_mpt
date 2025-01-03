# TARGET: Import Summary of Deposits
# INDATA: SOD data files in .csv format
# OUTDATA/ OUTPUT: mp_transmission_databasics_fips

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


# 1. Importing Summary of Deposits (by FDIC) for the years 2018 to 2024 ========

## 1.1 Import Files ------------------------------------------------------------

# list all raw sod files in h_sod_direct
files_sod <- list.files(paste0(A, "f_sod/"))
files_sod <- files_sod[grepl("\\csv$",files_sod)]
years <- as.numeric(str_extract(files_sod, "\\d{4}"))
files_sod <- files_sod[years >= 1994 & years <= 2024]

# Loop for importing all sod datasets from 1994 to 2024 
for (i in files_sod) {
  print(paste0("Iteration Status: ", i))
  year <- str_sub(i, start = 5, end = 8)
  # file <- suppressMessages(read_csv(paste0(A, "f_sod/", i), , col_types = cols(.default = "c")))
  # SAVE(dfx = file, namex = paste0("sod_", year))
  # rm(file, year)
  assign(
    paste0("df_sod_", year),
    read_csv(paste0(A, "f_sod/", i), , col_types = cols(.default = "c"))
  )
}



## 1.2 Append all raw SOD files ------------------------------------------------

# Loop over all SOD datasets in order to create one large data frame for 
# the years 1994 to 2024

# Create vector with all names of SOD datasets
# sod_temp <- list.files(paste0(TEMP))
# sod_temp <- sod_temp[str_detect(sod_temp, "^sod")]
# years <- as.numeric(str_extract(sod_temp, "\\d{4}"))
# sod_temp <- sod_temp[years >= 1994 & years <= 2024]
sod_temp <- ls(pattern = "df_sod_")
names <- colnames(get(sod_temp[1]))

# Ensure that all column names are upper case  
for (i in seq_along(sod_temp)) {
  data <- get(sod_temp[[i]]) 
  names_upper <- str_to_upper(colnames(data))
  colnames(data) <- names_upper
  assign(
    sod_temp[i],
    data
  )
}


# Order the columns for standardized column order
for (i in seq_along(sod_temp)) {
  data <- get(sod_temp[[i]])
  data <- data[, names]
  assign(
    sod_temp[i],
    data
  )
  print(paste0("Dataset: ", sod_temp[i]))
}

# Create empty list in which all SOD datasets will be saved
combined_sod <- list()

for (file in seq_along(sod_temp)) {
  combined_sod[[file]] <- get(sod_temp[file])
}



# # Save all datasets within a list object
# for (file in sod_temp) {
#   # Read the .rda file
#   load(paste0(TEMP, "/", file))
#   
#   # Extract df name of file
#   df_name <- str_sub(file, end = -5)
#   
#   # Retrieve the df by string
#   loaded_data <- get(df_name)
#   
#   # Combine Data
#   combined_sod <- append(combined_sod, list(loaded_data))
#   
#   # Avoid littering the global environment
#   rm(loaded_data)
# }

# Remove all single datasets in order to avoid littering the global enivronment
rm(list = sod_temp)

# Rename the variables to lower case variables
names_upper <- names(combined_sod[[1]])
names_lower <- str_to_lower(names_upper)
combined_sod <-  LOWERCASEVAR(combined_sod, names_lower)

# Combine all data frames within the list to one large data frame
combined_sod <-  bind_rows(combined_sod)

# Change object from data.frame to data.tabel for efficieny reasons
setDT(combined_sod)

# Label all variables with their original variables in upper case
for (i in seq_along(combined_sod)) {
  attr(combined_sod[[i]], "label") <- names_upper[i]
}

combined_sod |> 
  filter(year == 2019) |> 
  View()
## 1.3 Basic Data Cleaning -----------------------------------------------------

# Select the variables of interest
combined_sod <- combined_sod[, .(year, stcntybr, uninumbr, depsumbr, insured, 
                                 specdesc, msabr, bkmo, stnumbr, cntynumb, rssdid)]

# Clean variables depsumbr and sims_aquired_date from all special characters (This part can be potentially deleted)
combined_sod <- combined_sod[, depsumbr := gsub(",", "", depsumbr)]

# Format the relevant variables to integers
columns_to_convert <- c("year", "depsumbr", "msabr", "bkmo")
combined_sod <- combined_sod[, (columns_to_convert) := lapply(.SD, as.integer), .SDcols = columns_to_convert]

# Cleaning the variable specdesc from special characters
combined_sod <- combined_sod[, specdesc := str_to_lower(specdesc)]
combined_sod <- combined_sod[, specdesc := gsub(" ", "_", specdesc)]
combined_sod <- combined_sod[, specdesc := gsub(">", "greater", specdesc)]
combined_sod <- combined_sod[, specdesc := gsub("<", "lower", specdesc)]
combined_sod <- combined_sod[, specdesc := gsub("-", "_", specdesc)]
combined_sod <- combined_sod[, specdesc := gsub("\\$", "", specdesc)]

# Create fips-code by combining the state and county code
combined_sod <- combined_sod[stnumbr != "" & cntynumb != ""]
combined_sod <- FIPSCREATOR(data = combined_sod, state_col = "stnumbr", county_col = "cntynumb")

# Excluding the following US territories as they are not relevant for the analysis: 
# Puerto Rico (72), US Virgin Islands (78), American Samoa (60), 
# Northern Marian Islands (69), U.S. Minor Outlying Islands (74), Guam (66)
combined_sod <- combined_sod[!stnumbr %in% c("72", "66", "60", "69", "74", "78")]

# Validate the quality of the fips code by comparing the available fips code in
# the SOD with a list of all fips code in the US from the US Census Bureau (MDR Education)
# -> The SOD contains fips-codes that are not existing. The observations with the invalid fips-codes are excluded. 
# load(paste0(TEMP, "/", "fips_data.rda"))
fips_data <- LOAD(dfinput = "14_thesis_mpt_databasics_fips")
notvalid <- setdiff(fips_data$fips, combined_sod$fips)
combined_sod <- combined_sod[!(fips %in% notvalid)]

# Copy raw sod 
raw_sod <- combined_sod


## 1.4 Collapse combined_sod to county-year level ------------------------------

# Restrict the dataset the year 1999 to 2024
combined_sod <- raw_sod[year >= 1994 & year <= 2024]
  
# Only fips-codes, which are observed over the period of 2000 to 2020 are included
# in the dataset.
# Collapse data to county-year level
check_obs <- combined_sod[, .(fips, year, rssdid)]
check_obs <- check_obs |> distinct(fips, year, rssdid)
check_obs <- check_obs |> distinct(fips, year)

# Irrelevant warning that is supressed. Warning is related to the data.table package.
check_obs <- suppressWarnings(check_obs[, ones := 1])
  
# Determine the counties that are observed over all periods and filter for those counties
county_matrix <- dcast(check_obs, fips ~ year, value.var = "ones", fill = 0)
setDT(county_matrix)
year_nr <- ncol(county_matrix) - 1 # Number of years observed in the restricted dataset
counties_full_obs <- county_matrix[rowSums(county_matrix[ , 2:ncol(county_matrix), with = FALSE] > 0) == year_nr]
combined_sod <- combined_sod[fips %in% counties_full_obs$fips]

# Rename variables and sort columns
setnames(combined_sod, old = c("stnumbr", "cntynumb"), new = c("state", "county"))
setcolorder(combined_sod,c("year", "fips", "state"))
combined_sod[, stcntybr := NULL]

## 1.5 Collapse raw_sod to bank-county-year level ------------------------------

# Select year and variables 
raw_sod <- raw_sod[insured == "CB"]
raw_sod <- raw_sod[, .(year, fips, stnumbr, depsumbr, rssdid)] 
raw_sod <- raw_sod[year >= 1994 & year <= 2024]
setcolorder(raw_sod, c("year", "fips", "stnumbr", "rssdid", "depsumbr"))

# Collapse to bank-county-year level
raw_sod <- raw_sod[, .(depsumcnty = sum(depsumbr)), by = .(year, fips, rssdid)]



## 1.6 Save datasets  ----------------------------------------------------------

# Create two different datasets
# i. Only Commercial banks
sod_banks <- combined_sod[insured == "CB"]
sod_banks <- sod_banks[, insured := NULL]

# Save Combined SOD dataset
SAVE(dfx = sod_banks, namex = MAINNAME)

# Save raw sod dataset
SAVE(dfx = raw_sod, namex = "raw_sod")


################################ END ##########################################+