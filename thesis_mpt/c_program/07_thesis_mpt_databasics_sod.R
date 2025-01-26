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
  
  # Extract year
  year <- str_sub(i, start = 5, end = 8)

  # Import data   
  assign(
    paste0("df_sod_", year),
    read_csv(
      file = paste0(A, "f_sod/", i), 
      col_types = cols(.default = "c"),
      progress = FALSE
      )
  )
    
  # Remover garbage
  gc()
  
  # Update Message
  print(paste0("Iteration Status: ", i))
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

# Save datasets in file
for (file in seq_along(sod_temp)) {
  combined_sod[[file]] <- get(sod_temp[file])
}

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

# Save Raw Dataset
SAVE(dfx = combined_sod, namex = "SOD_raw")


## 1.3 Basic Data Cleaning -----------------------------------------------------

# Import data
combined_sod <- LOAD(dfinput = "SOD_raw")
setDT(combined_sod)

# Select the variables of interest
combined_sod <- combined_sod[, .(year, stcntybr, uninumbr, depsumbr, insured, 
                                 specdesc, msabr, bkmo, stnumbr, cntynumb, rssdid)]

# Clean variables depsumbr and sims_aquired_date from all special characters (This part can be potentially deleted)
combined_sod[, depsumbr := gsub(",", "", depsumbr)]

# Format the relevant variables to integers
columns_to_convert <- c("year", "depsumbr", "msabr", "bkmo")
combined_sod[, (columns_to_convert) := lapply(.SD, as.integer), .SDcols = columns_to_convert]

# Cleaning the variable specdesc from special characters
combined_sod[, specdesc := str_to_lower(specdesc)]
combined_sod[, specdesc := gsub(" ", "_", specdesc)]
combined_sod[, specdesc := gsub(">", "greater", specdesc)]
combined_sod[, specdesc := gsub("<", "lower", specdesc)]
combined_sod[, specdesc := gsub("-", "_", specdesc)]
combined_sod[, specdesc := gsub("\\$", "", specdesc)]

# Create fips-code by combining the state and county code
combined_sod <- combined_sod[stnumbr != "" & cntynumb != ""]
combined_sod <- FIPSCREATOR(data = combined_sod, state_col = "stnumbr", county_col = "cntynumb")

# Excluding the following US territories as they are not relevant for the analysis: 
# Puerto Rico (72), US Virgin Islands (78), American Samoa (60), 
# Northern Marian Islands (69), U.S. Minor Outlying Islands (74), Guam (66)
combined_sod <- combined_sod[stnumbr %in% c(
  "01", "02", "04", "05", "06", "08", "09", "10", "12", "13",
  "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
  "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
  "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
  "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "11"
)]

# Filter for Commercial Banks (CB) and Savings Institutions (SA)
combined_sod <- combined_sod[insured %in% c("CB", "SA")]

# Delete irrelevant variable
combined_sod[, stcntybr := NULL]
combined_sod[, cntynumb := NULL]

# Order Columns
setcolorder(combined_sod, c("year", "stnumbr", "fips", "msabr", "uninumbr", "bkmo", "depsumbr", "specdesc", "rssdid", "insured" ))

# Rename
setnames(
  combined_sod,
  old = "stnumbr",
  new = "state"
)


## 1.4 Create Bank-County-Year-Level & Raw Dataset ------------------------------

# Select year and variables 
sod_county_level <- raw_sod[, .(year, fips, stnumbr, depsumbr, rssdid)] 
setcolorder(sod_county_level, c("year", "fips", "state_code", "rssdid", "depsumbr"))

# Collapse to bank-county-year level
sod_county_level <- sod_county_level[, .(depsum_bank_cnty = sum(depsumbr)), by = .(year, fips, rssdid)]


## 1.5 Save datasets  ----------------------------------------------------------

# Save Raw SOD dataset - NOT Collapsed
SAVE(dfx = raw_sod, namex = MAINNAME)

# Save SOD - Collapsed to Bank-County-Year level
SAVE(dfx = sod_county_level, namex = paste0(MAINNAME, "_bank_county_year_level"))


################################ END ##########################################+