# TARGET: Import QWI 
# INDATA: qwi_e14db0de913c427aa12de971a73eb389.csv
# OUTDATA/ OUTPUT: mp_transmission_databasics_qwi

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
# MAIN PART ####

# 01. Import of SAIPE ==========================================================

# Small Area Income and Poverty Estimates (SAIPE)

# List all files
files <- list.files(paste0(A, "l_saipe/"))
files_xls <- files[str_detect(files, ".xls$")]
files_txt <- files[str_detect(files, ".txt$")]


## 01.1 Import .text files 2000 to 2002-----------------------------------------

# Set manually the width of each column
widths <- c(3, 4, 9, 9, 9, 5, 5, 5, 9, 9, 9, 5, 5, 5, 9, 9, 9, 5, 5, 5, 7, 7, 7, 8, 8, 8, 5, 5, 5, 45, 4)

# Import datasets
file_txt_1 <- files_txt[1:3]

for (i in file_txt_1) {
  
  # Extract year
  year <- str_sub(i, start = 4, end = 5)
  
  # Import Data
  assign(
    paste0("data_", year),
    read_fwf(
      file = paste0(A, "l_saipe/", i),
      col_positions = fwf_widths(widths),
      col_types = cols(.default = "c")
    )
  )
  
  # Create year variable
  assign(
    paste0("data_", year),
    mutate(get(paste0("data_", year)), year = as.numeric(paste0("20", year)))
  )
}


## 01.2 Import .txt files: 1999 and 1999 --------------------------------------

# Import dataset
file_txt_2 <- files_txt[4:length(files_txt)]

for (i in file_txt_2) {
  
  # Extract year
  year <- str_sub(i, start = 4, end = 5)
  
  # Import Data
  assign(
    paste0("data_", year),
    read_fwf(
      file = paste0(A, "l_saipe/", i),
      col_positions = fwf_widths(widths),
      col_types = cols(.default = "c")
    )
  )
  
  # Create year variable
  assign(
    paste0("data_", year),
    mutate(get(paste0("data_", year)), year = as.numeric(paste0("19", year)))
  )
}
 
                
## 01.3 Import .xls files: 2003 to 2004 ---------------------------------------

# Import dataset
files_xls_1 <- files_xls[1:2]

for (i in files_xls_1) {
  
  # Extract year
  year <- str_sub(i, start = 4, end = 5)
  
  # Import Data
  assign(
    paste0("data_", year),
    read_xls(
      path = paste0(A, "l_saipe/", i),
      skip = 1,
      col_types = "text"
      )
  )
  
  # Create year variable
  assign(
    paste0("data_", year),
    mutate(get(paste0("data_", year)), year = as.numeric(paste0("20", year)))
  )
}


## 01.4 Import .xls files: 2005 to 2012 ----------------------------------------

# Import dataset
files_xls_2 <- files_xls[3:10]

for (i in files_xls_2) {
  
  # Extract year
  year <- str_sub(i, start = 4, end = 5)
  
  # Import Data
  assign(
    paste0("data_", year),
    read_xls(
      path = paste0(A, "l_saipe/", i),
      skip = 2,
      col_types = "text"
    )
  )
  
  # Create year variable
  assign(
    paste0("data_", year),
    mutate(get(paste0("data_", year)), year = as.numeric(paste0("20", year)))
  )
}


## 01.5 Import .xls files: 2005 to 2012 ---------------------------------------------

# Import dataset
files_xls_3 <- files_xls[11:length(files_xls)]

for (i in files_xls_3) {
  
  # Extract year
  year <- str_sub(i, start = 4, end = 5)
  
  # Import Data
  assign(
    paste0("data_", year),
    read_xls(
      path = paste0(A, "l_saipe/", i),
      skip = 3,
      col_types = "text"
    )
  )
  
  # Create year variable
  assign(
    paste0("data_", year),
    mutate(get(paste0("data_", year)), year = as.numeric(paste0("20", year)))
  )
}


# 02. Create Dataset on Poverty and Median Household Income ====================

# List all dataset in the environment
envir <- ls(pattern = "^data_")

## 02.1 Change columns names from 2003 to 2021 ---------------------------------

# Create variable names
names <- colnames(data_03)
names <- str_replace_all(names, " ", "_")
names <- str_replace_all(names, "\\.\\.\\.\\d{1,2}$", "")
names <- str_to_lower(names)
names <- str_replace_all(names, "^90%_(.*)$", "\\1_90perc")
names <- str_replace_all(names, "-", "_")

# Standardize variable names
for (x_name in envir[4:(length(envir)-2)]) {
  data <- get(x_name)  
  colnames(data) <- names
  assign(x_name, data, envir = .GlobalEnv)
}

## 02.2 Change columns names from 2000 to 2002 & 1999 and 1998 -----------------

# Standardize variable names
for (i in envir[-c(4:(length(envir)-2))]) {
  data <- get(i)
  data <- data |> 
    relocate(X31, X30, .after = X2)
  colnames(data) <- names
  assign(i, data, envir = .GlobalEnv)
}

# 03. Append Data ==============================================================

# Select relevant variables
vars <- c("state_fips", "year","county_fips", "postal_code", "name", "poverty_estimate_all_ages",
         "median_household_income" )

for (i in envir) {
  data <- get(i)
  data <- data[, vars]
  assign(
    paste0(i),
    data
  )
}

# Create Main Raw Dataset
df_saipe_raw <- list() 

for (i in seq_along(envir)) {
  df_saipe_raw[[i]] <- get(envir[[i]])
  print(paste0("Dataset: ", i , " ...DONE"))
}

df_saipe_raw <- bind_rows(df_saipe_raw)

# 04. Basic Data Cleaning ======================================================

# Create FIPS ID
df_saipe <- FIPSCREATOR(df_saipe_raw, state_col = "state_fips", county_col = "county_fips")
  
df_saipe <- df_saipe |> 
  filter(county_fips != "000") |>  # Filter all state and national level observation
  select(fips, year, poverty_estimate_all_ages, median_household_income) |> 
  mutate(across(c(2:4), as.numeric)) |> 
  arrange(fips, year)
  
# 05. Save =====================================================================

# Save dataset on county-year level
SAVE(dfx = df_saipe)

################################ END ##########################################+