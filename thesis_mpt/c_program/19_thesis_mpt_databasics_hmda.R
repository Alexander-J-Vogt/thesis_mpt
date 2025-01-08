# TARGET: Reading-In all Raw Datasets  & Perform basic data cleaning
# INDATA: 
# OUTDATA/ OUTPUT:

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
#### MAIN ####

# Explanation:
# This script produces for each year a .rda-file as combining all datasets without 
# firstly collapsing them to county level would lead to a significant increase in
# computing time. Thus, in mp_transmission_otucome, each dataset gets cleaned and 
# is than collapsed to county-level and the county-level hmda datasets are than 
# appended.

# 1. Import Data from the Housing Mortgage Disclosure Act ======================

# The Home Mortgage Disclosure Act (HMDA) contains information on every originated 
# mortgage in the United States. This allows to later create a dataset on mortgage
# loan amount for each county. Thereby, each financial institution is obligated to 
# hand-in information on the mortgage loan, which can be identified by the 
# respondent ID.

##  1.1 List Panel and Loan Application Records Files (LRA) --------------------   
lra_files <- list.files(paste0(A, "p_hmda_lra/"))
lra_files <- lra_files[gsub("[^0-9]", "", lra_files) %in% c(2004:2023)]
panel_files <- list.files(paste0(A, "q_hmda_panel/"))
panel_files <- panel_files[gsub("[^0-9]", "", panel_files) %in% c(2004:2023)]

DEBUG <- F
if (DEBUG) {
  data04 <- fread(paste0(A, "p_hmda_lra/", lra_files[18]), colClasses = "character", nrows = 10)
  data09 <- fread(paste0(A, "p_hmda_lra/", lra_files[9]), colClasses = "character", nrows = 10)
  data10 <- fread(paste0(A, "p_hmda_lra/", lra_files[10]), colClasses = "character", nrows = 10)
  data20 <- fread(paste0(A, "p_hmda_lra/", lra_files[3]), colClasses = "character", nrows = 10)
  
}
start <- Sys.time()


## 1.2 Import LRA files -------------------------------------------------------

# This loop imports all LRA files
purrr::walk(lra_files, function(file) {
  
  # Loop year in string
  loopy <- gsub("[^0-9]", "", file)
  
  # Column name depend on the years of the submission of the LRA as the program
  # has undergone several changes over time.
  if (as.integer(loopy) %in% c(2004:2006)) {
    lra_columns <- c("activity_year", "respondent_id", "agency_code", "loan_amount",
                     "state_code", "county_code", "action_taken", "loan_purpose", 
                     "property_type", "income", "edit_status", "hoepa_status", 
                     "rate_spread", "applicant_sex", "applicant_race_1", "loan_type",
                     "purchaser_type", "lien_status", "occupancy", "msamd")
  } else if (as.integer(loopy) %in% c(2007:2017)) {
    lra_columns <- c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", 
                     "state_code", "county_code", "action_taken", "loan_purpose", 
                     "property_type", "applicant_income_000s", "edit_status",  "hoepa_status",
                     "rate_spread", "applicant_sex", "applicant_race_1", "loan_type",
                     "purchaser_type",  "lien_status", "owner_occupancy", "msamd")
  } else if (as.integer(loopy) %in% c(2018:2023)) {
    lra_columns <- c("activity_year", "lei", "loan_amount", 
                     "state_code", "county_code", "action_taken", "loan_purpose", 
                     "derived_dwelling_category", "income", "hoepa_status",
                     "rate_spread", "applicant_sex", "applicant_race_1", "loan_type",
                     "purchaser_type",  "lien_status", "occupancy_type", "derived_msa_md")
  }
  
  # Load all the raw LRA data on respondent-ID level (contains the information 
  # on each handed out loan). In order to reduce processing time, only the 
  # relevant variables in lra_columns are imported.
  if (DEBUG) {
    r <- 10
  } else {
    r <- Inf
  }
  data <- fread(
    paste0(A, "p_hmda_lra/", file),
    colClasses = "character",
    select = lra_columns,
    nrows = r
    )
  
  # Standardize the column names
  if (as.integer(loopy) %in% c(2007:2017)) {
    setnames(data,
             old = c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", 
                     "state_code", "county_code", "action_taken", "loan_purpose", 
                     "property_type", "applicant_income_000s", "edit_status",  "hoepa_status",
                     "rate_spread", "applicant_sex", "applicant_race_1", "loan_type",
                     "purchaser_type",  "lien_status", "owner_occupancy", "msamd"),
             
             new = c("activity_year", "respondent_id", "agency_code", "loan_amount", 
                     "state_code", "county_code", "action_taken",  "loan_purpose", 
                     "property_type", "income", "edit_status",  "hoepa_status",
                     "rate_spread", "applicant_sex", "applicant_race_1", "loan_type",
                     "purchaser_type",  "lien_status", "occupancy_type", "msamd"))
  } else if (as.integer(loopy) %in% c(2018:2023)) {
    setnames(data,
             old = c("activity_year", "lei", "loan_amount", "state_code", "county_code", 
                     "action_taken", "loan_purpose", "derived_dwelling_category", "income",
                     "hoepa_status", "rate_spread", "applicant_sex", "applicant_race_1",
                     "loan_type", "purchaser_type", "lien_status", "derived_msa_md"),
             
             new = c("activity_year", "lei", "loan_amount", "state_code", "county_code",
                     "action_taken", "loan_purpose", "property_type", "income",
                     "hoepa_status", "rate_spread", "applicant_sex", "applicant_race_1",
                     "loan_type", "purchaser_type",  "lien_status", "msamd"))
  } else if (as.integer(loopy) %in% 2004:2006) {
    setnames(data,
             old = c("activity_year", "respondent_id", "agency_code", "loan_amount", 
                     "state_code", "county_code", "action_taken", "loan_purpose", 
                     "property_type", "income", "edit_status",  "hoepa_status",
                     "rate_spread", "applicant_sex", "applicant_race_1", "loan_type",
                     "purchaser_type",  "lien_status", "occupancy", "msamd"),
             
             new = c("activity_year", "respondent_id", "agency_code", "loan_amount", 
                     "state_code", "county_code", "action_taken",  "loan_purpose", 
                     "property_type", "income", "edit_status",  "hoepa_status",
                     "rate_spread", "applicant_sex", "applicant_race_1", "loan_type",
                     "purchaser_type",  "lien_status", "occupancy_type", "msamd"))
  }
  
  # Save the raw lra dataset
  if (!DEBUG) {
  SAVE(dfx = data, namex = paste0("hmda_lra_", loopy), pattdir = TEMP)
  }
  print(paste0("LRA: Successful import of the year ", loopy))
  
  # Free unused memory and clear object from the  global environment.
  gc()
  rm(data)
})

## 1.3 Import Panel files ------------------------------------------------------

### 1.3.1 Import Panel files ---------------------------------------------------

# Import Panel data and retrieve all unique observation in order to
# get the all unique respondent_id and agency_code combinations.
# This is later needed to identify Commercial Banks that hand out Mortgages with 
# the help of the variable "other_lender_code".

# This loop imports panel data 
purrr::walk(panel_files, function(file) {
  
  if (DEBUG) {
    file <- panel_files[18]
  }
  
  # Get year of panel
  year <- as.integer(gsub("[^0-9]", "", file))
  
  # Check which year of the data is imported and adjust the column names.
  if (year == 2007) {
    # Reading in the file from 2009 needed a manual fix by skipping the last observation.
    # Otherwise, it was not possible to read the file independently of the import 
    # package used. 
    data <- fread(paste0(A, "q_hmda_panel/", file), nrows = 8608, colClasses = "character")
  } else {
    data <- fread(paste0(A, "q_hmda_panel/", file), colClasses = "character")
  }
  
  # Standardize the column names of the panel
  if (year %in% c(2007:2009)) {
    setnames(data, 
             old = c("Respondent Identification Number", "Agency Code", "Other Lender Code", "Assets"), 
             new = c("respondent_id", "agency_code", "other_lender_code", "assets"))
  } else if (year %in% c(2010:2017)) {
    setnames(data, 
             old = c("Respondent ID", "Agency Code", "Other Lender Code", "Respondent RSSD ID", "Assets"), 
             new = c("respondent_id", "agency_code", "other_lender_code", "respondent_rssd", "assets"))
  } else if (year %in% c(2018:2023)) {
    setnames(data,
             old = c("lei", "agency_code", "other_lender_code", "respondent_rssd", "assets"),
             new = c("lei", "agency_code", "other_lender_code", "respondent_rssd", "assets"))
  }
  
  # Select the relevant variables
  if (year %in% c(2004:2006, 2010:2017)) {
    data <- data[, c("respondent_id", "agency_code", "other_lender_code", "respondent_rssd", "assets")]
  } else if (year %in% c(2007:2009)) {
    data <- data[, c("respondent_id", "agency_code", "other_lender_code", "assets")]
  # } else if (year %in% c(2010:2017)) {
  #   data <- data[, c("respondent_id", "agency_code", "other_lender_code", "respondent_rssd", "assets")]
  } else if (year %in% c(2018:2023)) {
    data <- data[, c("lei", "agency_code", "other_lender_code", "respondent_rssd", "assets")]
  }

  # get rid off any duplication
  # as the years 2004 to 2006 are on respondent id, agency code, MSA level 
  if (year %in% c(2004:2017)) {
    data <- unique(data, by = c("respondent_id", "agency_code"))
  } else if (year %in% c(2018:2023)) {
    data <- unique(data, by = c("lei"))
  }
    
  # Save the panel dataset
  SAVE(dfx = data, namex = paste0("hmda_panel_", year), pattdir = TEMP)
  
  # Update on iteration process
  print(paste0("Panel: Successful import of the year ", year))
  
  # Remove objects from the global environment and clean memory
  rm(data)
  gc()
})

### 1.3.2 Recover RSSD for 2007 to 2009 ----------------------------------------

# Use the last year with respondent_rssd before 2007 and first year after 2009 
# in order to recover RSSD
panel_2006 <- LOAD(dfinput = "hmda_panel_2006")
panel_2010 <- LOAD(dfinput = "hmda_panel_2010")

# Adjust Respondent RSSD in order to have character length of 10
panel_2006 <- panel_2006 |> 
  mutate(
    respondent_rssd = str_pad(respondent_rssd, width = 10, side = "left", pad = "0")
  )

# Identify the Respondent ID, which were added between the years 2007 to 2009
panel_setdiff <- setdiff(panel_2010$respondent_id, panel_2006$respondent_id)
panel_2010_subset <- panel_2010[panel_2010$respondent_id %in% panel_setdiff,]

# Combine information of the year 2006 and 2010 on RSSD ID by the Federal Reserve
panel <- rbind(panel_2006, panel_2010_subset)
panel <- panel[, c("respondent_id", "agency_code", "respondent_rssd")]

# Adding the master list of respondent_id, agency_code, respondent_rssd
data_panel <- lapply(2007:2009, function (x) {
  data <- LOAD(dfinput = paste0("hmda_panel_", x))
  data_joined <- left_join(data, panel, by = c("respondent_id", "agency_code"))
  data_joined$year <- x
  return(data_joined)
}) 

# There are some missings in RSSD for the years 2007 to 2009. These are substitute by
# "000000000", which is equivalent to saying that they do not have any RSSD of the 
# Federal Reserves. This is a conservative assumption as these observations should not 
# be deleted.
data_panel <- lapply(data_panel, function (x) {
  x[[5]] <- if_else(is.na(x[[5]]), "0000000000", as.character(x[[5]]))
  return(x)
})

# Saving the adjusted data frames
data_panel <- map(data_panel, function(x) {
  year <- unique(x[[6]])
  x$year <- NULL
  SAVE(dfx = x, namex = paste0("hmda_panel_", year))
  return(x)
})

# Remove not used data
rm(list = c("data_panel", "panel", "panel_2006", "panel_2010", "panel_2010_subset"))
gc()

## 1.4 Merging the Panel and LRA dataset with each other -----------------------

#' In the next step, the final HMDA dataset for each year is produced. 
#' These datasets are all on respondent-ID level, where each respondent can have
#' multiple observations as they have to report every single originated loan.

# Merge both Panel and LRA based on year
purrr::walk(2007:2009, function(i) {

  # Determine the imported LRA and Panel dependent on the year
  lra <- paste0("hmda_lra_", i)
  panel <- paste0("hmda_panel_", i)
  
  # Import the HMDA datasets of the current iteration
  load(file = paste0(TEMP, "/", lra, ".rda"))
  load(file = paste0(TEMP, "/", panel, ".rda"))
  
  # Retrieve the object based on the vector lra and panel
  dflra <- get(lra)
  dfpanel <- get(panel)
  
  # Performs a left_join as we want to keep the observation level of the LRA
  # and want to be able to identify the lender code of each respondents. Thereby,
  # it is important to distinguish between the data structure before and after 2018.
  # - Key before 2018: respondent_id, agency_code
  # - Key from 2018 on: LEI
  if (i < 2018) {
    main <- left_join(dflra, dfpanel, by = c("respondent_id", "agency_code"))
  } else if (i >= 2018) {
    main <- left_join(dflra, dfpanel, by = c("lei"))
  }
  
  # Check if LRA and main dataset have same length: Test to make sure that 
  # no additional observations are added. (Double Check)
  if (nrow(main) == nrow(dflra)) {
    message("Merge successful for year: ", i)
  } else {
    warning("Merging issues for year ", i, ": LRA observations: ", nrow(get("lra")),
            ", Merged observations: ", nrow(main))
  }
  
  # Save the merged file
  SAVE(dfx = main, namex = paste0("hmda_merge_", i))
  
  # Clean up memory and Free unused memory
  rm(list = c(paste0("hmda_lra_", i), paste0("hmda_panel_", i), "main", "dflra", "dfpanel"))
  gc()
})

# Create Sample Size
SAMPLE_SIZE <- 0.01

# Create Sample
purrr::walk(2007:2009, function(x){

  main <- LOAD(dfinput = paste0("hmda_merge_", 2007))
  
  # Save sample dataset
  sample_main <- main |>
    mutate(
      fips = if_else(state_code != "" & county_code != "", paste0(state_code, county_code), NA)
    ) |> 
    drop_na(fips) |> 
    group_by(fips) |> 
    sample_frac(size = SAMPLE_SIZE) |> 
    ungroup()
  
  sample_main <- sample_main |> 
    select(-fips)
  
  # Save sample
  SAVE(dfx = sample_main, namex = paste0("hmda_sample_", x))
  
  rm(main)
  gc()
  
  # Message
  check <- if_else(nrow(sample_main) > 0, "YES", "NO")
  
  message(paste0("Sample saved for year ", x, ". And no empty DF: ", check))
  
})


end <- Sys.time()
diff <- end - start
message(paste0("The code took: ", diff))

# 2. Data Basics ===============================================================  

hmda_merged <- list.files(path = TEMP, pattern = "hmda_merge_")
hmda_merged <- hmda_merged[as.integer(gsub("[^0-9]", "", hmda_merged)) >= 2004]
hmda_merged <- gsub(".rda", "", hmda_merged)

# Start to clean all years
purrr::walk(seq_along(hmda_merged), function(x) {
  
  # Determine file
  file <- hmda_merged[x]

  # Determine the year of data
  year <- as.integer(gsub("[^0-9]", "", file))

  # Update message
  message(paste0("\n",VISUALSEP))
  message(paste0("Start to clean data for the year ", year, "."))
  
  # Load hmda_merged of this loop iteration
  data <- LOAD(dfinput = file, dfextension = ".rda")
  setDT(data)
  
  ## 2.1 Formatting Variables --------------------------------------------------

  # Columns as character
  if (year >= 2018) {
    chr_cols <- c("lei", "state_code", "county_code", "property_type", "respondent_rssd", "msamd")
  } else if (year < 2018) {
    chr_cols <- c("respondent_id", "state_code", "county_code", "property_type", "respondent_rssd", "edit_status", "msamd")
  }
  
  # Columns as numeric
  num_cols <- c("activity_year", "loan_amount", "action_taken", "loan_purpose", "income",
                "hoepa_status", "rate_spread", "applicant_sex", "applicant_race_1",
                "loan_type", "agency_code", "other_lender_code", "assets", "lien_status",
                "occupancy_type", "purchaser_type")
  
  # Formatting the columns
  data[, (chr_cols) := lapply(.SD, as.character), .SDcols = chr_cols]
  data[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  
  # Formatting edit_status for the years before 2018
  if (year < 2018) {
    data[, edit_status := fifelse(edit_status == "", NA, edit_status)]
    data[, edit_status := as.numeric(edit_status)]
  }


  ## 2.2 Variable Transformations based on time-varying Data Structure ---------

  # Variable Transformation for the years after 2017
  if (year >= 2018) {
    
    # Basic Variable Transformations
    data[, `:=` (
    
      # Detect NAs in lei
      lei = fifelse(lei == "", NA, lei),
      
      # Change unit of loan amount in 000s
      loan_amount = loan_amount / 1000,
      
      # Extract state code number
      state_code = fifelse(!is.na(county_code), str_sub(county_code, start = 1, end = 2), county_code),
      
      # Clean Respondent RSSD by Federal Reserve
      respondent_rssd = str_pad(respondent_rssd, width = 10, side = "left", pad = "0"),
      
      # Replace -1 by NA in assets variable
      assets = fifelse(assets < 0, NA, assets),
      
      # Introduce empty edit_status variable (not existing in the years after 2017)
      edit_status = as.numeric(NA)
      
      )]
    
    # Recode property type in order to match the data structure before 2018
    data <- data[, property_type := fifelse(property_type == "Single Family (1-4 Units):Site-Built", "1", property_type)]
    data <- data[, property_type := fifelse(property_type %in% c("Single Family (1-4 Units):Manufactured", "Multifamily:Manufactured"), "2", property_type)]
    data <- data[, property_type := fifelse(property_type == "Multifamily:Site-Built", "3", property_type)]
    data <- data[, property_type := as.numeric(property_type)]
    
    ## Recode loan purpose
    # Post-2017: more granular documentation of loan purpose
    # Relevant for the later analysis is 1- Home Purchase & 3 - Refinancing
    data[loan_purpose == 31, loan_purpose := 3]
    data[loan_purpose == 32, loan_purpose := 3]
    
    # Unify lei and fips code across all years
    setnames(data,
             old = c("lei", "county_code"),
             new = c("id", "fips")
             )
    
  # Variable Transformation for the years before 2018
  } else if (year < 2018) {
    
    # Clear the year 2012 from a trailing "0" at the end
    if (year == 2012) {
      data[, respondent_rssd := str_sub(respondent_rssd, start = 1, end = nchar(respondent_rssd) - 1)]
    }
    
    # Basic Variable Transformations
    data[, `:=` (
      
      # Detect NAs in respondent_id
      respondent_id = fifelse(respondent_id == "", NA, respondent_id),
      
      # Account for missings leading zeros in state_code and county_code
      state_code = fifelse(state_code != "", str_pad(state_code, width = 2, side = "left", pad = "0"), NA),
      county_code = fifelse(county_code != "", str_pad(county_code, width = 3, side = "left", pad = "0"), NA),
      
      # Clean Respondent RSSD by Federal Reserve
      respondent_rssd = fifelse(nchar(respondent_rssd) != 10, str_pad(respondent_rssd, width = 10, side = "left", pad = "0"), respondent_rssd)
    )]
    
    # Unify respondent_id across years
    setnames(data,
             old = "respondent_id",
             new = "id"
             )
    
    # Unify fips across years
    data[, fips := fifelse(nchar(state_code) > 0 & nchar(county_code) > 0, paste0(state_code, county_code), "")]
    data[, county_code := NULL]
  }

  
  ## 2.3 Variable Transformation for all data structures -----------------------

  # Adjust year variable
  setnames(data,
           old = "activity_year",
           new = "year"
           )
  
  # Order columns
  setcolorder(data, 
              c("year", "id", "agency_code", "state_code", "fips", "msamd", "action_taken", "loan_purpose",
                "property_type", "loan_type", "hoepa_status", "loan_amount", "lien_status", "occupancy_type", 
                "income", "rate_spread", "applicant_sex", "applicant_race_1", "purchaser_type", "other_lender_code",
                "respondent_rssd", "assets", "edit_status")
              )
  
  # Update message
  message("Finished Basic Transformations.")


  ### Basic Filter for ... -----------------------------------------------------

  ## Only include states and Washington D.C. BUT no U.S. territory
  data <- data[!state_code %in% c("66", "60", "69","72", "74", "78")]
  
  # Originated Loans 
  data <- data[action_taken == 1]
  
  # One-to-Four-Family Dwellings 
  data <- data[property_type == 1]

  # First Mortgage Lien (No second mortgage on a house - interest rate are higher on these)
  data <- data[lien_status == 1]

  # Principle Residence (in order to exclude any investors)
  data <- data[occupancy_type == 1]
  
  # Missings in Loan Amount, FIPS-Code, Income 
  data <- data[!is.na(fips)] # Exclude missing FIPS-code (There are multiple reasons on why they can miss. See: HMDA guide 2010)
  data <- data[!is.na(loan_amount)] # Should be not missing but just to be extra sure this line of code excludes all NAs 
  data <- data[!is.na(loan_purpose)] # Exclude missings in loan purpose
  
  # Exclude Observation with individual having an annual income of more than 100 Billion (see year 2018)
  data <- data[, income_chr := nchar(as.character(income))]
  data <- data[income_chr != 9]
  data <- data[, income_chr := NULL]
  
  # Exclude Observation with income of 9999 (before 2018 this was the limit for income)
  # in order to maintain precision of the analysis
  if (year < 2018) data <- data[income < 9999]
    
  # Home Purchase & Refinancing (for now to evaluate leave all loan purposes in)
  data <- data[loan_purpose %in% c(1,3)]
  
  # Depository Institutions (0) & mortgage banks subsidiary of commercial bank (1)
  data <- data[other_lender_code %in% c(0, 1)]
  
  # Delete not relevant variables
  data[, `:=` (
    action_taken = NULL,
    property_type = NULL,
    lien_status = NULL,
    occupancy_type = NULL
  )]
  
  # Update message
  message("Finished Basic Filtering.")


  ## 2.4 Calculation of Common Characteristics ---------------------------------

  ### 2.4.1 Calculation on Loan Application Level ------------------------------

  # Loan-to-Income Ratio
  data[, lti_ratio := fifelse(!is.na(loan_amount) & !is.na(income), loan_amount / income, NA)]
  
  # log loan amount
  data[, log_loan_amount := log(loan_amount)]


  ### 2.4.2  Calculations by FIPS ----------------------------------------------

  # Total number of observations by county 
  data[, tot_origin := .N, by = fips] # total loan origination
  
  # Total Number of ...
  # Race
  data[, nr_white_applicant := as.numeric(sum(applicant_race_1 == 5, na.rm = TRUE)), by = fips]
  data[, nr_black_applicant := sum(applicant_race_1 == 3, na.rm = TRUE), by = fips]
  data[, nr_asian_applicant := sum(applicant_race_1 == 2, na.rm = TRUE), by = fips]
  data[, nr_americanindian_applicant := sum(applicant_race_1 == 1, na.rm = TRUE), by = fips]
  data[, nr_others_applicant := sum(!applicant_race_1 %in% c(1, 2, 3, 5), na.rm = TRUE), by = fips]
  
  # Sex
  data[, nr_male_applicant := sum(applicant_sex == 1, na.rm = TRUE), by = fips]
  data[, nr_female_applicant := sum(applicant_sex == 2, na.rm = TRUE), by = fips]
  
  
  # Share of ...
  # Race
  data[, share_white_applicant := nr_white_applicant / tot_origin]
  data[, share_black_applicant := nr_black_applicant / tot_origin]
  data[, share_asian_applicant := nr_asian_applicant / tot_origin]
  data[, share_americanindian_applicant := nr_americanindian_applicant / tot_origin]
  data[, share_others_applicant := nr_others_applicant / tot_origin]
  
  # Sex
  data[, share_male_applicant := nr_male_applicant / tot_origin]
  data[, share_female_applicant := nr_female_applicant / tot_origin]

  
  # Delete nr_* variables
  data[, grep("^nr_", names(data), value = TRUE) := NULL]
  
  # Median loan by fips
  data[, income_median := fquantile(income, probs = .5), by = fips]
  
  ## Save cleaned data set
  SAVE(dfx = data, namex = paste0("hmda_clean_", year))
  
  ## Save a sample of the dataset
  frac <- 0.01
  data_sample <- data[, .SD[sample(.N, size = max(1, .N * frac))], by = fips]
  SAVE(dfx = data_sample, namex = paste0("hmda_clean_sample_", year))
  
  # Update message
  message("End of data cleaning.")
  message(paste0("Observations: ", nrow(data)))
  message(paste0("File Size: ", object.size(data) / (1024^2), " MB."))
  
  } # End of function
) # End of purrr::walk


############################### END ###########################################+