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
purrr::walk(data_panel, function(x) {
  year <- unique(x[[6]])
  SAVE(dfx = x, namex = paste0("hmda_panel_", year))
})

# Remove not used data
rm(list = c("data_panel", "panel", "panel_2006", "panel_2010", "panel_2010_subset"))
gc()

## 1.4 Merging the Panel and LRA dataset with each other -----------------------

#' In the next step, the final HMDA dataset for each year is produced. 
#' These datasets are all on respondent-ID level, where each respondent can have
#' multiple observations as they have to report every single originated loan.

# Merge both Panel and LRA based on year
purrr::walk(2004:2023, function(i) {
  
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

end <- Sys.time()
diff <- end - start
message(paste0("The code took: ", diff))

# 2. Data Basics ===============================================================  

hmda_merged <- list.files(path = TEMP, pattern = "hmda_merge_")
hmda_merged <- hmda_merged[as.integer(gsub("[^0-9]", "", hmda_merged)) >= 2004]
hmda_merged <- gsub(".rda", "", hmda_merged)

x <- hmda_merged[19]
y <- hmda_merged[10]

datax <- LOAD(dfinput = x)
datax <- head(datax, n = 1000000)
datay <- LOAD(dfinput = y)
datay <- head(datay, n = 1000000)
setDT(datax)
setDT(datay)

## CURRENT DATASET
data <- datay
setDT(data)

# Initiate list
desc_stats_tot_list <- list()
desc_stats_county_list <- list()
fips_detailed_list <- list()
fips_missings_list <- list()
loan_amount_dist_list <- list()

# Start to clean all years
purrr::walk(seq_along(hmda_merged), function(x) {

  # Determine file
  file <- hmda_merged[x]
    
  # Determine the year of data
  year <- as.integer(gsub("[^0-9]", "", file))
  
  # Determine lowest year of hmda_merged
  low_year_plus1 <- min(as.integer(gsub("[^0-9]", "", hmda_merged))) - 1
  
  # Load hmda_merged of this loop iteration
  data <- LOAD(dfinput = paste0(TEMP, file))

## 2.1 Formatting Variables ----------------------------------------------------

  # Columns as character
  if (year >= 2018) {
    chr_cols <- c("lei", "state_code", "county_code", "property_type", "respondent_rssd")
  } else if (year < 2018) {
    chr_cols <- c("respondent_id", "state_code", "county_code", "property_type", "respondent_rssd", "edit_status")
  }
  
  # Columns as numeric
  num_cols <- c("activity_year", "loan_amount", "action_taken", "loan_purpose", "income",
                "hoepa_status", "rate_spread", "applicant_sex", "applicant_race_1",
                "loan_type", "agency_code", "other_lender_code", "assets", "lien_status",
                "occupancy_type")
  
  # Formatting the columns
  data[, (chr_cols) := lapply(.SD, as.character), .SDcols = chr_cols]
  data[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  
  # Formatting edit_status for the years before 2018
  if (year < 2018) {
    data[, edit_status := fifelse(edit_status == "", NA, edit_status)]
    data[, edit_status := as.numeric(edit_status)]
  }


## 2.2 Variable Transformations based on time-varying Data Structure -----------

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


## 2.3 Variable Transformation for all data structures -------------------------

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


### Basic Filter for ... -------------------------------------------------------

### Determine Missing Counties | Detailed --------------------------------------
  
  fips_before_filter <- unique(data$fips)
  
  ## Only include states and Washington D.C. BUT no U.S. territory
  data <- data[!state_code %in% c("66", "60", "69","72", "74", "78")]
  
  # Determine missing counties
  fips_after_noterritory <- unique(data$fips)
  diff_after_noterritory <- setdiff(fips_before_filter, fips_after_noterritory)
  
  # Originated Loans 
  data <- data[action_taken == 1]
  
  # Determine missing counties
  fips_after_org <- unique(data$fips)
  diff_after_org <- setdiff(fips_after_noterritory, fips_after_org)
  
  # One-to-Four-Family Dwellings 
  data <- data[property_type == 1]
  
  # Determine missing counties
  fips_after_onetofour <- unique(data$fips)
  diff_after_onetofour <- setdiff(fips_after_org, fips_after_onetofour)
  
  # First Mortgage Lien (No second mortgage on a house - interest rate are higher on these)
  data <- data[lien_status == 1]
  
  # Determine missing counties
  fips_after_firstlien <- unique(data$fips)
  diff_after_firstlien <- setdiff(fips_after_onetofour, fips_after_firstlien)
  
  # Principle Residence (in order to exclude any investors)
  data <- data[occupancy_type == 1]
  
  # Determine missing counties
  fips_after_occu <- unique(data$fips)
  diff_after_occu <- setdiff(fips_after_firstlien, fips_after_occu)
  
  # Missings in Loan Amount, FIPS-Code, Income 
  data <- data[!is.na(fips)] # Exclude missing FIPS-code (There are multiple reasons on why they can miss. See: HMDA guide 2010)
  data <- data[!is.na(loan_amount)] # Should be not missing but just to be extra sure this line of code excludes all NAs 
  
  # Determine missing counties
  fips_after_na <- unique(data$fips)
  diff_after_na <- setdiff(fips_after_occu, fips_after_na)
  
  # Missing Counties between filterting for originated loans and filtering for missings
  diff_org_na <- setdiff(fips_after_org, fips_after_na)
  diff_org_na <- diff_org_na[!is.na(diff_org_na)]
  
  # Save detailed missing counties
  diff_vectors <- list(diff_after_noterritory, diff_after_org, diff_after_onetofour, diff_after_firstlien, diff_after_occu, diff_after_na, diff_org_na)
  max_length <- max(sapply(diff_vectors, length))
  
  # Create DF
  padded_vectors <- lapply(diff_vectors, function(x) c(x, rep(NA, max_length - length(x))))
  df_fips_detailed <- as.data.frame(do.call(cbind, padded_vectors))
  df_fips_detailed$year <- year
  fips_detailed_names <- c("diff_after_noterritory", "diff_after_org", "diff_after_onetofour", "diff_after_firstlien", "diff_after_occu", "diff_after_na", "diff_org_na", "year")
  colnames(df_fips_detailed) <- fips_detailed_names
  
  # Save in list
  fips_detailed_list[[year - low_year_plus1]] <- df_fips_detailed
  names(fips_detailed_list)[[year - low_year_plus1]] <- paste0("hdma_", year)


### Determine Missing Counties | General ---------------------------------------

  # Final number of counties
  fips_after_filter <- unique(data$fips)
  
  # List of Counties by TIGRIS
  if (year < 2010) {
    fips_raw <- counties(year = 2000, progress_bar = FALSE)
  } else {
    fips_raw <- counties(year = year, progress_bar = FALSE)
  }
  
  # Basic Transformations for a base dataset on available FIPS Codes
  fips <- fips_raw |>
    as_tibble() |> 
    select(STATEFP, GEOID, NAMELSAD) |> 
    filter(!STATEFP %in% c("66", "60", "69","72", "74", "78")) |> # 50 States + Washington D.C.
    arrange(STATEFP, GEOID)
  
  # Determine which of the counties are missing
  diff_missings_counties <- setdiff(fips$GEOID, fips_after_filter)
  
  # Create DF and add information on state and county name
  df_missings <- data.frame(county_code = diff_missings_counties)
  
  # Load data on fips code and names
  data(fips_codes)
  
  # select relevant variables
  fips_code <- fips_codes |> 
    mutate(
      county_code = paste(state_code, county_code, sep = "")
    ) |> 
    select(county_code, state_name, county)
  
  # Join Missing Counties with Information
  df_missings <- fips_code |> 
    left_join(df_missings, by = "county_code")
  
  # Save the DF into the list
  fips_missings_list[[year - low_year_plus1]] <- df_missings
  names(fips_missings_list)[[year - low_year_plus1]] <- paste0("hdma_", year)


# Home Purchase & Refinancing (for now to evaluate leave all loan purposes in)
# data <- data[loan_purpose %in% c(1,3)]

  # Delete not relevant variables
  data[, `:=` (
    action_taken = NULL,
    property_type = NULL,
    lien_status = NULL,
    occupancy_type = NULL
  )]

## 2.4 Calculation of Common Characteristics -----------------------------------

### 2.4.1 Calculation on Loan Application Level --------------------------------

  # Loan-to-Income Ratio
  data[, lti_ratio := fifelse(!is.na(loan_amount) & !is.na(income), loan_amount / income, NA)]


### 2.4.2  Calculations by FIPS ------------------------------------------------

  # Share of Originated, Rejected and Purchased loans by Institutions 
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


### 2.4.3 Calculation in Total -------------------------------------------------

  # Collect Descriptive Stats in DF
  df_descr_stats_tot <- data.frame()
  df_descr_stats_county <- data.frame()
  
  ## Key Numbers for ...
  # General
  tot_rate_spread <- sum(!is.na(data$rate_spread), na.rm = TRUE)
  tot_origin_all <-  nrow(data)
  
  # Sex
  tot_male <- sum(data$applicant_sex == 1, na.rm = TRUE)
  tot_female <- sum(data$applicant_sex == 2, na.rm = TRUE)
  
  # Race
  tot_white <- sum(data$applicant_race_1 == 5, na.rm = TRUE)
  tot_black <- sum(data$applicant_race_1 == 3, na.rm = TRUE)
  tot_asian <- sum(data$applicant_race_1 == 2, na.rm = TRUE)
  tot_americanindian <- sum(data$applicant_race_1 == 1, na.rm = TRUE)
  tot_others <- sum(!data$applicant_race_1 %in% c(1, 2, 3, 5), na.rm = TRUE)
  
  # Save Descriptive Statistics for Total Population in DF
  df_descr_stats_tot <- data |> 
    summarise(
      year = year,
      share_male_applicant = tot_male / tot_origin_all * 100,
      share_female_applicant = tot_female / tot_origin_all * 100,
      share_white_applicant = tot_white / tot_origin_all * 100,
      share_black_applicant = tot_black / tot_origin_all * 100,
      share_asian_applicant = tot_asian / tot_origin_all * 100,
      share_american_indian = tot_americanindian / tot_origin_all * 100,
      share_others = tot_others / tot_origin_all * 100,
      rate_spread_NA = tot_rate_spread,
      rate_spread_min = min(rate_spread, na.rm= TRUE),
      rate_spread_q25 = quantile(rate_spread, probs = .25, na.rm = TRUE),
      rate_spread_median = median(rate_spread, na.rm = TRUE),
      rate_spread_mean = mean(rate_spread, na.rm = TRUE),
      rate_spread_q75 = quantile(rate_spread, probs = .75, na.rm = TRUE),
      rate_spread_max = max(rate_spread, na.rm= TRUE),
      income_NA = sum(!is.na(income)),
      income_min = min(income, na.rm= TRUE),
      income_q25 = quantile(income, probs = .25, na.rm = TRUE),
      income_median = median(income, na.rm = TRUE),
      income_mean = mean(income, na.rm = TRUE),
      income_q75 = quantile(income, probs = .75, na.rm = TRUE),
      income_max = max(income, na.rm= TRUE),
      loan_amount_NA = sum(!is.na(loan_amount)),
      loan_amount_min = min(loan_amount, na.rm= TRUE),
      loan_amount_q25 = quantile(loan_amount, probs = .25, na.rm = TRUE),
      loan_amount_median = median(loan_amount, na.rm = TRUE),
      loan_amount_mean = mean(loan_amount, na.rm = TRUE),
      loan_amount_q75 = quantile(loan_amount, probs = .75, na.rm = TRUE),
      loan_amount_max = max(loan_amount, na.rm= TRUE),
      hoepa_high_cost = sum(hoepa_status == 1),
      hoepa_share_high_cost = sum(hoepa_status == 1) / tot_origin_all,
      hoepa_non_high_cost = sum(hoepa_status == 2),
      hoepa_share_non_high_cost = sum(hoepa_status == 2) / tot_origin_all,
      loan_purpose_hp = sum(loan_type == 1) / tot_origin_all,
      loan_purpose_hi = sum(loan_type == 2) / tot_origin_all,
      loan_purpose_refin = sum(loan_type == 2) / tot_origin_all
    )
  
  df_descr_stats_tot[[year - low_year_plus1]] <- graph
  names(df_descr_stats_tot)[[year - low_year_plus1]] <- paste0("HDMA_", year)
  

### 2.4.3 Calculation by FIPS -------------------------------------------------
  
  # Save Descriptive Statistics by FIPS in DF
  df_descr_stats_county <- data |> 
    group_by(fips) |> 
    reframe(
      year = year,
      share_male_applicant = sum(applicant_sex == 1) / n() * 100,
      share_female_applicant = sum(applicant_sex == 2) / n() * 100,
      share_white_applicant = sum(applicant_race_1 == 5) / n() * 100,
      share_black_applicant = sum(applicant_race_1 == 3) / n() * 100,
      share_asian_applicant = sum(applicant_race_1 == 2) / n() * 100,
      share_american_indian = sum(applicant_race_1 == 1) / n() * 100,
      share_others = sum(!applicant_race_1 %in% c(1, 2, 3, 5)) / n() * 100,
      rate_spread_exist = any(!is.na(rate_spread)),
      rate_spread_NA = sum(is.na(rate_spread)),
      rate_spread_min = if (rate_spread_exist) min(rate_spread, na.rm = TRUE) else NA,
      rate_spread_q25 = if (rate_spread_exist) quantile(rate_spread, probs = .25, na.rm = TRUE) else NA,
      rate_spread_median = if (rate_spread_exist) median(rate_spread, na.rm = TRUE) else NA,
      rate_spread_mean = if (rate_spread_exist) mean(rate_spread, na.rm = TRUE) else NA,
      rate_spread_q75 = if (rate_spread_exist) quantile(rate_spread, probs = .75, na.rm = TRUE) else NA,
      rate_spread_max = if (rate_spread_exist) max(rate_spread, na.rm = TRUE) else NA,
      income_exist = any(!is.na(income)),
      income_NA = sum(is.na(income)),
      income_min = if (income_exist) min(income, na.rm= TRUE) else NA,
      income_q25 = if (income_exist) quantile(income, probs = .25, na.rm = TRUE) else NA,
      income_median = if (income_exist) median(income, na.rm = TRUE) else NA,
      income_mean = if (income_exist) mean(income, na.rm = TRUE) else NA,
      income_q75 = if (income_exist) quantile(income, probs = .75, na.rm = TRUE) else NA,
      income_max = if (income_exist) max(income, na.rm= TRUE) else NA,
      loan_amount_NA = sum(is.na(loan_amount)),
      loan_amount_min = min(loan_amount, na.rm= TRUE),
      loan_amount_q25 = quantile(loan_amount, probs = .25, na.rm = TRUE),
      loan_amount_median = median(loan_amount, na.rm = TRUE),
      loan_amount_mean = mean(loan_amount, na.rm = TRUE),
      loan_amount_q75 = quantile(loan_amount, probs = .75, na.rm = TRUE),
      loan_amount_max = max(loan_amount, na.rm= TRUE),
      hoepa_high_cost = sum(hoepa_status == 1),
      hoepa_share_high_cost = sum(hoepa_status == 1) / n() * 100,
      hoepa_non_high_cost = sum(hoepa_status == 2),
      hoepa_share_non_high_cost = sum(hoepa_status == 2) / n() * 100,
      loan_purpose_hp = sum(loan_type == 1) / n() * 100,
      loan_purpose_hi = sum(loan_type == 2) / n() * 100,
      loan_purpose_refin = sum(loan_type == 2) / n() * 100
    )
  
  desc_stats_county_list[[year - low_year_plus1]] <- df_descr_stats_county
  names(desc_stats_county_list)[[year - low_year_plus1]] <- paste0("HDMA_", year)
  
  # Analyse the loan_amount distribution
  graph <- ggplot(data = data, aes(loan_amount)) +
    geom_density(fill = "blue", alpha = 0.4) +
    ggtitle(paste0("Density of Loan Amount for the year ", year)) +  
    xlab("Value") +                     
    ylab("Density") +                   
    theme_minimal()
  
  ggsave(filename = paste0(FIGURE, "loan_amount_", year, ".pdf"), plot = graph)
  
}
)
df_descr_stats_county <- data |> 
  group_by(fips) |> 
  summarise(
    loan_amount_iqr = IQR(loan_amount, na.rm = TRUE)
  )

data_sex_lstatus <- data |> 
  mutate(loan_status = case_when(
    action_taken == 1 ~ "loan_originated",
    action_taken == 3 ~ "denied_inst",
    action_taken == 6 ~ "purchased_inst",
    .default = "denied_other_reason"
  ))

agg_data <- data_sex_lstatus |> 
  filter(applicant_race_1 %in% c(3,5)) |> 
  group_by(applicant_race_1, loan_status) |> 
  summarise(count = n(),.groups = "drop")

ggplot(agg_data, aes(x = applicant_race_1, y = count, fill = loan_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Number of Applications by Applicant Sex and Loan Status",
    x = "Applicant Sex",
    y = "Number of Applications",
    fill = "Loan Status"
  ) +
  theme_minimal()
  
data_rs <- data |> 
  filter(action_taken == 1 & !is.na(rate_spread)) |> 
  filter(inrange(rate_spread, -100, 100)) |> 
  ggplot(aes(rate_spread)) +
  geom_density()



############


col_classes <- c("numeric", "character", "numeric", "numeric", "numeric", "numeric", "numeric",
                 "numeric", "character", "character", "character", "character", "numeric", "numeric",
                 "numeric", "numeric", "numeric", "numeric", "character", "numeric", "numeric",
                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                 "numeric", "numeric", "numeric")

test05 <- fread(
  paste0(A, "p_hmda_lra/", lra_files[19]),
  colClasses = col_classes)



# Basic Cleaning
test05_f <- test05[action_taken == 1]
test05_f <- test05_f[loan_purpose %in% c(1,3)] # Home Purchase + Refinancing
test05_f <- test05_f[property_type %in% c(1)] # SF
test05_f <- test05_f[nchar(state_code) == 2 & nchar(county_code) == 3] # Keep only observation with fips code
test05_f <- test05_f[income != ""] # Delete all missings
test05_f <- test05_f[, income := as.numeric(income)] # Convert into numbers
test05_f <- test05_f[!is.na(income)] # Delete all left missings
test05_f <- test05_f[, lti_ratio := loan_amount / income ] # Loan-to-Income Ratio
test05_f <- test05_f[lti_ratio > 30] # Exclude loans with applicants, who have a Loan-to-Income Ratio of above 30 / this is seen even with down-payments unrealistic (ChatGPT)
# test05_f <- test05_f[, d_lti_ratio := ifelse(lti_ratio > 30, 1, 0)] 

# Cleaning if flagged observation
test05_v <- test05_f[is.na(edit_status)]

test05_nv <- test05_f[!is.na(edit_status)]

test05_hoepa <- test05_f[hoepa_status == 1]

ggplot(data = test05_hoepa) +
  geom_density(aes(x = loan_amount, color = "Loan Amount"), alpha = 0.4) +
  geom_density(aes(x = as.numeric(income), color = "Income"), alpha = 0.4) 

ggplot(data = test05_hoepa) +
  geom_density(aes(lti_ratio))

test05_valid <- test05_f |> 
  mutate(income = as.integer(income)) |> 
  filter(is.na(edit_status))

table(is.na(test05_valid$income))

test05_f <- test05_f[!is.na(edit_status)]

test06_head <- test06 |> head()

test1 <- test1 |> 
  mutate(
   loan_99999 = if_else(loan_amount_000s == 99999, 1, 0),
   loan_below_99999 = if_else(loan_amount_000s < 99999 & loan_amount_000s > 50000, 1, 0) 
  ) |> 
  filter(
   action_taken 
  )
test1_head <- test1 |> head()

View(test_head)
View(test1_head)


t
test <- read_delim(paste0(A, "p_hmda_lra/", "HMS.F2000.LARS"), n_max = 10)

summary(lm(property_type ~ loan_type + , data = test1))

cor(test1$property_type, test1$loan_type)

test1 |> 
  filter(loan_amount_000s < 1000) |> 
  ggplot(aes(loan_amount_000s)) +
  geom_density()

# test

test2 <- read_delim(file = paste0(A, "q_hmda_panel/HMS.F2000.PANEL"))
test2 <- read.table(file = paste0(A, "q_hmda_panel/HMS.F2000.PANEL"))



test3 <- LOAD(dfinput = "hmda_panel_2014")



## Panel

panel04 <- fread(paste0(A, "q_hmda_panel/", "HMDA_PANEL_2004.txt"), colClasses = "character")
panel05 <- fread(paste0(A, "q_hmda_panel/", "HMDA_PANEL_2005.txt"), colClasses = "character")

# Format Panel 
panel05_t <- panel05 |> 
  # mutate(across(-c("respondent_id", "parent_id", "respondent_rssd", "respondent_name", "respondent_city", "respondent_state"), as.integer)) |> 
  distinct(respondent_id, msamd, agency_code)

# Only RSSD Institutions (RSSD is assigned by the Federal Reserve)
panel05_rssd <- panel05 |> 
  filter(!is.na(respondent_id)) |> # this line makes a different of 7 unique observations -> there are 7 unique observations with RSSD but not RID
  distinct(respondent_rssd, other_lender_code) |> 
  filter(respondent_rssd != 0)


panel05_rid <- panel05 |> 
  distinct(respondent_id, respondent_rssd, other_lender_code) |> 
  filter(!is.na(respondent_id)) |> 
  filter(respondent_rssd == 0)

table(panel05_rid$other_lender_code)

panel05_rid_1 <- panel05 |> 
  distinct(respondent_id, respondent_rssd, other_lender_code) |> 
  filter(!is.na(respondent_id)) |> 
  filter(respondent_rssd != 0)

table(panel05_rid_1$other_lender_code)

table(is.na(panel05$other_lender_code))

panel05_NA <- panel05 |> 
  filter(is.na(respondent_id))

# At least one of the Identifiers exists
panel05_NA_anyid <- panel05 |> 
  filter(is.na(respondent_id) & is.na(parent_id) & is.na(respondent_rssd))
names <- unique(panel05_NA_anyid$respondent_name)

# Check if any observation
panel05_checknames <- panel05 |> 
  filter(!(is.na(respondent_id) & is.na(parent_id) & is.na(respondent_rssd))) |> 
  filter(respondent_name %in% names)

# 
names_parent <- unique(panel05$parent_id)
names_parent <- names_parent[!is.na(names_parent)]

check <- panel05 |> 
  filter(respondent_id %in% names_parent | parent_id %in% names_parent)


test <- panel05 |> 
  filter(str_detect(respondent_id, "-")) |> 
  mutate(respondent_id = as.integer(respondent_id))

table(is.na(test$respondent_id))


## New datasets
test18 <- fread(paste0(A, "p_hmda_lra/", lra_files[1]), colClasses = "character", nrows = 5)
panel18 <- fread(paste0(A, "q_hmda_panel/", panel_files[1]), colClasses = "character", nrows = 5)

test18_f <- test18[action_taken == 1]
# test18_f <- test18_f[!is.na(edit_status)] # Not Necessary
test18_f <- test18_f[loan_purpose %in% c(1,3)]
test18_f <- test18_f[property_type %in% c(1,3)]

############################### END ###########################################+