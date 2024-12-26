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

if (DEBUG) {
  data04 <- fread(paste0(A, "p_hmda_lra/", lra_files[18]), colClasses = "character", nrows = 10)
  data10 <- fread(paste0(A, "p_hmda_lra/", lra_files[10]), colClasses = "character", nrows = 10)
  data20 <- fread(paste0(A, "p_hmda_lra/", lra_files[3]), colClasses = "character", nrows = 10)
}
## 1.2 Import LRA files -------------------------------------------------------

# This loop imports all LRA files
lapply(lra_files, function(file) {
  
  # Loop year in string
  loopy <- gsub("[^0-9]", "", file)
  
  # Column name depend on the years of the submission of the LRA as the program
  # has undergone several changes over time.
  if (as.integer(loopy) %in% c(2004:2006)) {
    lra_columns <- c("activity_year", "respondent_id", "agency_code", "loan_amount",
                     "state_code", "county_code", "action_taken", "loan_purpose", 
                     "property_type", "income", "edit_status", "hoepa_status", 
                     "rate_spread", "applicant_sex", "applicant_race_1")
  } else if (as.integer(loopy) %in% c(2007:2017)) {
    lra_columns <- c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", 
                     "state_code", "county_code", "action_taken", "loan_purpose", 
                     "property_type", "applicant_income_000s", "edit_status",  "hoepa_status",
                     "rate_spread", "applicant_sex", "applicant_race_1")
  } else if (as.integer(loopy) %in% c(2018:2023)) {
    lra_columns <- c("activity_year", "lei", "loan_amount", 
                     "state_code", "county_code", "action_taken", "loan_purpose", 
                     "derived_dwelling_category", "income", "hoepa_status",
                     "rate_spread", "applicant_sex", "applicant_race_1")
  }
  
  # Load all the raw LRA data on respondent-ID level (contains the information 
  # on each handed out loan). In order to reduce processing time, only the 
  # relevant variables in lra_columns are imported.
  data <- fread(
    paste0(A, "p_hmda_lra/", file),
    colClasses = "character",
    select = lra_columns
    )
  
  # Standardize the column names
  if (as.integer(loopy) %in% c(2007:2017)) {
    setnames(data,
             old = c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", 
                     "state_code", "county_code", "action_taken", "loan_purpose", 
                     "property_type", "applicant_income_000s", "edit_status",  "hoepa_status",
                     "rate_spread", "applicant_sex", "applicant_race_1"),
             new = c("activity_year", "respondent_id", "agency_code", "loan_amount", 
                     "state_code", "county_code", "action_taken",  "loan_purpose", 
                     "property_type", "income", "edit_status",  "hoepa_status",
                     "rate_spread", "applicant_sex", "applicant_race_1"))
  } else if (as.integer(loopy) %in% c(2018:2023)) {
    setnames(data,
             old = c("activity_year", "lei", "loan_amount", "state_code", "county_code", 
                     "action_taken", "loan_purpose", "derived_dwelling_category", "income",
                     "hoepa_status", "rate_spread", "applicant_sex", "applicant_race_1"),
             new = c("activity_year", "lei", "loan_amount", "state_code", "county_code",
                     "action_taken", "loan_purpose", "property_type", "income",
                     "hoepa_status", "rate_spread", "applicant_sex", "applicant_race_1"))
  }
  
  # # Recode property 
  # if (as.integer(loopy) %in% c(2018:2023)) {
  #   data <- data[, property_type := fifelse(property_type == "Single Family (1-4 Units):Site-Built", "1", property_type)]
  #   data <- data[, property_type := fifelse(property_type %in% c("Single Family (1-4 Units):Manufactured", "Multifamily:Manufactured"), "2", property_type)]
  #   data <- data[, property_type := fifelse(property_type == "Multifamily:Site-Built", "3", property_type)]
  # }
  
  # Save the raw lra dataset
  SAVE(dfx = data, namex = paste0("hmda_lra_", loopy), pattdir = TEMP)
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

## 1.4 Merging the Panel and LRA dataset with each other -----------------------

#' In the next step, the final HMDA dataset for each year is produced. 
#' These datasets are all on respondent-ID level, where each respondent can have
#' multiple observations as they have to report every single originated loan.

# Merge both Panel and LRA based on year
purrr::walk(2018:2023, function(i) {
  
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
  } else {
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


# 2. Data Basics ===============================================================  

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