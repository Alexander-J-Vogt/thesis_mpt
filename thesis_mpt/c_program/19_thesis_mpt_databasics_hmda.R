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

## 1.2 Import LRA files -------------------------------------------------------

# This loop import all LRA files
lapply(lra_files, function(file) {
  
  # Column name depend on the years of the submission of the LRA as the program
  # has undergone several changes over time.
  if (as.integer(gsub("[^0-9]", "", file) %in% c(2004:2006))) {
    lra_columns <- c("activity_year", "respondent_id", "agency_code", "loan_amount", "state_code", "county_code", )
  } else if (as.integer(gsub("[^0-9]", "", file) %in% c(2007:2017))) {
    lra_columns <- c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", "state_code", "county_code")
  } 
  
  # Load all the raw LRA data on respondent-ID level (contains the information 
  # on each handed out loan). In order to reduce processing time, only the 
  # relevant variables in lra_columns are imported.
  data <- fread(paste0(A, "p_hmda_lra/", file), colClasses = "character", select = lra_columns)
  
  # Standardize the column names
  if (as.integer(gsub("[^0-9]", "", file) %in% c(2007:2017))){
    setnames(data,
             old = c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", "state_code", "county_code"),
             new = c("activity_year", "respondent_id", "agency_code", "loan_amount", "state_code", "county_code"))
  }
  
  # Save the raw lra dataset
  SAVE(dfx = data, namex = paste0("hmda_lra_", gsub("[^0-9]", "", file)), pattdir = TEMP)
  print(paste0("LRA: Successful import of the year ", gsub("[^0-9]", "", file)))
  
  # Free unused memory and clear object from the  global environment.
  gc()
  rm(data)
})

## 1.3 Import Panel files ------------------------------------------------------

# Import Panel data and retrieve all unique observation in order to
# get the all unique respondent_id and agency_code combinations.
# This is later needed to identify Commercial Banks that hand out Mortgages with 
# the help of the variable "other_lender_code".

# This loop imports panel data 
purrr::walk(panel_files, function(file) {
  
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
             old = c("Respondent Identification Number", "Agency Code", "Other Lender Code"), 
             new = c("respondent_id", "agency_code", "other_lender_code"))
  } else if (year %in% c(2010:2017)) {
    setnames(data, 
             old = c("Respondent ID", "Agency Code", "Other Lender Code"), 
             new = c("respondent_id", "agency_code", "other_lender_code"))
  }
  
  # Select the relevant variables
  data <- data[, c("respondent_id", "agency_code", "other_lender_code")]
  
  # get rid off any duplicants
  data <- unique(data, by = c("respondent_id", "agency_code"))
  
  # Save the panel dataset
  SAVE(dfx = data, namex = paste0("hmda_panel_", year), pattdir = TEMP)
  
  # Update on iteration process
  print(paste0("Panel: Successful import of the year ", year))
  
  # Remove objects from the global environment and clean memory
  rm(data)
  gc()
})

## 1.4 Merging the Panel and LRA dataset with each other -----------------------

#' In the next step, the final HMDA dataset for each year is produced. 
#' These datasets are all on respondent-ID level, where each respondent can have
#' multiple observations as they have to report every single originated loan.

# Merge both Panel and LRA based on year
purrr::walk(2000:2017, function(i) {
  
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
  # and want to be able to identify the lender code of each respondents.  
  main <- left_join(dflra, dfpanel, by = c("respondent_id", "agency_code"))
  
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

test <- read_delim(paste0(A, "p_hmda_lra/", lra_files[13]))
test_head <- test |> head()

test1 <- fread(paste0(A, "p_hmda_lra/", lra_files[1]))
test05 <- fread(paste0(A, "p_hmda_lra/", lra_files[23]))
test04 <- read_delim(paste0(A, "p_hmda_lra/", lra_files[22]), n_max = 10)
test03 <- read_delim(paste0(A, "p_hmda_lra/", lra_files[21]), n_max = 10)


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
############################### END ###########################################+