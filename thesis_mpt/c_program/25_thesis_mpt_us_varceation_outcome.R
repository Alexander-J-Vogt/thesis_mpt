# TARGET: Creating two county-level datasets, where one only contains mortgages of commercial banks and one from all financial institutions
# INDATA: hmda_merge files for all years
# OUTDATA/ OUTPUT: hmda_banks, hmda_all

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

# 1. Data Cleaning and Collapsing Data to County-level ========================= 

DEBUG <- T

# Determine whether to debug the code or not
hmda_clean <- list.files(path = TEMP, pattern = "hmda_clean")
hmda_clean <- hmda_clean[!str_detect(hmda_clean, pattern = "sample")]
hmda_clean <- gsub(hmda_clean, pattern = ".rda", replacement = "")

if (DEBUG) {
  hmda_clean <- list.files(path = TEMP, pattern = "hmda_clean")
  hmda_clean <- hmda_clean[str_detect(hmda_clean, pattern = "sample")]
  hmda_clean <- gsub(hmda_clean, pattern = ".rda", replacement = "")
}

# 

hmda_home_purchase <- purrr::map(seq_along(hmda_clean), function(x){
  
  # x <- 5
  
  #*******************************************************************
  # Determine file to collapse
  file <- hmda_clean[x]
  
  # Determine year of the file
  year <- as.integer(str_replace_all(file, "[^0-9]", ""))
  
  # Load Data
  data_lar <- LOAD(dfinput = paste0("hmda_clean_", year))
  setDT(data_lar)
  
  # Update MEssage
  message(VISUALSEP)
  message(paste0("Start to collapse data from the year ", year, "."))
  
  
  #*******************************************************************
  
  # Filter for home purchase
  data_lar <- data_lar[loan_purpose == 1]
  
  # Create Weights by loan application
  data_lar <- data_lar[, `:=` (
    weight_loan_amount = loan_amount / sum(loan_amount, na.rm = TRUE),
    weight_income = income / sum(income, na.rm = TRUE)
  )] 
  
  # Create Dataset County-level
  data_fips <- data_lar[, .(
    year = unique(year),
    loan_amount_fips = sum(loan_amount, na.rm = TRUE), # loan amount by county
    share_white_applicant = sum(applicant_race_1 == 5, na.rm = TRUE) / .N, # share of white applicants
    share_black_applicant = sum(applicant_race_1 == 3, na.rm = TRUE) / .N, # share of black applicants
    share_asian_applicant = sum(applicant_race_1 == 2, na.rm = TRUE) / .N, # share of asian applicant
    share_americanindian_applicant = sum(applicant_race_1 == 1, na.rm = TRUE) / .N, # share of asian applicants
    share_others_applicant = sum(!applicant_race_1 %in% c(1:3, 5)) / .N, # share of applicants with other race
    share_male_applicant = sum(applicant_sex == 1, na.rm = TRUE) / .N, # share of male applicants
    share_female_applicant = sum(applicant_sex == 2, na.rm = TRUE) / .N, # share of female applicants
    income_mean = fmean(income, na.rm = TRUE), # mean income
    income_weighted_mean = fmean(income, na.rm = TRUE, w = weight_loan_amount), # mean, weight = loan amount
    income_median = fquantile(income, probs = .5), # median income
    lti_ratio_mean = fmean(lti_ratio, na.rm = TRUE), # mean loan-to-income ratio
    lti_ratio_median = fmedian(lti_ratio, na.rm = TRUE), # median loan-to-income ratio
    nr_originated_loan = .N # Number of originated loans
  ), by = fips]
  
  # Update Message
  message("End of collapse.")
  
  
})




#' This section performs a standardized way to do some basic cleaning of data
#' and collapses the data to county-level with the help of the COUNTYLEVEL function.
#'
#' What does the COUNTYLEVEL-function do?
#' i) It allows to filter either for Commercial Banks and their subdivisions,
#' which lend mortgages or to exclude the filter. I
#' -> instfilter: institution filter for commercial banks (TRUE: Only Commercial banks)
#' ii) It excludes all observation if either the state-code or county-code is missing.
#' iii) It creates the fips-code. The unique identifier for each county in the U.S.
#' iv) It makes sure to only include US states and District of Columbia.
#' v) It collapses the data to county-year-level.

# List all merged files
hmda_files <- list.files(paste0(TEMP, "/") , pattern = "hmda_merge")

# Filter for all Commercial Banks
# Core Problem less and less CB or their mortgage subdivisions are handing out 
# mortgage after 2009, which is trend observed in the mortgage lending business.
mortgages_banks <- lapply(hmda_files, COUNTYLEVEL, instfilter = TRUE)

# Create the actual datasets
hmda_banks <- bind_rows(mortgages_banks)

# 2. Creating Variable =========================================================
hmda_banks[, state := substr(fips, 1, 2)] 

# Create loan amount variables
hmda_banks[, ln_loan_amount := log(total_amount_loan)]
hmda_banks[, lead_ln_loan_amount := shift(ln_loan_amount, type = "lead"), by = fips]

# Deselect relevant variables
setcolorder(hmda_banks, c("year", "fips", "state"))


# 3. Save ======================================================================

SAVE(dfx = hmda_banks, namex = MAINNAME)

########################## ENDE ###############################################+
