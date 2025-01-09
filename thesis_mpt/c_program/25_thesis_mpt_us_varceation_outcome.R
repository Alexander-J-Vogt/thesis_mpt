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

# Values for loan purpose 
loan_purpose_id <- c(1,3)

hmda_list <- purrr::map(seq_along(hmda_clean), function(x) {
  
  #*******************************************************************
  # Basic Setup to run iteration
  
  # Determine file to collapse
  file <- hmda_clean[x]
 
  # Determine year of the file
  year <- as.integer(str_replace_all(file, "[^0-9]", ""))
 
  # Load Data
  data_lar <- LOAD(dfinput = paste0("hmda_clean_", year))
  setDT(data_lar)
      
  # Update Message
  message(VISUALSEP)
  message(paste0("START TO COLLAPSE DATA FOR THE YEAR ", year, ".\n"))

  #*******************************************************************
  
  # Dynamically create dataset for refinancing and home purchase
  dataset <- purrr::map(loan_purpose_id, function(i){
    
    # Determine loan purpose by name
    name <- if(i == 1) "home_purchase" else "refinancing"
    
    # Update message
    message(paste0("Collapsing Data for loan purpose !", name, "!."))

    # Filter for loan purpose
    data <- data_lar[loan_purpose == i]
    
    # Decision on only including depository institution or also MBS of commercial banks is not MADE year!
    
    # Create Weights by loan application
    data <- data[, `:=` (
      weight_loan_amount = loan_amount / sum(loan_amount, na.rm = TRUE),
      weight_income = income / sum(income, na.rm = TRUE)
    )] 
    
    # Create Dataset County-level
    data <- data[, .(
      year = unique(year),
      loan_amount = sum(loan_amount, na.rm = TRUE), # loan amount by county
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
      rate_spread = fmean(rate_spread, na.rm = TRUE, w = weight_loan_amount), # rate spread, weighted by loan amount
      nr_originated_loan = .N # Number of originated loans
    ), by = fips]
    
    # Calculate log variables after collapse
    data[, `:=` (
      log_loan_amount = log(loan_amount),
      log_income_median = log(income_median),
      log_income_mean = log(income_mean),
      log_income_weighted_mean = log(income_weighted_mean)
      )]
    # Update Message
    message("End of collapse.\n")
    
    # Dataset to return
    return(data)
    
    }) # End of purrr::map #2
    
  # Combine datasets for all loan purposes for this year
  names(dataset) <- c("home_purchase", "refinancing")
  return(dataset)
  
}) # End of purrr:map #1

# Create dataset for home purchases
hmda_home_purchase <- map(hmda_list, "home_purchase")
dt_hmda_home_purchase <- bind_rows(hmda_home_purchase)

# Create dataset for refinancing
hmda_refinancing <- map(hmda_list, "refinancing")
dt_hmda_refinancing <- bind_rows(hmda_refinancing)

# Save datasets
SAVE(dfx = dt_hmda_home_purchase, namex = paste0(MAINNAME, "_HP"))
SAVE(dfx = dt_hmda_refinancing, namex = paste0(MAINNAME, "_REF"))



########################## ENDE ###############################################+
