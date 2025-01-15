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

# 1. Data Collapsing - Data from 2004 on ======================================= 

DEBUG <- T

# Determine whether to debug the code or not
hmda_clean <- list.files(path = TEMP, pattern = "hmda_clean")
hmda_clean <- hmda_clean[!str_detect(hmda_clean, pattern = "sample")]
hmda_clean <- gsub(hmda_clean, pattern = ".rda", replacement = "")

if (DEBUG) {
  hmda_clean <- list.files(path = TEMP, pattern = "hmda_clean")
  hmda_clean <- hmda_clean[str_detect(hmda_clean, pattern = "sample")]
  hmda_clean <- hmda_clean[!str_detect(hmda_clean, pattern = "reform")]
  hmda_clean <- gsub(hmda_clean, pattern = ".rda", replacement = "")
  x <- 1
}

# Values for loan purpose 
loan_purpose_id <- c(1,3)

# 1.1  Iteration over all years + Creating Main Dataset on County-level --------
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
    loan_purpose_name <- if(i == 1) "home_purchase" else "refinancing"
    
    # Initialize list to hold datasets for different lender codes
    datasets_by_lender <- list()
    
    # Loop over lender code filters
    lender_filters <- list(
      "depository_only" = c(1),
      "depository_and_mbs" = c(1, 2)
    )
    
    # Dynamiccal create extra 
    for (lender in names(lender_filters)) {
      
      # Update message
      message(paste0("Collapsing Data for loan purpose ", loan_purpose_name , " and ", lender, " ."))
      
      # Define set of lender for this loop
      lender_set <- lender_filters[[lender]]
      
      # Filter for loan purpose
      data <- data_lar[loan_purpose == i]
      
      # Filter for Financial Institution
      data <- data[other_lender_code %in% lender_set]

      
      # Create Weights by loan application
      data <- data[, `:=` (
        weight_loan_amount = loan_amount / sum(loan_amount, na.rm = TRUE),
        weight_income = income / sum(income, na.rm = TRUE)
      )] 
      
      data <- data[, weight_org_loans = .N, by = fips]
      
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
        income_mean_wloan = fmean(income, na.rm = TRUE, w = weight_loan_amount), # mean, weight = loan amount
        income_mean_wnr = fmean(income, na.rm = TRUE, w = weight_org_loans), # mean, weight = Number of originated Loans
        income_median = fquantile(income, probs = .5), # median income
        lti_ratio_mean = fmean(lti_ratio, na.rm = TRUE), # mean loan-to-income ratio
        lti_ratio_median = fmedian(lti_ratio, na.rm = TRUE), # median loan-to-income ratio
        rate_spread_wloan = fmean(rate_spread, na.rm = TRUE, w = weight_loan_amount), # rate spread, weighted by loan amount
        rate_spread_winc = fmean(rate_spread, na.rm = TRUE, w = weight_income), # rate spread, weighted by income
        rate_spread_wnr = fmean(rate_spread, na.rm = TRUE, w = weight_org_loans), # rate spread, weighted by number of originated loans
        nr_originated_loan = .N # Number of originated loans
      ), by = fips]
      
      # Calculate log variables after collapse
      data[, `:=` (
        log_loan_amount = log(loan_amount),
        log_income_median = log(income_median),
        log_income_mean = log(income_mean),
        log_income_weighted_mean = log(income_mean_wloan)
        )]
      
      # Update Message
      message("End of collapse.\n")
      
      # Dataset to return
      datasets_by_lender[[lender]] <- data
      
    } # End of for-loop #3
    
    # Combine datasets for all lender groups for this loan prupose
    return(datasets_by_lender)
    
  }) # End of purrr::map #2
    
  # Combine datasets for all loan purposes for this year
  names(dataset) <- c("home_purchase", "refinancing")
  return(dataset)
  
}) # End of purrr:map #1

# 1.2 Save Datasets by Year, Loan Purpose and Lender Group ---------------------

# Combine and save datasets for all years by loan purpose and lender group
save_combined_datasets <- function(hmda_list, purpose, lender_group, file_name) {
  combined_data <- rbindlist(
    lapply(hmda_list, function(year_data) year_data[[purpose]][[lender_group]]),
    fill = TRUE
  )
  SAVE(dfx = combined_data, namex = file_name)
  return(combined_data)
}

# Save datasets
hmda_hp_dep <- save_combined_datasets(hmda_list, "home_purchase", "depository_only", paste0(MAINNAME, "_hp_depsitory_only"))
hmda_hp_deb_mbs <- save_combined_datasets(hmda_list, "home_purchase", "depository_and_mbs", paste0(MAINNAME, "_hp_depository_and_mbs"))
hmda_ref_dep <- save_combined_datasets(hmda_list, "refinancing", "depository_only", paste0(MAINNAME, "_ref_depository"))
hmda_ref_dep_mbs <- save_combined_datasets(hmda_list, "refinancing", "depository_and_mbs", paste0(MAINNAME, "_ref_depository_and_mbs"))



# Create dataset for home purchases
hmda_home_purchase <- map(hmda_list, "home_purchase")
dt_hmda_home_purchase <- bind_rows(hmda_home_purchase)

# Create dataset for refinancing
hmda_refinancing <- map(hmda_list, "refinancing")
dt_hmda_refinancing <- bind_rows(hmda_refinancing)

# Save datasets
SAVE(dfx = dt_hmda_home_purchase, namex = paste0(MAINNAME, "_HP"))
SAVE(dfx = dt_hmda_refinancing, namex = paste0(MAINNAME, "_REF"))


# 2. Data Collapsing - Data from 2018 on =======================================

hmda_reform <- list.files(path = TEMP, pattern = "reform_clean")
hmda_reform <- str_replace_all(hmda_reform, ".rda", "")

if (DEBUG) {
  hmda_reform <- list.files(path = TEMP, pattern = "clean_reform")
  hmda_reform <- str_replace_all(hmda_reform, ".rda", "") 
  x <- 2
  hmda_reform <- hmda_reform[1:3]
}

# Values for loan purpose 
loan_purpose_id <- c(1,3)

# 1.1  Iteration over all years + Creating Main Dataset on County-level --------
hmda_reform_list <- purrr::map(seq_along(hmda_reform), function(x) {
  
  #*******************************************************************
  # Basic Setup to run iteration
  
  # Determine file to collapse
  file <- hmda_reform[x]
  
  # Determine year of the file
  year <- as.integer(str_replace_all(file, "[^0-9]", ""))
  
  # Load Data
  data_lar <- LOAD(dfinput = file)
  setDT(data_lar)
  
  if (DEBUG) {
    setnames(data_lar, old = c("county_code"), new = c("fips"))
    if (year > 2018) setnames(data_lar, old = c("combined_loan_to_value_ratio"), new = c("loan_to_value_ratio"))
  }
  # Update Message
  message(VISUALSEP)
  message(paste0("START TO COLLAPSE DATA FOR THE YEAR ", year, ".\n"))
  
  #*******************************************************************
  
  # Dynamically create dataset for refinancing and home purchase
  dataset <- purrr::map(loan_purpose_id, function(i){
    
    i <- 1
    # Determine loan purpose by name
    loan_purpose_name <- if(i == 1) "home_purchase" else "refinancing"
    
    # Initialize list to hold datasets for different lender codes
    datasets_by_lender <- list()
    
    # Loop over lender code filters
    lender_filters <- list(
      "depository_only" = c(1),
      "depository_and_mbs" = c(1, 2)
    )
    
    # Dynamiccal create extra 
    for (lender in names(lender_filters)) {
      
      # Update message
      message(paste0("Collapsing Data for loan purpose ", loan_purpose_name , " and ", lender, "."))
      
      # Define set of lender for this loop
      lender_set <- lender_filters[[lender]]
      
      # Filter for loan purpose
      data <- data_lar[loan_purpose == i]
      
      # Filter for Financial Institution
      data <- data[other_lender_code %in% lender_set]
      
      # Create Weights by loan application
      data <- data[, `:=` (
        weight_loan_amount = loan_amount / sum(loan_amount, na.rm = TRUE),
        weight_income = income / sum(income, na.rm = TRUE)
      )] 
      
      data <- data[, weight_org_loans := .N, by = fips]
      
      # Create Dataset County-level
      data <- data[, .(
        year = unique(year),
        loan_amount = sum(loan_amount, na.rm = TRUE), # loan amount by county
        log_loan_amount_dir = mean(log_loan_amount), # Caluclate log loan amount based on the available var log_loan_amount in LAR
        share_white_applicant = sum(applicant_race_1 == 5, na.rm = TRUE) / .N, # share of white applicants
        share_black_applicant = sum(applicant_race_1 == 3, na.rm = TRUE) / .N, # share of black applicants
        share_asian_applicant = sum(applicant_race_1 == 2, na.rm = TRUE) / .N, # share of asian applicant
        share_americanindian_applicant = sum(applicant_race_1 == 1, na.rm = TRUE) / .N, # share of asian applicants
        share_others_applicant = sum(!applicant_race_1 %in% c(1:3, 5)) / .N, # share of applicants with other race
        share_male_applicant = sum(applicant_sex == 1, na.rm = TRUE) / .N, # share of male applicants
        share_female_applicant = sum(applicant_sex == 2, na.rm = TRUE) / .N, # share of female applicants
        share_hoepa = sum(hoepa_status == 1, na.rm = TRUE) / .N, # Share of High Risk Loans
        income_mean = fmean(income, na.rm = TRUE), # mean income
        income_mean_wloan = fmean(income, na.rm = TRUE, w = weight_loan_amount), # mean, weight = loan amount
        income_mean_wnr = fmean(income, na.rm = TRUE, w = weight_org_loans), # mean, weight = loan amount
        income_median = fquantile(income, probs = .5), # median income
        lti_ratio_mean = fmean(loan_to_value_ratio, na.rm = TRUE), # mean loan-to-income ratio
        lti_ratio_median = fmedian(loan_to_value_ratio, na.rm = TRUE), # median loan-to-income ratio
        rate_spread_wloan = fmean(rate_spread, na.rm = TRUE, w = weight_loan_amount), # rate spread, weighted by loan amount
        rate_spread_winc = fmean(rate_spread, na.rm = TRUE, w = weight_income), # rate spread, weighted by loan amount
        rate_spread_wnr = fmean(rate_spread, na.rm = TRUE, w = weight_org_loans), # rate spread, weighted by loan amount
        interest_rate_wloan = fmean(interest_rate, na.rm = TRUE, w = weight_loan_amount), # interest_rate, weight = loan amount
        interest_rate_winc = fmean(interest_rate, na.rm = TRUE, w = weight_income), # interest_rate, weighted by loan amount
        interest_rate_wnr = fmean(interest_rate, na.rm = TRUE, w = weight_org_loans), # interest_rate, weighted by loan amount
        property_value_wloan = fmean(property_value, na.rm = TRUE, w = weight_loan_amount), # property_value, weight = loan amount
        property_value_winc = fmean(property_value, na.rm = TRUE, w = weight_income), # property_value, weighted by loan amount
        property_value_wnr = fmean(property_value, na.rm = TRUE, w = weight_org_loans), # property_value, weighted by loan amount
        nr_originated_loan = .N # Number of originated loans
      ), by = fips]
      
      # Calculate log variables after collapse
      data[, `:=` (
        log_loan_amount = log(loan_amount),
        log_income_median = log(income_median),
        log_income_mean = log(income_mean),
        log_income_weighted_mean = log(income_mean_wnr)
      )]
      # Update Message
      message("End of collapse.\n")
      
      # Dataset to return
      datasets_by_lender[[lender]] <- data
      
    } # End of for-loop #3
    
    # Combine datasets for all lender groups for this loan prupose
    return(datasets_by_lender)
    
  }) # End of purrr::map #2
  
  # Combine datasets for all loan purposes for this year
  names(dataset) <- c("home_purchase", "refinancing")
  return(dataset)
  
}) # End of purrr:map #1

# Save datasets
hmda_reform_hp_dep <- save_combined_datasets(hmda_reform_list, "home_purchase", "depository_only", paste0(MAINNAME, "_reform_hp_depsitory_only"))
hmda_reform_hp_deb_mbs <- save_combined_datasets(hmda_reform_list, "home_purchase", "depository_and_mbs", paste0(MAINNAME, "_reform_hp_depository_and_mbs"))
hmda_reform_ref_dep <- save_combined_datasets(hmda_reform_list, "refinancing", "depository_only", paste0(MAINNAME, "_reform_ref_depository"))
hmda_reform_ref_dep_mbs <- save_combined_datasets(hmda_reform_list, "refinancing", "depository_and_mbs", paste0(MAINNAME, "_reform_ref_depository_and_mbs"))


########################## ENDE ###############################################+

