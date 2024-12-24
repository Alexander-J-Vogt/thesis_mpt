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

## 1. Descriptive Statistics ===================================================

# list lra files
hmda_lra <- list.files(path = TEMP, pattern = "hmda_lra_")
hmda_lra <- gsub(".rda", "", hmda_lra)
years <- gsub("[^0-9]", "", hmda_lra)
hmda_lra <- hmda_lra[as.integer(years) >= 2004]

# initiate list
plot_lti_list <- list()
summary_stats_list <- list()
county_stats <- list()
nas_list <- list()

library(patchwork)
for (i in seq_along(hmda_lra)) {
  # i <- 18
  # loop over all hmda files
  year <- gsub("[^0-9]", "", hmda_lra[i])
  print(paste0("Start: ", year))
  data <- LOAD(dfinput = hmda_lra[i])
  setDT(data)
  nrows_pre <- nrow(data)
  
  # Basic Filtering 
  data <- data[action_taken == 1]
  data <- data[loan_purpose %in% c(1,3)]
  
  # Recode property 
  if (as.integer(year) %in% c(2018:2023)) {
    data <- data[, property_type := fifelse(property_type == "Single Family (1-4 Units):Site-Built", "1", property_type)]
    data <- data[, property_type := fifelse(property_type %in% c("Single Family (1-4 Units):Manufactured", "Multifamily:Manufactured"), "2", property_type)]
    data <- data[, property_type := fifelse(property_type == "Multifamily:Site-Built", "3", property_type)]
  }
  data <- data[property_type %in% c(1)]
  
  if (as.integer(year) %in% c(2004:2017)) {
    data <- data[nchar(state_code) == 2 & nchar(county_code) == 3]
  } else if (as.integer(year) %in% c(2018:2023)) {
    data <- data[nchar(county_code) == 5]
  }
  data <- data[income != ""]
  
  # Format Vars to numeric
  exclude_cols <- c("respondent_id", "county_code", "state_code")
  data[, (setdiff(names(data), exclude_cols)) := lapply(.SD, as.numeric), .SDcols = setdiff(names(data), exclude_cols)]
  
  # Clean last missings in income
  data <- data[!is.na(income)]
  
  # Calculate LTI, Loan Origination number by county
  data <- data[, lti_ratio := loan_amount / income]
  nrow_post <- nrow(data)
  data <- data[, ones := 1]
  
  # Determine FIPS
  if (as.integer(year) %in% c(2004:2017)) {
    data <- FIPSCREATOR(data = data, state_col = "state_code", county_col = "county_code")
  } else {
    setnames(data, old = "county_code", new = "fips")
  }
  data <- data[, cnty_loan_org := sum(ones), by = fips ]
  
  # Key descriptive statistics
  home_purchase_perc <- round(sum(data$ones[data$loan_purpose == 1]) / nrow_post * 100, digits = 2)
  hoepa_status_perc  <- round(sum(data$ones[data$hoepa_status == 1]) / nrow_post * 100, digits = 2)
  lti_above30 <- round(sum(data$ones[data$lti > 30]) / nrow_post * 100, digits = 2)
  hp_org_volume <- sum(data$loan_amount[data$loan_purpose == 1]) / 1000000
  rf_org_volume <- sum(data$loan_amount[data$loan_purpose == 3]) / 1000000
  nr_loan_amount_zero <- sum(data$ones[data$loan_amount == 0])
  nr_income_zero <- sum(data$ones[data$income == 0])
  
  stats <- data |> 
    summarise(
      year = as.integer(year),
      loan_amount_mean = mean(loan_amount, na.rm = TRUE),
      loan_amount_sd = sd(loan_amount, na.rm = TRUE),
      loan_amount_q25 = quantile(loan_amount, probs = .25, na.rm = TRUE),
      loan_amount_median = median(loan_amount, na.rm = TRUE),
      loan_amount_q75 =  quantile(loan_amount, probs = .75, na.rm = TRUE),
      income_mean = mean(income, na.rm = TRUE),
      income_sd = sd(income, na.rm = TRUE),
      income_q25 = quantile(income, probs = .25, na.rm = TRUE),
      income_median = median(income, na.rm = TRUE),
      income_q75 =  quantile(income, probs = .75, na.rm = TRUE),
      hp_perc = home_purchase_perc,
      hoep_1_perc = hoepa_status_perc,
      lti_above30 = lti_above30,
      hp_org_volume = hp_org_volume,
      rf_org_volume = rf_org_volume,
      nr_zero_loan = nr_loan_amount_zero,
      nr_zero_income = nr_income_zero,
      nrow_org = nrows_pre,
      nrow_clean = nrow_post
    )
  
  summary_stats_list[[i]] <- stats
  names(summary_stats_list)[[i]] <- paste0("hmda_", year)
  print(paste0("Stats ", year, " finished."))
  
  # Key Plots
  
  plot1 <- ggplot(data = data[lti_ratio < 30]) +
    geom_density(aes(loan_amount))
  
  plot2 <- ggplot(data = data[lti_ratio < 30]) + 
    geom_density(aes(income))
  
  plot3 <- ggplot(data = data[lti_ratio < 30]) + 
    geom_density(aes(lti_ratio))
  
  plot4 <- ggplot(data = data[lti_ratio > 30]) +
    geom_density(aes(loan_amount))
  
  plot5 <- ggplot(data = data[lti_ratio > 30]) + 
    geom_density(aes(income))
  
  plot6 <- ggplot(data = data[lti_ratio > 30]) + 
    geom_density(aes(lti_ratio))
  
  graph <- (plot1 | plot2 | plot3) /
    (plot4 | plot5 | plot6)
  
  plot_lti_list[[i]] <- graph
  names(plot_lti_list)[[i]] <- paste0("hmda_", year)
  print(paste0("Graphs ", year, " finished."))
  
  # County Information
  cnty_origination <- data[, .SD[1], by = .(fips, cnty_loan_org)]
  vars_to_keep <- c("activity_year", "fips", "cnty_loan_org")
  cnty_origination <- cnty_origination[, ..vars_to_keep]
  
  county_stats[[i]] <- cnty_origination
  names(stats)[[i]] <- paste0("hmda_", year)
  print(paste0("County-level data for ", year, " finished."))
  
  # Count NAs per column and save in wide format
  na_counts <- data.table(t(sapply(data, function(x) sum(is.na(x)))))
  na_counts[1,1] <- as.integer(year)
  nas_list[[i]] <- na_counts
  names(nas_list)[[i]] <- paste0("hmda_", year)
  
}

nas <- bind_rows(nas_list)




############################### END ###########################################+