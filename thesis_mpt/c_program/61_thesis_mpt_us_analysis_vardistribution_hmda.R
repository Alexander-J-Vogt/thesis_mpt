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

## 1. LAR - Descriptive Statistics for HMDA ====================================

DEBUG <- T

# Data was cleaned on and is on loan application level

# Determine whether to debug the code or not
hmda_clean <- list.files(path = TEMP, pattern = "hmda_clean")
hmda_clean <- hmda_clean[!str_detect(hmda_clean, pattern = "sample")]
hmda_clean <- gsub(hmda_clean, pattern = ".rda", replacement = "")

if (DEBUG) {
  hmda_clean <- list.files(path = TEMP, pattern = "hmda_clean")
  hmda_clean <- hmda_clean[str_detect(hmda_clean, pattern = "sample")]
  hmda_clean <- gsub(hmda_clean, pattern = ".rda", replacement = "")
}

# Initiate lists in order to save different statistics
desc_stats_tot_list <- list()
desc_stats_county_list <- list()
fips_detailed_list <- list()
fips_missings_list <- list()


## 1.1 Loop for Descriptive Statistics -----------------------------------------

purrr::walk(seq_along(hmda_clean), function(x) {
  
  x <- 5
  
  # ****************************************************************************
  # Determine file to analyse
  file <- hmda_clean[x]
  
  # Determine year
  year <- as.integer(str_replace_all(file, "[^0-9]", ""))
  
  # Update message
  message(paste0("\n",VISUALSEP))
  message(paste0("Start to analyse data for the year ", year, "."))
  
  # Load data
  data <- LOAD(dfinput = file)
  setDT(data)
  #
  # ****************************************************************************
  
  
  ### i) Evaluation of missing counties ---
  
  ## Load Decennial Census ---
  
  # Final number of counties
  fip_in_data <- unique(data$fips)
  
  # List of Counties by TIGRIS-Package - Decennial Census
  if (year <= 2010) {
    fips_raw <- counties(year = 2000, progress_bar = FALSE)
  } else if (year > 2010 & year <= 2020) {
    fips_raw <- counties(year = 2010, progress_bar = FALSE)
  } else if (year > 2020) {
    fips_raw <- counties(year = 2020, progress_bar = FALSE)
  }
  
  # Vars for county are named differently across census - Adjust for that
  county_var <- if (year <= 2010) "CNTYIDFP00" else if (year > 2010 & year <= 2020) "GEOID10" else if (year > 2020) "GEOID"
  county_name_var <- if (year <= 2010) "NAME00" else if (year > 2010 & year <= 2020) "NAME10" else if (year > 2020)"NAMELSAD"
  
  # Basic Transformations for a base dataset on available FIPS Codes
  fips <- fips_raw |>
    as_tibble() |> 
    select(STATEFP, !!sym(county_var), !!sym(county_name_var)) |> 
    filter(!STATEFP %in% c("66", "60", "69","72", "74", "78")) |> # 50 States + Washington D.C.
    arrange(STATEFP, !!sym(county_var))
  
  fips_existing_counties <- fips[[county_var]]
  
  ## Determine missing counties ---
  
  # Determine which of the counties are missing
  diff_missings_counties <- setdiff(fips_existing_counties, fip_in_data)
  
  # Create DF and add information on state and county name
  df_missings <- data.frame(county_code = diff_missings_counties)
  
  # Load data on fips code and names
  data(fips_codes)
  
  # Select relevant variables
  fips_code <- fips_codes |> 
    mutate(
      county_code = paste(state_code, county_code, sep = "")
    ) |> 
    filter(!state_code %in% c("66", "60", "69","72", "74", "78")) |> 
    select(county_code, state_name, county)
  
  # Join Missing Counties with Information on state and county name
  df_missings <- df_missings |> 
    left_join(fips_code, by = "county_code") |> 
    mutate(year = year)
  
  ## Save the DF into the list ---
  fips_missings_list[[x]] <<- df_missings
  names(fips_missings_list)[[x]] <<- paste0("hdma_", year)
  
  # Update Message
  message("Finished Determining Missing Counties.")
  
  ### ii) Calculation for Total Population ---
  
  # Collect Descriptive Stats in DF
  df_descr_stats_tot <- data.frame()
  df_descr_stats_county <- data.frame()
  
  ## Key Numbers for ...
  # General
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
      year = unique(year),
      share_male_applicant = tot_male / tot_origin_all * 100,
      share_female_applicant = tot_female / tot_origin_all * 100,
      share_white_applicant = tot_white / tot_origin_all * 100,
      share_black_applicant = tot_black / tot_origin_all * 100,
      share_asian_applicant = tot_asian / tot_origin_all * 100,
      share_american_indian = tot_americanindian / tot_origin_all * 100,
      share_others = tot_others / tot_origin_all * 100,
      rate_spread_NA = sum(is.na(data$rate_spread)) / tot_origin_all,
      rate_spread_min = min(rate_spread, na.rm= TRUE),
      rate_spread_q25 = quantile(rate_spread, probs = .25, na.rm = TRUE),
      rate_spread_median = median(rate_spread, na.rm = TRUE),
      rate_spread_mean = mean(rate_spread, na.rm = TRUE),
      rate_spread_q75 = quantile(rate_spread, probs = .75, na.rm = TRUE),
      rate_spread_max = max(rate_spread, na.rm= TRUE),
      rate_spread_iqr = IQR(rate_spread, na.rm = TRUE),
      income_NA = sum(is.na(income)) / tot_origin_all,
      income_min = min(income, na.rm= TRUE),
      income_q25 = quantile(income, probs = .25, na.rm = TRUE),
      income_median = median(income, na.rm = TRUE),
      income_mean = mean(income, na.rm = TRUE),
      income_q75 = quantile(income, probs = .75, na.rm = TRUE),
      income_max = max(income, na.rm = TRUE),
      income_iqr = IQR(income, na.rm = TRUE),
      income_above500 = sum(income > 500) / tot_origin_all,
      loan_amount_NA = sum(is.na(loan_amount)) / tot_origin_all,
      loan_amount_min = min(loan_amount, na.rm= TRUE),
      loan_amount_q25 = quantile(loan_amount, probs = .25, na.rm = TRUE),
      loan_amount_median = median(loan_amount, na.rm = TRUE),
      loan_amount_mean = mean(loan_amount, na.rm = TRUE),
      loan_amount_q75 = quantile(loan_amount, probs = .75, na.rm = TRUE),
      loan_amount_max = max(loan_amount, na.rm= TRUE),
      loan_amount_iqr = IQR(loan_amount, na.rm = TRUE),
      loan_amount_outside_iqr = sum(loan_amount > loan_amount_q75 + loan_amount_iqr * 1.5) / tot_origin_all * 100,
      hoepa_high_cost = sum(hoepa_status == 1),
      hoepa_share_high_cost = sum(hoepa_status == 1) / tot_origin_all,
      hoepa_non_high_cost = sum(hoepa_status == 2),
      hoepa_share_non_high_cost = sum(hoepa_status == 2) / tot_origin_all,
      loan_purpose_hp = sum(loan_type == 1) / tot_origin_all * 100,
      loan_purpose_hi = sum(loan_type == 2) / tot_origin_all * 100,
      loan_purpose_refin = sum(loan_type == 3) / tot_origin_all * 100,
      nr_obs = tot_origin_all
    )
  
  desc_stats_tot_list[[x]] <- df_descr_stats_tot
  names(desc_stats_tot_list)[[x]] <<- paste0("HDMA_", year)
  
  message("Finished calculating Descriptive Statistics for Total Population.")
  
  
  ### iii) Calculation by FIPS ---
  
  # Save Descriptive Statistics by FIPS in DF
  df_descr_stats_county <- data |> 
    group_by(fips) |> 
    reframe(
      year = unique(year),
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
      income_negativ = sum(income < 0),
      income_excesive = sum(income > 9999),
      loan_amount_NA = sum(is.na(loan_amount)),
      loan_amount_min = min(loan_amount, na.rm= TRUE),
      loan_amount_q25 = quantile(loan_amount, probs = .25, na.rm = TRUE),
      loan_amount_median = median(loan_amount, na.rm = TRUE),
      loan_amount_mean = mean(loan_amount, na.rm = TRUE),
      loan_amount_q75 = quantile(loan_amount, probs = .75, na.rm = TRUE),
      loan_amount_max = max(loan_amount, na.rm= TRUE),
      income_negativ = sum(income < 0),
      hoepa_high_cost = sum(hoepa_status == 1),
      hoepa_share_high_cost = sum(hoepa_status == 1) / n() * 100,
      hoepa_non_high_cost = sum(hoepa_status == 2),
      hoepa_share_non_high_cost = sum(hoepa_status == 2) / n() * 100,
      loan_purpose_hp = sum(loan_type == 1) / n() * 100,
      loan_purpose_hi = sum(loan_type == 2) / n() * 100,
      loan_purpose_refin = sum(loan_type == 3) / n() * 100
    )
  
  desc_stats_county_list[[x]] <<- df_descr_stats_county
  names(desc_stats_county_list)[[x]] <<- paste0("HDMA_", year)
  
  
  message("Finished calculating Descriptive Statistics for by County")
          
  ### iv) Boxplot for loan amount and income by year ---
  
  # Analyse the loan_amount distribution
  graph <- ggplot(data = data, aes(loan_amount)) +
    geom_density(fill = "blue", alpha = 0.4, size = 0.7, na.rm = TRUE) +
    ggtitle(paste0("Density of Loan Amount for the year ", year)) +  
    xlab("Value") +                     
    ylab("Density") +                   
    theme_minimal() +
    scale_y_continuous(
      limits = c(0,0.004),
      breaks = seq(0, 0.004, by = 0.001)
    ) +
    scale_x_continuous(
      limits = c(0, 10000),  # Fixed x-axis range
      breaks = seq(0, 10000, by = 2000)  # Optional: Set axis breaks
    ) 
  ggsave(filename = paste0(FIGURE, "loan_amount_", year, ".pdf"), plot = graph)
  
  # Analyse the income distribution
  graph <- ggplot(data = data, aes(income)) +
    geom_density(fill = "blue", alpha = 0.4, size = 0.7, na.rm = TRUE) +
    ggtitle(paste0("Density of Income for the year ", year)) +
    xlab("Value") +
    ylab("Density") +
    theme_minimal() +
    scale_y_continuous(
      limits = c(0,0.02),
      breaks = seq(0, 0.02, by = 0.005)
    ) +
    scale_x_continuous(
      limits = c(0, 500),  # Fixed x-axis range
      breaks = seq(0, 500, by = 50)  # Optional: Set axis breaks
    )
  
  ggsave(filename = paste0(FIGURE, "income_dist_", year, ".pdf"), plot = graph)
  
  # Boxplot loan amount and log loan amount
  graph <- ggplot(data = data, aes(x = loan_amount, y = "loan_amount")) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 18, outlier.size = 2) +
    stat_summary(
      fun = mean, geom = "point", shape = 18, size = 3, color = "blue"
    ) +
    labs(
      title = paste0("Loan Amount Boxplot for the year ", year),
      x = "Loan Amount in 000s",
      y = ""
    ) +
     scale_x_continuous(
       limits = c(0,15000),
       breaks = seq(0, 15000, by = 2500)
     ) +
     theme_minimal()
  
  ggsave(filename = paste0(FIGURE, "boxplot_loan_amount_", year, ".pdf"), plot = graph)
   
  # Update Message
  message("Finished creating Boxplot.")
  message("End of Analysis")
  
  year_max <- max(as.integer(str_replace_all(hmda_clean, "[^0-9]", "")))
  
  if (year == year_max) {
    message(paste0("\n", VISUALSEP))
    message(paste0("LOOP IS FINISHED AFTER ", year_max, "."))
    message(VISUALSEP)
  }
  
  # Boxplot
  # Clean environment from garbage
  rm(data)
  gc()
  
  } # End of function
) # End of purrr:walk

## 1.2 Evaluate Descriptive Statistics -----------------------------------------

### 1.2.1 Create DT and Save ---------------------------------------------------

# Descriptive Statistics on Total Population
desc_stats_tot <- bind_rows(desc_stats_tot_list)
setDT(desc_stats_tot)
SAVE(dfx = desc_stats_tot, namex = "descriptive_statistics_total_population")

# Descriptive Statistics by County
desc_stats_county <- bind_rows(desc_stats_county_list)
setDT(desc_stats_county)
SAVE(dfx = desc_stats_tot, namex = "descriptive_statistics_by_county")

# FIPS Missing
fips_missing <- bind_rows(fips_missings_list)
setDT(fips_missing)
SAVE(dfx = desc_stats_tot, namex = "fips_missing")

# Fips Misssing after each filter step
fips_detailed <- bind_rows(fips_detailed_list)
setDT(fips_detailed)
SAVE(dfx = fips_detailed, namex = "fips_detailed_missing")

# 1.2.3 Create Boxplot for Loan Amount and Income ------------------------------

## Boxplot for loan amount
boxplot_loan_amount <- ggplot(desc_stats_tot, aes(x = factor(year))) +  # Use year as a factor for x-axis
  geom_boxplot(
    aes(
      ymin = loan_amount_q25 - 1.5 * loan_amount_iqr,    # Lower whisker
      lower = loan_amount_q25,    # Box lower hinge
      middle = loan_amount_median, # Median line
      upper = loan_amount_q75,    # Box upper hinge
      ymax = loan_amount_q75 + 1.5 * loan_amount_iqr     # Upper whisker
    ),
    stat = "identity"  # Use raw data, not summary statistics from ggplot
  ) +
  geom_point(aes(y = loan_amount_mean), color = "red", size = 3) +  # Add mean as a red point
  labs(
    title = "Loan Amount Boxplot by Year",
    x = "Year",
    y = "Loan Amount"
  ) +
  theme_minimal()

ggsave(filename = paste0(FIGURE, "boxplot_loan_amount.pdf"))

## Boxplot for income

boxplot_income <- ggplot(desc_stats_tot, aes(x = factor(year))) +  # Use year as a factor for x-axis
  geom_boxplot(
    aes(
      ymin = income_q25 - 1.5 * income_iqr,    # Lower whisker
      lower = income_q25,    # Box lower hinge
      middle = income_median, # Median line
      upper = income_q75,    # Box upper hinge
      ymax = income_q75 + 1.5 * income_iqr     # Upper whisker
    ),
    stat = "identity"  # Use raw data, not summary statistics from ggplot
  ) +
  geom_point(aes(y = income_mean), color = "red", size = 3) +  # Add mean as a red point
  labs(
    title = "Income Boxplot by Year",
    x = "Year",
    y = "Income"
  ) +
  theme_minimal()

ggsave(filename = paste0(FIGURE, "boxplot_income.pdf"))





############################### END ###########################################+