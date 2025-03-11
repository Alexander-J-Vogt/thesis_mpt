# TARGET: Merge Control Variables from ECB, Eurostat, BSI and IMF
# OUTDATA/ OUTPUT: MAINNAME 

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

# 01. Import all datasets ======================================================

# Import 
df_ecb <- LOAD(dfinput = "03_thesis_mpt_databasics_ecb_m")

df_eurostat <- LOAD(dfinput = "04_thesis_mpt_databasics_eurostat_m")

df_imf <- LOAD(dfinput = "06_thesis_mpt_databasics_imf")

df_eurostat_q <- LOAD(dfinput = "04_thesis_mpt_databasics_eurostat_q")

df_eurostat_a <- LOAD(dfinput = "04_thesis_mpt_databasics_eurostat_a")


# 02. Relevant Variables =======================================================

# Select relevant variables from the BSI
df_ecb <- df_ecb |> 
  dplyr::select(country, 
         month, 
         cr_outst_amount_EUR, # Credit & Reserves
         dl_outst_amount_EUR, # Deposit and Liabilities
         tl_outst_amount_EUR, # Total Assets / Liabilities
         deposit_rate,        # Deposit Rate 
         total_loan,          # Loan Volume
         MMF,                 # Money Market Fund
         overnight_deposits,  # Overnight Deposit Volume 
         bonds                # Bond Volume
         ) |> 
  mutate(
    loan_share = total_loan / (total_loan + bonds) * 100,
    deposit_share = overnight_deposits / (overnight_deposits + MMF) * 100,
    log_credit_reserves = log(cr_outst_amount_EUR),
    log_deposit = log(dl_outst_amount_EUR),
    log_assets = log(tl_outst_amount_EUR),
    log_overnight_deposits = log(overnight_deposits),
    log_total_loan = log(total_loan)
  ) |> 
  dplyr::select(-c("MMF", "bonds", ends_with("EUR"), "total_loan", "overnight_deposits")) |> 
  mutate(deposit_rate = as.numeric(deposit_rate))


# 03. Real GDP =================================================================

## 03.1 Real GDP by Country ----------------------------------------------------


# List of EA countries in 
EA <- c("AT", "BE", "FI", "FR", "DE", "GR", "IE", "IT", 
  "NL", "PT", "ES", "SI", "SK")

list_gdp <- purrr::map(EA, function (x) {
  x <- "IE"
   
  # Transform back to data frame
  if (x == "GR") {
    time <- seq(as.Date("2003-01-01"), as.Date("2023-12-01"), by = "month")
  } else if (x == "SI") {
    time <- seq(as.Date("2007-01-01"), as.Date("2023-12-01"), by = "month")
  } else if (x == "SK") {
    time <- seq(as.Date("2009-01-01"), as.Date("2023-12-01"), by = "month")
  } else {
    time <- seq(as.Date("2003-01-01"), as.Date("2023-12-01"), by = "month")
  }
  
  # Get data to ts-object
  df_gdp <- df_eurostat_q |> 
    filter(country == x) |> 
    filter(month %in% time) |> 
    dplyr::select(real_gdp)
  
  ip <- df_eurostat |> 
    filter(country == x) |> 
    filter(month %in% time) |> 
    dplyr::select(ip)
  
  if (x %in% c("SI", "SK")) {
    min <- min(year(time))
  } else {
    min <- 2003
  }
  
  # Determine TS
  ts_gdp <- ts(data = df_gdp$real_gdp, start =  min, frequency = 4)
  ts_ip <- ts(data = ip$ip, start = min, frequency = 12)
  
  # Use Chow-Lin Interpolation to get monthly real GDP
  real_gdp <- tempdisagg::td(ts_gdp ~ ts_ip, to = 12, method = "chow-lin-maxlog", conversion = "last")
  real_gdp_predict <- predict(real_gdp)
  
  # Main dataset
  df_long <- data.frame(
    country = x,
    month = time,
    gdp = as.vector(real_gdp_predict)
  )
  
  # Return Value
  return(df_long)

})

df_gdp <- bind_rows(list_gdp)

# Calculate annual GDP growth
df_gdp <- df_gdp |> 
  group_by(country) |> 
  mutate(gdp_country_growth = (gdp - lag(gdp, 12)) / gdp) |> 
  ungroup()


## 03.2 EA Real GDP ------------------------------------------------------------

# Import 
# Get data to ts-object
df_gdp_ea <- df_eurostat_q |> 
  filter(country == "EA") |> 
  mutate(real_gdp = as.double(real_gdp)) |> 
  dplyr::select(real_gdp)

ts_gdp_ea <- ts(data = df_gdp_ea$real_gdp, start = 2003, frequency = 4)

# Use Denton-Cholette Interpolation to get annual real GDP
real_gdp_ea <- tempdisagg::td(ts_gdp_ea ~ 1, to = 12, method = "denton-cholette", conversion = "last")
real_gdp_ea_predict <- predict(real_gdp_ea)

# DF
df_real_gdp_ea <- data.frame(
  month = seq(as.Date("2003-01-01"), as.Date("2023-12-01"), by = "month"),
  gdp_ea = as.vector(real_gdp_ea_predict)
)

# Calculate Annual GDP growth
df_real_gdp_ea <- df_real_gdp_ea |> 
  mutate(gdp_ea_growth = (gdp_ea - lag(gdp_ea, 12)) / gdp_ea * 100)
 

# 04. HICP =====================================================================

## 04.1 HICP by Country --------------------------------------------------------

# All countries excl IP
df_eurostat_country <- df_eurostat |> 
  filter(country != "EA") |> 
  dplyr::select(-ip)


## 04.2 HICP for EA ------------------------------------------------------------

# HICP for EA
df_hicp_ea <- df_eurostat |> 
  dplyr::select(country, month, hicp) |> 
  filter(country == "EA") |> 
  dplyr::select(-country) |> 
  rename(hicp_ea = hicp)

# 05. HPI ======================================================================

# EA Countries in Sample
EA <- c("AT", "BE", "FI", "FR", "DE", "GR", "IE", "IT", 
        "NL", "PT", "ES", "SI", "SK")

# Create a monthly frequency from the quarterly HPI
list_hpi <- purrr::map(EA, function (x) {
  
  # Get data to ts-object
  df_hpi <- df_eurostat_q |> 
    filter(country == x) |> 
    dplyr::select(hpi)
  
  ts_hpi <- ts(data = df_hpi$hpi, start = 2003, frequency = 4)

  
  # Use Chow-Lin Interpolation to get monthly real GDP
  hpi <- tempdisagg::td(ts_hpi ~ 1, to = 12, method = "denton-cholette", conversion = "last")
  hpi_predict <- predict(hpi)
  
  # Transform back to data frame
  if (x == "GR") {
    time <- seq(as.Date("2003-01-01"), as.Date("2023-12-01"), by = "month")
  } else if (x == "SI") {
    time <- seq(as.Date("2007-01-01"), as.Date("2023-12-01"), by = "month")
  } else if (x == "SK") {
    time <- seq(as.Date("2009-01-01"), as.Date("2023-12-01"), by = "month")
  } else {
    time <- seq(as.Date("2003-01-01"), as.Date("2023-12-01"), by = "month")
  }
  
  
  df_long <- data.frame(
    country = x,
    month = time,
    hpi = as.vector(hpi_predict)
  )
  
  # Return Value
  return(df_long)
  
})

df_hpi <- bind_rows(list_hpi)

# Calcualte Annual HPI growth
df_hpi <- df_hpi |> 
  group_by(country) |> 
  mutate(hpi_growth = (hpi - lag(hpi, 12)) / hpi * 100) |> 
  ungroup()


# 06. Homeownership Rate  ====================================

# Create a monthly frequency from the annual Homeownership Rate Data
list_hosr <- purrr::map(EA, function (x) {
  
  # Get data to ts-object
  df_hosr <- df_eurostat_a |> 
    arrange(country, year) |> 
    filter(!is.na(hosr)) |> 
    filter(country == x) |>
    dplyr::select(hosr)
  
  # Create TS
  ts_hosr <- ts(data = df_hosr$hosr, start = 2003, frequency = 1)

  # Use Chow-Lin Interpolation to get monthly real GDP
  hosr <- tempdisagg::td(ts_hosr ~ 1, to = 12, method = "denton-cholette", conversion = "last")
  hosr_predict <- predict(hosr)
  
  time <- seq(as.Date("2003-01-01"), as.Date("2023-12-01"), by = "month")
  
  df_long <- data.frame(
    country = x,
    month = time,
    hosr = as.vector(hosr_predict)
  )
  
  # Return Value
  return(df_long)
  
})

# Combine to DF
df_hosr <- bind_rows(list_hosr)


# 07. Merge to Dataset =========================================================

# Merge datasets
df_controls <- df_ecb |>
  # Merge Dataset
  full_join(df_eurostat_country, by = c("country", "month")) |> # REER, HICP, UR by Country + EUR/USD Exchange Rate
  full_join(df_imf, by = c("country", "month")) |> # Commodity Index by country
  full_join(df_gdp, by = c("country", "month")) |> # GDP by country
  full_join(df_hicp_ea, by = c("month")) |> # EA HICP
  full_join(df_real_gdp_ea, by = c("month")) |>  # EA GDP
  full_join(df_hpi, by = c("country", "month")) |> # House Price Index
  full_join(df_hosr, by = c("country", "month")) |> 
  # Filter for relevant time period
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01")) |> 
  filter(!(country == "GR" & month < as.Date("2003-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) # Filter for years with SK being part of the Eurozone


# 08. Impute Missings ==========================================================

# Prepare dataset for Imputation
df_controls_imp <- df_controls |> 
  mutate(
    month = as.numeric(month),
    country = as.factor(country)
  ) |> 
  arrange(country, month) 
  
# Impute Missings
df_controls_imputed <- missForest(df_controls_imp)
df_controls_ximp <- df_controls_imputed$ximp


# Get the data into its original data formats
df_controls <- df_controls_ximp |> 
  # month to date format & country to character format
  mutate(
    month = as.Date(month),
    country = as.character(country)
  ) |> 
  # Select and arrange variables
  arrange(country, month)

## 08.1 Check Imputations on plausibility ---------------------------------------

df_non_imp <- df_controls_imp |> 
  filter(country %in% c("BE", "DE", "FR", "NL")) |> 
  dplyr::select(month, country, ur) |> 
  mutate(
    country = as.character(country),
    month = as.Date(month)
  ) |> 
  rename(ur = ur )

df_imp <- df_controls |> 
  filter(country %in% c("BE", "DE", "FR", "NL")) |>
  dplyr::select(country, month, ur) |> 
  rename(ur_imp = ur)

df_combined <- df_imp |> 
  full_join(df_non_imp, by = c("country", "month"))

# Imputed Variables
ggplot(df_combined, aes(x = month, y = ur_imp, color = country)) +
  geom_point() + 
  geom_line(data = df_combined %>% filter(!is.na(ur_imp))) +  # Connect only non-missing values
  labs(title = "Interest rate on 5 years lending | Imputed ",
       x = "Month", 
       y = "Interest Rate") +
  theme_minimal()

# Non-Imputed Variables
ggplot(df_combined, aes(x = month, y = ur, color = country)) +
  geom_point() + 
  geom_line(data = df_combined %>% filter(!is.na(ur))) +  # Connect only non-missing values
  labs(title = "Interest rate on 5 years lending",
       x = "Month", 
       y = "Interest Rate") +
  theme_minimal()

#' As approximation of the UR in the mid-2000s it is okay but is a bit off compared 
#' to what is found at the Statistiches Bundesamt.


# 09. Crisis Indicator =========================================================

# State-Dependent Indicator
df_controls <- df_controls |> 
  mutate(
    # Financial Crisis
    d_fincrisis = if_else(inrange(month, as.Date("2008-04-01"), as.Date("2009-06-01")), 1, 0),
    # Euoprean Sovereing Debt Crisis
    d_eurocrisis = if_else(inrange(month, as.Date("2011-06-01"), as.Date("2013-04-01")), 1, 0),
    # Countries with a large share of ARM mortgages by Albertazzi, Ugo, Fulvia Fringuellotti, and Steven Ongena (2024)
    d_countries_arm = ifelse(country %in% c("AT", "GR", "IT", "PT", "ES"), 1, 0)
  ) |> 
  mutate(year = year(month))

# Year Dummy Variable
df_controls <- fastDummies::dummy_cols(df_controls, select_columns = "year", remove_first_dummy = TRUE)
df_controls <- df_controls |> dplyr::select(-year)


# 10. SAVE =====================================================================

SAVE(dfx = df_controls)

###############################################################################+
################################# ENDE ########################################+
###############################################################################+