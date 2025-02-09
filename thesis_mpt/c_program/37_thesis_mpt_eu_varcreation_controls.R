# TARGET: 
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
# MAIN PART ####

# 01. Import all datasets ======================================================

# Import 
df_ecb <- LOAD(dfinput = "03_thesis_mpt_databasics_ecb_m")

df_eurostat <- LOAD(dfinput = "04_thesis_mpt_databasics_eurostat_m")

df_imf <- LOAD(dfinput = "06_thesis_mpt_databasics_imf")

df_eurostat_q <- LOAD(dfinput = "04_thesis_mpt_databasics_eurostat_q")


# 02. Relevant Variables =======================================================

# Select relevant variables from the BSI
df_ecb <- df_ecb |> 
  select(country, 
         month, 
         cr_outst_amount_EUR, # Credit & Reserves
         dl_outst_amount_EUR, # Deposit and Liabilities
         tl_outst_amount_EUR  # Total Assets & Liabilities
         ) 


# 03. Real GDP =================================================================

# df_real_gdp <- df_eurostat |>
#   select(country, month, ip) |> 
#   full_join(df_eurostat_q, by = c("country", "month")) |> 
#   mutate(real_gdp = as.double(real_gdp)) |> 
#   mutate(real_gdp = nafill(real_gdp, type = "locf"))

# install.packages("tsbox")
library(tsbox)

EA <- c("AT", "BE", "FI", "FR", "DE", "IE", "IT", 
  "NL", "PT", "ES", "SI", "SK")

list_gdp <- purrr::map(EA, function (x) {
  
  # Get data to ts-object
  df_gdp <- df_eurostat_q |> 
    filter(country == x) |> 
    mutate(real_gdp = as.double(real_gdp)) |> 
    select(real_gdp)
  
  ip <- df_eurostat |> 
    filter(country == x) |> 
    select(ip)
  
  ts_gdp <- ts(data = df_gdp$real_gdp, start = 2003, frequency = 4)
  
  ts_ip <- ts(data = ip$ip, start = 2003, frequency = 12)
  
  # Use Chow-Lin Interpolation to get monthly real GDP
  real_gdp <- tempdisagg::td(ts_gdp ~ ts_ip, to = 12, method = "chow-lin-maxlog" )
  real_gdp_predict <- predict(real_gdp)
  
  # Transform back to data frame
  if (x == "GR") {
    time <- seq(as.Date("2001-01-01"), as.Date("2023-12-01"), by = "month")
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
    real_gdp = as.vector(real_gdp_predict)
  )
  
  # Return Value
  return(df_long)

})

df_gdp <- bind_rows(list_gdp)


# 03. Merge to Dataset =========================================================

# Merge datasets
df_controls <- df_ecb |> 
  full_join(df_eurostat, by = c("country", "month")) |> 
  full_join(df_imf, by = c("country", "month")) |> 
  full_join(df_gdp, by = c("country", "month")) |> 
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01")) |> 
  filter(!(country == "GR" & month < as.Date("2003-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) # Filter for years with SK being part of the Eurozone

# 04. Impute Missings ==========================================================

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

# 04.1 Check Imputations on plausibility ---------------------------------------

df_non_imp <- df_controls_imp |> 
  filter(country %in% c("BE", "DE", "FR", "NL")) |> 
  select(month, country, ur) |> 
  mutate(
    country = as.character(country),
    month = as.Date(month)
  ) |> 
  rename(ur = ur )

df_imp <- df_controls |> 
  filter(country %in% c("BE", "DE", "FR", "NL")) |>
  select(country, month, ur) |> 
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
#' to what is found at the Statistiches Bundesamt

  
# 05. SAVE =====================================================================

SAVE(dfx = df_controls)

###############################################################################+
################################# ENDE ########################################+
###############################################################################+