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

# 01. Import dataset ===========================================================

main <- LOAD(dfinput = "38_thesis_mpt_eu_varcreation_merge")

# 02. Clean Data ===============================================================

df_main <- main |> 
  # Filter
  filter(!(country == "GR" & month < as.Date("2003-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01")) |> # Filter for time period 2003 to 2023
  # Time Variables
  mutate(
    year = year(month),
    .after = month
  ) |> 
  relocate(year, .after = country)

# 03. Scaling Variables ========================================================

# Scaling variables
df_main <- df_main |> 
  rename(
    lending_rate_total = lending_hp_total_outst_amount,
    lending_rate_1year = lending_hp_over_1_year_outst_amount,
    lending_rate_5year = lending_hp_over_5_years_outst_amount
  ) |> 
  mutate(
    # Percent to Decimal
    lending_rate_total_deci = lending_rate_total / 100,
    lending_rate_1year_deci = lending_rate_1year / 100,
    lending_rate_5year_deci = lending_rate_5year / 100, 
    reer_deci = reer / 100,
    ur_deci = ur / 100,
    
    # Log variables
    log_cr = log(cr_outst_amount_EUR),
    log_tl = log(tl_outst_amount_EUR),
    log_dl = log(dl_outst_amount_EUR),
    log_hp_total_amount =  log(hp_outst_amount_EUR)) 

# 04. New Units for Variables ==================================================

# # Delta
# df_main <- df_main |> 
#   # Make sure that the dataset is order by county, year
#   arrange(country, month) |> 
#   group_by(country) |> 
#   mutate(
#     # Delta Lending Rates
#     delta_lending_rate_total = lending_rate_total - lag(lending_rate_total),
#     delta_lending_rate_1year = lending_rate_1year - lag(lending_rate_1year),
#     delta_lending_rate_5year = lending_rate_5year - lag(lending_rate_5year),
#     
#     # Delta Total Amount
#     delta_hp_total = hp_outst_amount_EUR - lag(hp_outst_amount_EUR),
#     
#     # Log delta Total Amount
#     log_delta_hp_total_amount = log(hp_outst_amount_EUR) - lag(log(hp_outst_amount_EUR))
#   ) |> 
#   ungroup()


# 05. Monthly Dataset ==========================================================

# Save as monthly dataset
SAVE(df_main, paste0(MAINNAME, "_m"))


# 06. Annual dataset =========================================================== 

# Collapse data to annual data by taking the mean over a month
df_main_a <- df_main |> 
  dplyr::select(-c("month")) |> 
  group_by(country, year) |> 
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# 05. Save =====================================================================

# Save annual dataset
SAVE(dfx = df_main_a, paste0(MAINNAME, "_a"))

###############################################################################+
################################# ENDE ########################################+
###############################################################################+