# TARGET: Create Main Sample
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
    
    # Log
    log_hp_total_amount =  log(hp_outst_amount_EUR),
    
    # Intermediation margin
    int_margin = lending_rate_total - deposit_rate
    ) 


# 04. Monthly Dataset ==========================================================

# Save as monthly dataset
SAVE(df_main, paste0(MAINNAME, "_m"))



###############################################################################+
################################# ENDE ########################################+
###############################################################################+