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

df_main_raw <- LOAD(dfinput = "38_thesis_mpt_eu_varcreation_merge")

# 02. Clean Data ===============================================================

df_main <- df_main_raw |> 
  filter(!(country == "GR" & month < as.Date("2003-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01"))

df_main <- df_main |> 
  relocate(year, .after = country)

# 03. Imputation of Missings ===================================================

## Determine the NAs
# Get data ready for imputation
df_impute <- df_main |> 
  select(-c("year", "quarter", "hhi_total_credit"))

# Control for NAs
vis_miss(df_impute)
miss_var_summary(df_impute)
gg_miss_var(df_impute)

# GER: 2003 - 2006
df_main |> filter(is.na(ur))

# GR: from 2013 missing from time to time
df_main |> filter(is.na(lending_hp_total_outst_amount))

# BE: 2003 to 2006
df_main |> filter(is.na(lending_hp_over_5_years_outst_amount))

# Impute NAs with the help of non-parametric estimation via ML Algorithm (missForest-package)
df_imputed_mf <- missForest(
  df_impute[,-c(1,2)],
  verbose = TRUE
  )

# Copy of df_main
df_main_imputed <- df_main

# Impute missings
df_main_imputed$ur <- df_imputed_mf$ximp$ur
df_main_imputed$lending_hp_total_outst_amount <- df_imputed_mf$ximp$lending_hp_total_outst_amount
df_main_imputed$lending_hp_over_5_years_outst_amount <- df_imputed_mf$ximp$lending_hp_over_5_years_outst_amount

# 04. Annual dataset =========================================================== 

# Collapse data to annual data by taking the mean over a month
df_main_a <- df_main_imputed |> 
  select(-c("quarter", "month", "hhi_total_credit")) |> 
  group_by(country, year) |> 
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# 05. Save =====================================================================

# Save annual dataset
SAVE(dfx = df_main_a)

###############################################################################+
################################# ENDE ########################################+
###############################################################################+