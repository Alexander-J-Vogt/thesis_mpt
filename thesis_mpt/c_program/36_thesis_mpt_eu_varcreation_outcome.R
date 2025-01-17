# TARGET: Download Call Reports from FFIEC & Perform Basic Data Cleaning
# INDATA: banks_sod, pop_cnty, ur_cnty, qwi_earnings, controls_sod
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

# 01. Import Data ==============================================================

df_ecb <- LOAD(dfinput = "03_thesis_mpt_databasics_ecb_m")

# 02. Select Relevant Variables ================================================
# Create quarter variable # select relevant variables
df_hp <- df_ecb |> 
  select(country, month, hp_outst_amount_EUR, lending_hp_total_outst_amount,
         lending_hp_over_1_year_outst_amount, lending_hp_over_5_years_outst_amount) |> 
  mutate(quarter = yq(paste0(year(month), "-Q", quarter(month))), .before = "month") |> 
  mutate(quarter = as.Date(quarter)) |> 
  mutate(year = year(month), .after = "country")

if (DEBUG) {
library(naniar)
# Limit to the period between 2003 and 2023
df_hp <- df_hp |> 
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01"))

df_ecb <- df_ecb |> 
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01"))

miss_var_summary(df_ecb)
miss_var_summary(df_hp)

lapply(colnames(df_ecb), function(x) {
  cat(paste0("Variable: ", x))
  print(table(is.na(df_ecb[[x]])))
  cat("\n")
})
}

# 03. Save =====================================================================

SAVE(dfx = df_hp)

###############################################################################+
################################# ENDE ########################################+
###############################################################################+










































###############################################################################+
################################# ENDE ########################################+
###############################################################################+