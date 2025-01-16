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

df_ecb <- LOAD(dfinput = "03_thesis_mpt_databasics_ecb_m")

df_eurostat <- LOAD(dfinput = "04_thesis_mpt_databasics_eurostat_m")

df_imf <- LOAD(dfinput = "06_thesis_mpt_databasics_imf")

# 02. Relevant Variables =======================================================

# Select relevant variables from the BSI
df_ecb <- df_ecb |> 
  select(country, month, cr_outst_amount_EUR, dl_outst_amount_EUR, tl_outst_amount_EUR) 
# |> 
  # mutate(quarter = yq(paste0(year(month), "-Q", quarter(month))), .before = "month")

# 03. Merge to Dataset =========================================================

# Merge datasets
df_controls <- df_ecb |> 
  full_join(df_eurostat, by = c("country", "month")) |> 
  full_join(df_imf, by = c("country", "month"))

# 
if (DEBUG) {
df_controls <- df_controls |> 
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01"))


miss_var_summary(df_controls)
gg_miss_var(df_controls)
vis_miss(df_controls)
}

# 04 SAVE ======================================================================

SAVE(dfx = df_controls)

###############################################################################+
################################# ENDE ########################################+
###############################################################################+