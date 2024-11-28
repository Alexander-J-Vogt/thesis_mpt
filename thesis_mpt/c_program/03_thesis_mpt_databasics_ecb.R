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


# 1. Banking Structural Statistic Indicator ====================================

# Read bulk download dataset
df_ssi_raw <- read.csv(paste0(A, "b_ecb/structural_finance_indicator.csv"), colClasses = "character")

# Create list of countries
euro_cntry <- c("AT", "BE", "CY", "DE", "EE", "ES", "FI", "FR", "GR", "IE", 
                "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK")
euro_cntry_relevant <- c("AT", "BE", "FI", "FR", "GR", "IE", "IT", "NL", 
                         "PT", "ES", "SI", "SK")

# Read, filter and bring data into wide forma
df_ssi <- df_ssi_raw |> 
  filter(REF_AREA %in% euro_cntry_relevant) |> 
  filter(SSI_INDICATOR %in% c("H10", "H20")) |> # Include only H10 and H20
  select(REF_AREA, TIME_PERIOD, SSI_INDICATOR, OBS_VALUE) |> 
  pivot_wider(names_from = "SSI_INDICATOR", values_from = "OBS_VALUE") |> # Wide Format
  rename(hhi_ci_total_assets = H10,
         hhi_total_credit = H20,
         country = REF_AREA,
         year = TIME_PERIOD
  )

# 2. Consolidated Banking Data =================================================

df_cbd2 <- read.csv(paste0(A, "b_ecb/cbd2.csv"))













###############################################################################+
################################# ENDE ########################################+
###############################################################################+