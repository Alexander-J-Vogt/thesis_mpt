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


# 01. SSI - Market Concentration - Annual ======================================

# Import
ssi <- LOAD(dfinput = "03_thesis_mpt_databasics_ecb_a")

# Select HHI for Total Assets (only variable with no missings)
df_ssi <- ssi |>
  select(-c("share_top5_largest_ci_total_asset", "hhi_total_credit"))

# 02. Policy Rates - Monthly ===================================================

# Import
ecb_policy_rate <- LOAD("03_thesis_mpt_databasics_ecb_policy_rates")

# ZLB measures
df_ecb_policy_rate <- ecb_policy_rate |> 
  # ZLB indicators
  mutate(
    d_mpr_2 = if_else(mpr <= 2, 1, 0),
    d_mpr_1 = if_else(mpr <= 1, 1, 0),
    d_dfr_2 = if_else(dfr <= 2, 1, 0),
    d_dfr_1 = if_else(dfr <= 1, 1, 0),
    d_dfr_nirp = if_else(dfr <= 0, 1, 0)
  ) 

# 03. Monetary Shock ===========================================================

# Load Eurozone Monetary Shock
monetary_shock <- LOAD("24_thesis_mpt_databasics_monetary_shock_eurozone")

# SD
sd_altavilla <- sd(monetary_shock$Altavilla_target)
sd_jarocinski <- sd(monetary_shock$MP_median)


# Select Relevant Variables
df_monetary_shock <- monetary_shock |> 
  select(month, MP_median, Altavilla_target)|> 
  # Cintractionary & Expansionary Monetary Shock
  mutate(
    Altavilla_contractionary = -1 * Altavilla_target,
    Altavilla_expansionary = Altavilla_target,
    MP_median_contractionary = -1 * MP_median / sd_jarocinski, # Transform the shock from 1 bps to 1 sd
    MP_median_expansionary = MP_median / sd_jarocinski # Transform the shock from 1 bps to 1 sd
  )

# 04. Merge Dataset ============================================================

# Create monthly dataset
df_month <- tibble(month = seq(as.Date("2000-01-01"), as.Date("2023-12-01"), by = "month"))

# Merge + Restrict to relevant period
df_merge <- df_month |>
  mutate(year = year(month)) |> 
  # Merge
  full_join(df_ssi, by = "year", relationship = "many-to-many") |> 
  left_join(df_ecb_policy_rate, by = "month") |> 
  left_join(df_monetary_shock, by = "month") |> 
  # Get the time right
  filter(month >= as.Date("2000-01-01")) |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  # Arrange + Select relevant var
  arrange(country, month) |> 
  select(-year)


# 04. SAVE =====================================================================

SAVE(dfx = df_merge)

###############################################################################+
################################# ENDE ########################################+
###############################################################################+


