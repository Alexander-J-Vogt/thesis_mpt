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
  )  |> 
  mutate(
    d_dfr_above = if_else(dfr > 0, 1, 0)
  )
  
  
# 03. Monetary Shock ===========================================================

# Load Eurozone Monetary Shock
df_monetary_shock <- LOAD("24_thesis_mpt_databasics_monetary_shock_eurozone")

df_monetary_shock <- df_monetary_shock |> 
  mutate(
    MP_pm_positiv = if_else(MP_pm > 0, MP_pm, 0),
    MP_pm_negativ = if_else(MP_pm < 0, MP_pm, 0)
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


# 05. Create Interaction Terms =================================================

# Mean HHI 
hhi_mean <- mean(df_ssi$hhi_ci_total_assets, na.rm =T)

# Demean HHI
df_merge <- df_merge |> 
  mutate(hhi_demeaned = hhi_ci_total_assets - hhi_mean)

# Create all possible Interaction Terms
df_merge <- df_merge |> 
  # Create Interaction Terms
  mutate(
    # Three Parted Interaction Term - NIRP
    I_HHI_A_TOTAL_NIRP = hhi_ci_total_assets * altavilla_total    * d_dfr_nirp,
    I_HHI_A_POS_NIRP   = hhi_ci_total_assets * altavilla_positiv  * d_dfr_nirp,
    I_HHI_A_NEG_NIRP   = hhi_ci_total_assets * altavilla_negativ  * d_dfr_nirp,
    I_HHI_J_TOTAL_NIRP = hhi_ci_total_assets * MP_median_total    * d_dfr_nirp,
    I_HHI_J_POS_NIRP   = hhi_ci_total_assets * MP_median_positiv  * d_dfr_nirp,
    I_HHI_J_NEG_NIRP   = hhi_ci_total_assets * MP_median_negativ  * d_dfr_nirp,
    # Three Parted Interaction Term - Above Zero
    I_HHI_A_TOTAL_ABOVE = hhi_ci_total_assets * altavilla_total    * d_dfr_above,
    I_HHI_A_POS_ABOVE   = hhi_ci_total_assets * altavilla_positiv  * d_dfr_above,
    I_HHI_A_NEG_ABOVE   = hhi_ci_total_assets * altavilla_negativ  * d_dfr_above,
    I_HHI_J_TOTAL_ABOVE = hhi_ci_total_assets * MP_median_total    * d_dfr_above,
    I_HHI_J_POS_ABOVE   = hhi_ci_total_assets * MP_median_positiv  * d_dfr_above,
    I_HHI_J_NEG_ABOVE   = hhi_ci_total_assets * MP_median_negativ  * d_dfr_above,
    #Two Parted Interaction Term
    I_HHI_A_TOTAL      = hhi_ci_total_assets * altavilla_total    ,
    I_HHI_A_POS        = hhi_ci_total_assets * altavilla_positiv  ,
    I_HHI_A_NEG        = hhi_ci_total_assets * altavilla_negativ  ,
    I_HHI_J_TOTAL      = hhi_ci_total_assets * MP_median_total    ,
    I_HHI_J_POS        = hhi_ci_total_assets * MP_median_positiv  ,
    I_HHI_J_NEG        = hhi_ci_total_assets * MP_median_negativ  ,
    # Interaction Term betwenn NIRP and MS
    I_A_TOTAL_NIRP     = d_dfr_nirp * altavilla_total    ,
    I_A_POS_NIRP       = d_dfr_nirp * altavilla_positiv  ,
    I_A_NEG_NIRP       = d_dfr_nirp * altavilla_negativ  ,
    I_J_TOTAL_NIRP     = d_dfr_nirp * MP_median_total    ,
    I_J_POS_NIRP       = d_dfr_nirp * MP_median_positiv  ,
    I_J_NEG_NIRP       = d_dfr_nirp * MP_median_negativ,
    I_JPM_TOTAL_NIRP   = d_dfr_nirp * MP_pm    ,
    I_JPM_POS_NIRP     = d_dfr_nirp * MP_pm_positiv  ,
    I_JPM_NEG_NIRP     = d_dfr_nirp * MP_pm_negativ
  ) |> 
  # Demeaned Values
  mutate(
    # Three Parted Interaction Term - NIRP
    I_HHI_A_TOTAL_NIRP_demeaned    = hhi_demeaned * altavilla_total    * d_dfr_nirp,
    I_HHI_A_POS_NIRP_demeaned      = hhi_demeaned * altavilla_positiv  * d_dfr_nirp,
    I_HHI_A_NEG_NIRP_demeaned      = hhi_demeaned * altavilla_negativ  * d_dfr_nirp,
    I_HHI_J_TOTAL_NIRP_demeaned    = hhi_demeaned * MP_median_total    * d_dfr_nirp,
    I_HHI_J_POS_NIRP_demeaned      = hhi_demeaned * MP_median_positiv  * d_dfr_nirp,
    I_HHI_J_NEG_NIRP_demeaned      = hhi_demeaned * MP_median_negativ  * d_dfr_nirp,
    I_HHI_JKPM_TOTAL_NIRP_demeaned = hhi_demeaned * MP_pm    * d_dfr_nirp,
    I_HHI_JKPM_POS_NIRP_demeaned   = hhi_demeaned * MP_pm_positiv  * d_dfr_nirp,
    I_HHI_JKPM_NEG_NIRP_demeaned   = hhi_demeaned * MP_pm_negativ  * d_dfr_nirp,
    # Three Parted Interaction Term - Above 0
    I_HHI_A_TOTAL_ABOVE_demeaned = hhi_demeaned * altavilla_total    * d_dfr_above,
    I_HHI_A_POS_ABOVE_demeaned   = hhi_demeaned * altavilla_positiv  * d_dfr_above,
    I_HHI_A_NEG_ABOVE_demeaned   = hhi_demeaned * altavilla_negativ  * d_dfr_above,
    I_HHI_J_TOTAL_ABOVE_demeaned = hhi_demeaned * MP_median_total    * d_dfr_above,
    I_HHI_J_POS_ABOVE_demeaned   = hhi_demeaned * MP_median_positiv  * d_dfr_above,
    I_HHI_J_NEG_ABOVE_demeaned   = hhi_demeaned * MP_median_negativ  * d_dfr_above,
    #Two Parted Interaction Term
    I_HHI_A_TOTAL_demeaned      = hhi_demeaned * altavilla_total    ,
    I_HHI_A_POS_demeaned        = hhi_demeaned * altavilla_positiv  ,
    I_HHI_A_NEG_demeaned        = hhi_demeaned * altavilla_negativ  ,
    I_HHI_J_TOTAL_demeaned      = hhi_demeaned * MP_median_total    ,
    I_HHI_J_POS_demeaned        = hhi_demeaned * MP_median_positiv  ,
    I_HHI_J_NEG_demeaned        = hhi_demeaned * MP_median_negativ  ,
    I_HHI_JKPM_TOTAL_demeaned   = hhi_demeaned * MP_pm              ,
    I_HHI_JKPM_POS_demeaned     = hhi_demeaned * MP_pm_positiv      ,
    I_HHI_JKPM_NEG_demeaned     = hhi_demeaned * MP_pm_negativ
  )


# 06. SAVE =====================================================================

SAVE(dfx = df_merge)

###############################################################################+
################################# ENDE ########################################+
###############################################################################+


