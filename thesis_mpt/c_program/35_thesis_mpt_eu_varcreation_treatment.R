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

# 01. Worldscope - Market Concentration - Quaterly =============================

df_worldscope <- LOAD(dfinput = "05_thesis_mpt_databasics_worldscope")

# Create measure for mark concentration by total asset and total liabilities 
# for each country in each year
df_worldscope <- df_worldscope |> 
  group_by(country, quarter) |> 
  mutate(
    cntry_year_asset = sum(total_assets, na.rm = TRUE),
    cntry_year_liabilities = sum(total_liabilities,  na.rm = TRUE)
  ) |>  # Sum of Total Assets and Liabilities in a year for a country
  ungroup() |> 
  mutate(
    market_share_assets = total_assets / cntry_year_asset,
    market_share_liabilities = total_liabilities / cntry_year_liabilities
  ) |>  # Calculating the market share for each bank in a year for a country
  mutate(
    market_share_assets2 = market_share_assets^2,
    market_share_liabilities2 = market_share_liabilities^2
  ) |> 
  group_by(country, quarter) |> 
  mutate(
    hhi_assets = sum(market_share_assets2),
    hhi_liabilities = sum(market_share_liabilities2)
  )

# Create Qarter-level Dataset
df_hhi <- df_worldscope |> 
  distinct(country, quarter, hhi_assets, hhi_liabilities) |> 
  filter(quarter > as.Date("2002-12-01")) |> 
  mutate(year = year(quarter), .before = quarter)


# 02. SSI - Market Concentration - Annual ======================================

df_ssi <- LOAD(dfinput = "03_thesis_mpt_databasics_ecb_a")

df_ssi <- df_ssi |>
  select(-share_top5_largest_ci_total_asset)
  # mutate(share_top5_largest_ci_total_asset = share_top5_largest_ci_total_asset / 100)


# 03. Create Dataset ===========================================================

# df_main <- df_hhi |> 
#   left_join(df_ssi, by = c("country", "year"))
df_main <- df_ssi
# 04. SAVE =====================================================================

SAVE(dfx = df_main)

###############################################################################+
################################# ENDE ########################################+
###############################################################################+


