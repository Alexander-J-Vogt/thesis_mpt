# TARGET: Determine Market Concentration in counties + Create treatment/Control group + Create annual data for FFR
# INDATA: sod_banks
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

# 1. Summary of Deposits =======================================================

## 1.1 Load the Dataset ---------------------------------------------------------

# Load the Summary of Deposits for the period 1994 to 2023
sod <- LOAD(dfinput = "07_thesis_mpt_databasics_sod", dfextension = ".rda")
setDT(sod)

setnames(
  sod,
  old = "stnumbr",
  new = "state"
)

# Select the relevant variables for creating HHI by county-level
sod <- sod[, .(year, fips, state, depsumbr, rssdid)]
setorder(sod, year, fips, rssdid)

# Exclude all irrelevant time periods
sod <-  sod[inrange(year, 2004 , 2023)]


## 1.2 Create HHI by county -----------------------------------------------------

# Calculate the sum of deposit by year, financial institution and fips
sod[, bank_cnty_dep := sum(depsumbr), by = .(rssdid, fips, year)]

# Calculate the sum of deposits by year and fips-code
sod[, cnty_tot_dep := sum(depsumbr), by = .(fips, year)]

# Calculate the market share and square the value of it. Additionally, substitute
# all NaN with 0, which were produced when only one bank is active in the whole county.
sod[, bank_market_share := bank_cnty_dep / cnty_tot_dep * 100]
sod[, bank_market_share := ifelse(is.nan(bank_market_share), 0, bank_market_share)]
sod[, bank_market_share_sq := bank_market_share^2]

# Drop all duplicates in order to create a dataset on bank-cnty-year level:
# One observation is equal to the deposits of one bank in one county in one year.
sod <- unique(sod, by = c("year", "fips", "rssdid"))

# Calculate the HHI for each county based on the deposits of one banks in one county in one year.
sod <- sod[, .(hhi = sum(bank_market_share_sq) / 10000) , by = .(fips, year)]


# 1.3 Divide U.S. into High- and Low- Market Concentration Area ================

# Divide the U.S. into high and low market concentration counties by median

# Take mean of hhi by fips over time
sod[, hhi_mean := mean(hhi), by = fips]

# Calculate the median based on fips
sod_median <- distinct(sod, fips, hhi_mean)
hhi_median <- median(sod_median$hhi_mean)

# Determine high and low market concentration counties
sod[, d_hhi_indicator := if_else(hhi_mean > hhi_median, 1, 0)]


# 2. Federal Funds Rate ========================================================

# Load raw dataset of the federal funds rate
# ffr_data <- LOAD(dfinput = "ffr")
ffr_data <- LOAD(dfinput = "11_thesis_mpt_databasics_us_ffr")
setDT(ffr_data)

# Creating measures of the federal funds rate
# Method 1: Average FFR of each year
ffr_data[, ffr_mean := mean(ffr), by = year]
ffr_data[, d_ffr_mean_1perc := as.integer(ifelse( ffr_mean < 1, 1,0))]
ffr_data[, d_ffr_mean_2perc := as.integer(ifelse( ffr_mean < 2, 1,0))]
ffr_data[, ffr_mean := NULL]

# Method 2: Last FFR of each year
ffr_data[, ffr_last := ffr[.N], by = .(year)]
ffr_data[, d_ffr_last_1perc := as.integer(ifelse(ffr_last < 1, 1, 0))]
ffr_data[, d_ffr_last_2perc := as.integer(ifelse(ffr_last < 2, 1, 0))]
ffr_data[, ffr_last := NULL]

# Dummy Variable for before and after 2008:
# From the year 2007 to 8 the US experienced a drop in the FFR by 4.08 as the 
# Great Recession unfolded.
ffr_data[, d_great_recession := as.integer(ifelse(year == 2008, 1, 0))]

# Reduce dataset to yearly dataset
ffr_data <- unique(ffr_data, by = c("year"))

# Save dataset
SAVE(dfx = ffr_data, namex = "ffr_annual")


# 3. Monetary Shock ============================================================

# Import Monetary Shocks
monetary_shock <- LOAD("24_thesis_mpt_databasics_monetary_shock_us")

# Select Conventional Monetary Policy Shocks
df_monetary_shock <- monetary_shock |> 
  dplyr::select(year, starts_with("NS"), starts_with("MP"))

# 3. Combine Federal Funds Rate & SOD ==========================================

# Combine SOD dataset and FFR dataset by year
df_treatment <- sod |> 
  left_join(ffr_data, by = c("year")) |> 
  left_join(df_monetary_shock, by = c("year")) |> 
  dplyr::select(-date)


# 4. Create Interaction Term for Endogenous Variables ==========================

df_treatment <- df_treatment |> 
  mutate(
    # Nakamura & Steinsson (2018)
    ## Sum
    I_HHI_NS_SUM_1  = d_ffr_mean_1perc * NS_total * hhi,
    I_HHI_NS_SUM_2  = d_ffr_mean_2perc * NS_total * hhi,
    I_HHI_NS_SUM    = NS_total * hhi,
    I_NS_SUM_2      = d_ffr_mean_2perc * NS_total,
    I_NS_SUM_1      = d_ffr_mean_1perc * NS_total,
    ## Mean
    I_HHI_NS_MEAN_1 = d_ffr_mean_1perc * NS_total_mean * hhi,
    I_HHI_NS_MEAN_2 = d_ffr_mean_2perc * NS_total_mean * hhi,
    I_HHI_NS_MEAN   = NS_total_mean * hhi,
    I_NS_MEAN_2     = d_ffr_mean_2perc * NS_total_mean,
    I_NS_MEAN_1     = d_ffr_mean_1perc * NS_total_mean,
    # Jarocinski & Karadi - Median (2020)
    ## Sum
    I_HHI_JK_MEDIAn_SUM_1  = d_ffr_mean_1perc * MP_median_sum * hhi,
    I_HHI_JK_MEDIAn_SUM_2  = d_ffr_mean_2perc * MP_median_sum * hhi,
    I_HHI_JK_MEDIAn_SUM    = MP_median_sum  * hhi,
    I_JK_SUM_MEDIAn_2      = d_ffr_mean_2perc * MP_median_sum ,
    I_JK_SUM_MEDIAn_1      = d_ffr_mean_1perc * MP_median_sum,
    ## Mean
    I_HHI_JK_MEDIAN_MEAN_1  = d_ffr_mean_1perc * MP_median_mean * hhi,
    I_HHI_JK_MEDIAN_MEAN_2  = d_ffr_mean_2perc * MP_median_mean* hhi,
    I_HHI_JK_MEDIAN_MEAN    = MP_median_mean * hhi,
    I_JK_MEDIAN_MEAN_2      = d_ffr_mean_2perc * MP_median_mean,
    I_JK_MEDIAN_MEAN_1      = d_ffr_mean_1perc * MP_median_mean,
    # Jarocinski & Karadi - PM (2020)
    ## Sum
    I_HHI_JK_PM_SUM_1  = d_ffr_mean_1perc * MP_pm_sum * hhi,
    I_HHI_JK_PM_SUM_2  = d_ffr_mean_2perc * MP_pm_sum * hhi,
    I_HHI_JK_PM_SUM    = MP_pm_sum * hhi,
    I_JK_PM_SUM_2      = d_ffr_mean_2perc * MP_pm_sum,
    I_JK_PM_SUM_1      = d_ffr_mean_1perc * MP_pm_sum,
    ## Mean
    I_HHI_JK_PM_MEAN_1  = d_ffr_mean_1perc * MP_median_mean * hhi,
    I_HHI_JK_PM_MEAN_2  = d_ffr_mean_2perc * MP_median_mean * hhi,
    I_HHI_JK_PM_MEAN    = MP_median_mean * hhi,
    I_JK_PM_MEAN_2      = d_ffr_mean_2perc * MP_median_mean,
    I_JK_PM_MEAN_1      = d_ffr_mean_1perc * MP_median_mean
)

# 5. Saving the dataset ========================================================

SAVE(df_treatment, namex = MAINNAME)


########################## ENDE ###############################################+