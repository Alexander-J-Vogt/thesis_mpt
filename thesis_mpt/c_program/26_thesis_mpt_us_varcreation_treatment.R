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

# Select the relevant variables for creating HHI by county-level
sod <- sod[, .(year, fips, state, county, depsumbr, rssdid)]
setorder(sod, year, fips, rssdid)

# Exclude all irrelevant time periods
sod <-  sod[inrange(year, 2004 , 2023)]


## 1.2 Create HHI by county -----------------------------------------------------

# Calculate the sum of deposit by year, financial institution and fips
sod <- sod[, bank_cnty_dep := sum(depsumbr), by = .(rssdid, fips, year)]

# Calculate the sum of deposits by year and fips-code
sod <- sod[, cnty_tot_dep := sum(depsumbr), by = .(fips, year)]

# Calculate the market share and square the value of it. Additionally, substitute
# all NaN with 0, which were produced when only one bank is active in the whole county.
sod <- sod[, bank_market_share := bank_cnty_dep / cnty_tot_dep * 100]
sod <- sod[, bank_market_share := ifelse(is.nan(bank_market_share), 0, bank_market_share)]
sod <- sod[, bank_market_share_sq := bank_market_share^2]

# Drop all duplicates in order to create a dataset on bank-cnty-year level:
# One observation is equal to the deposits of one bank in one county in one year.
sod <- unique(sod, by = c("year", "fips", "rssdid"))

# Calculate the HHI for each county based on the deposits of one banks in one county in one year.
sod <- sod[, .(hhi = sum(bank_market_share_sq) / 10000) , by = .(fips, year)]

# 1.3 Mean HHI for each county ---------------------------------------

# This section does two things:
# i) Creates and indicator for each county with an HHI of 10000
# ii) Creates a dataset with the mean HHI of each county over all periods

# Calculate the mean HHI for each county over the period 2004 to 2008
# Result: Contains observations on county-level (no annual data!)
sod_hhi <- sod[, .(mean_hhi = mean(hhi)), by = fips]
sod_hhi_pre <- sod[year >= 2004 & year < 2008, .(mean_hhi_pre = mean(hhi)), by = fips]
sod_hhi <- merge(sod_hhi, sod_hhi_pre, by = c("fips"))

# Create dummy variable for counties with HHI of 10000. Counties with HHI 10000
# are associated with rural areas like counties in Alaska, Nebraska, South Dakota etc,
# There are 106 counties, which have this high market concentration over the whole period.
sod_hhi <- sod_hhi[, d_hhi_max := ifelse(mean_hhi == 10000, 1, 0)]
sod_hhi <- sod_hhi[, d_hhi_max_pre := ifelse(mean_hhi_pre == 10000, 1, 0)]

# Merge the mean_hhi of each county, the dummy for perfect market concentration
sod_highconc <- sod_hhi[, c("fips", "mean_hhi", "mean_hhi_pre", "d_hhi_max", "d_hhi_max_pre")]
sod <- base::merge(sod, sod_highconc, by = "fips")

# 1.4 Create Treatment/Control Dummy Variables ----------------------------------

# Create different dummy variables that divide the dataset into treatment and 
# control group in the main SOD datase. Thereby, sod_hhi (mean hhi on county-level)
# is used to determine median, mean and q70 of the mean_hhi.

# a) Threshold: Median
# All Counties
median_hhi_all <- median(sod_hhi$mean_hhi)
median_hhi_all_pre <- median(sod_hhi$mean_hhi_pre)
sod <- sod[, d_median_all := ifelse(mean_hhi > median_hhi_all, 1, 0)]
sod <- sod[, d_median_all_pre := ifelse(mean_hhi > median_hhi_all_pre, 1, 0)]

# b) Threshold: Mean
# All Counties
mean_hhi_all <- mean(sod_hhi$mean_hhi)
mean_hhi_all_pre <- mean(sod_hhi$mean_hhi_pre)
sod <- sod[, d_mean_all := ifelse(mean_hhi > mean_hhi_all, 1, 0)]
sod <- sod[, d_mean_all_pre := ifelse(mean_hhi > mean_hhi_all_pre, 1, 0)]

# c) Threshold: Market Definition of a highly-concentrated market (HHI > 2500)
# All counties
sod <- sod[, d_marketdef_all := ifelse(mean_hhi > 2500, 1, 0)]

# d) Threshold: 70 percentile 
q70_hhi_all <- quantile(sod_hhi$mean_hhi, probs = 0.70)
sod <- sod[, d_q70_all := ifelse(mean_hhi > q70_hhi_all, 1, 0)]

# Save dataset
SAVE(dfx = sod, name = "SOD_final")


# 2. Federal Funds Rate ========================================================

# Load raw dataset of the federal funds rate
# ffr_data <- LOAD(dfinput = "ffr")
ffr_data <- LOAD(dfinput = "11_thesis_mpt_databasics_us_ffr")
setDT(ffr_data)

# Creating measures of the federal funds rate
# Method 1: Average FFR of each year
ffr_data <- ffr_data[, ffr_mean := mean(ffr), by = year]
ffr_data <- ffr_data[, d_ffr_mean := as.integer(ifelse( ffr_mean < 2, 1,0))]

# Method 2: Last FFR of each year
ffr_data <- ffr_data[, ffr_last := ffr[.N], by = .(year)]
ffr_data <- ffr_data[, d_ffr_last := as.integer(ifelse(ffr_last < 2, 1, 0))]

# Dummy Variable for before and after 2008:
# From the year 2007 to 8 the US experienced a drop in the FFR by 4.08 as the 
# Great Recession unfolded.
ffr_data <- ffr_data[, d_ffr_indicator := as.integer(ifelse(year >= 2008, 1, 0))]

# Reduce dataset to yearly dataset
ffr_data <- unique(ffr_data, by = c("year"))

# Save dataset
SAVE(dfx = ffr_data, namex = "ffr_annual")

# 3. Combine Federal Funds Rate & SOD ==========================================

# Combine SOD dataset and FFR dataset by year
treatment_data <- left_join(sod, ffr_data, by = c("year"))

# 4. Placebo-Test Variables ====================================================

# Create Placebo variables
treatment_data <- treatment_data[, d_placebo_2004 := ifelse(year >= 2004, 1, 0)]
treatment_data <- treatment_data[, d_placebo_2014 := ifelse(year >= 2014, 1, 0)]

# 5. Saving the dataset ========================================================

SAVE(treatment_data, namex = MAINNAME)


########################## ENDE ###############################################+