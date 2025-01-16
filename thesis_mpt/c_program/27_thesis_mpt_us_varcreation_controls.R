# TARGET: Creating a Main dataset for all control variables from different sources
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

# 1. Summary of Deposits - Control Variables ===================================

# Create control variables based on variables available in the SOD

# Load the Summary of Deposits for the period 1994 to 2020
# sod <- LOAD(dfinput = "banks_sod", dfextension = ".rda")
sod <- LOAD(dfinput = "07_thesis_mpt_databasics_sod", dfextension = ".rda")
setDT(sod)

# Restrict to the relevant variables
sod <- sod[, .(year, fips, msabr, bkmo)]

# Number of Main offices in a county:
# Create dummy whether county is has the main office or not (bkmo)
sod <-  sod[, cnty_main_office := sum(bkmo), by = .(fips, year)]

# Collapse by year and fips 
sod <- unique(sod, by = c("year", "fips"))

# Indicator whether a county lays in a Metropolitan Statistical Area (MSA) or not:
# MSA tells whether an areas has than 50000 inhabitants
# Could account for the fact that a different economic dynamic is present compared
# to non-MSA and the amount of loans handed-out is different compared to a areas
# with less than 50000.
sod <- sod[, d_msa := as.integer(ifelse(msabr > 0, 1, 0))]

# Select relevant variables
sod <-  sod[, c("year", "fips", "cnty_main_office", "d_msa")]

# SAVE
SAVE(dfx = sod, name = "controls_sod")

# clear global environment
rm(list = c("sod"))

# 2. Creating a variable for firmsize

# load dataset
raw_sod <- LOAD(dfinput = "raw_sod")
setDT(raw_sod)

# Market power of commercial banks
top_banks <- raw_sod[, .(depsumbank = sum(depsumcnty)), by = .(year, rssdid)]
top_banks <- top_banks[, tot_marketvalue_yearly  := as.numeric(sum(depsumbank)), by = year]
top_banks <- top_banks[, marketshare_yearly := depsumbank / tot_marketvalue_yearly]

# Get all top 5 banks for each year
top_banks <- top_banks |> 
  group_by(year) |>
  arrange(desc(marketshare_yearly)) |> 
  slice_head(n = 5) |>
  mutate(d_top_bank = 1) |>
  ungroup() |> 
  select(year, rssdid, d_top_bank)

# Merge top 5 banks by rssdid and year + Substitute all missings by 0
raw_sod <- raw_sod |> 
  left_join(top_banks, by = c("year", "rssdid")) |>
  mutate(d_top_bank = ifelse(is.na(d_top_bank), 0, d_top_bank)) 

# Variables is created in order to get an indicator whether a top 5 bank is in the 
# county or not
raw_sod <- raw_sod %>%
  dplyr::group_by(year, fips) %>%
  dplyr::summarize(d_top_bank = as.integer(any(d_top_bank == 1)), .groups = 'drop')


# 2. Creating Control Dataset ==================================================

# Import Population County dataset
# pop_cnty <- LOAD(dfinput = "pop_cnty")
pop_cnty <- LOAD(dfinput = "12_thesis_mpt_databasics_us_pop")

# Import Population State dataset
pop_state <- LOAD(dfinput = "pop_state")

# Import Unemployment Rate
# ur_cnty <- LOAD(dfinput = "ur_cnty")
ur_cnty <- LOAD(dfinput = "10_thesis_mpt_databasics_us_ur")

# Import Average Earnings Data
# qwi_earnings <- LOAD(dfinput = "qwi_earnings")
# qwi_earnings <- LOAD(dfinput = "mp_transmission_databasics_qwi")

# Import Controls from sod
controls_sod <- LOAD(dfinput = "controls_sod")

# Population density
# pop_density <- LOAD(dfinput = "landarea_data")
pop_density <- LOAD(dfinput = "13_thesis_mpt_databasics_us_landarea")

# Median Household Income and Poverty Rate
saipe <- LOAD(dfinput = "16_thesis_mpt_databasics_saipe")

# Combining datasets by county and year
merged_data <- left_join(pop_cnty, controls_sod, by = c("fips", "year"))
# merged_data <- left_join(merged_data, qwi_earnings, by = c("fips", "year"))
merged_data <- left_join(merged_data, ur_cnty, by = c("fips", "year"))
merged_data <- left_join(merged_data, pop_density, by = "fips")
merged_data <- left_join(merged_data, raw_sod, by = c("year", "fips"))
merged_data <- left_join(merged_data, saipe, by = c("year", "fips"))

# 3. Variable Creation =========================================================
# 
# # Creating share of employment in each county and year
# setDT(merged_data)
# merged_data[, emp_rate := mean_emp / cnty_pop]
setDT(merged_data)
# # Calculate population desnity of a county
merged_data <- merged_data[, pop_density := cnty_pop / landarea_sqkm]
# 
# # Creating log emp
# merged_data <- merged_data[, log_emp := log(mean_emp)]
# 
# # Creating log mean_earnings in order to get normally distributed variables
# merged_data[, log_earnings := log(mean_earning)]
DEBUG <- T
# 4. Imputation of Missings ====================================================
if (DEBUG) {
# Basic Imputation of Data
df_imputation <- merged_data |> 
  select(-c("state")) |> 
  mutate(fips = as.numeric(fips)) |> 
  select(-c("cnty_main_office", "landarea_sqm", "landarea_sqkm", "fips", "year" ))

imp <- mice(df_imputation, seed = SEED, print = F)

test <- complete(imp)


test <- merged_data |> 
  mutate(
    ur_imputed = missForest(df_imputation)$ximp$ur
  )


missing <- merged_data |> 
  filter(is.na(ur)) |> 
  arrange(fips) |> 
  mutate(ones = 1) |> 
  group_by(state, fips) |> 
  mutate(na_by_state = sum(ones))
# 4. Saving ====================================================================


missing <- merged_data |> 
  filter(is.na(ur)) 
fips_na <- unique(missing$fips)

test <- merged_data |> 
  filter(fips %in% fips_na)



# save dataset
SAVE(dfx = merged_data)

########################## ENDE ###############################################+