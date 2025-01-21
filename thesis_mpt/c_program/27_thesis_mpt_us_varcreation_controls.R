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

## 1.2 Indicator for MSA & Main Office -----------------------------------------
# Create control variables based on variables available in the SOD

# Load the Summary of Deposits for the period 1994 to 2020
sod <- LOAD(dfinput = "07_thesis_mpt_databasics_sod")
setDT(sod)

# Restrict to the relevant variables
sod <- sod[, .(year, fips, msabr, bkmo, rssdid)]

# Number of Main offices in a county:
# Create dummy whether county is has the main office or not (bkmo)
sod[, tot_nr_banks := .N, by = year]
sod[, `:=` (
  share_main_office_cnty = sum(bkmo) / tot_nr_banks,
  nr_main_office_cnty = sum(bkmo)
  ), by = .(fips, year)]


# Indicator whether a county lays in a Metropolitan Statistical Area (MSA) or not:
# MSA tells whether an areas has than 50000 inhabitants
# Could account for the fact that a different economic dynamic is present compared
# to non-MSA and the amount of loans handed-out is different compared to a areas
# with less than 50000.
sod <- sod[, d_msa := as.integer(ifelse(msabr > 0, 1, 0))]

# Collapse by year and fips 
sod <- distinct(sod, year, fips, msabr, share_main_office_cnty, nr_main_office_cnty, d_msa)

# SAVE
SAVE(dfx = sod, name = "sod_msa_main_office")

# clear global environment
rm(list = c("sod"))


## 1.2 Indicator for Firm Size -------------------------------------------------

# load dataset
raw_sod <- LOAD(dfinput = "07_thesis_mpt_databasics_sod")
setDT(raw_sod)

# Market Power of Depository Institutions 
# top_banks <- raw_sod[insured == "CB"]
top_banks <- raw_sod[, .(depsumbank = sum(depsumbr)), by = .(year, rssdid)]
top_banks <- top_banks[, tot_marketvalue_yearly  := as.numeric(sum(depsumbank)), by = year]
top_banks <- top_banks[, marketshare_yearly := depsumbank / tot_marketvalue_yearly]

# Get all top 5 banks for each year
top_banks <- top_banks |> 
  group_by(year) |>
  arrange(desc(marketshare_yearly)) |>
  arrange(year) |> 
  slice_max(marketshare_yearly, n = 5) |>
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

# Save
SAVE(dfx = raw_sod, namex= "sod_top5_banks")


# 2. Creating Control Dataset ==================================================

# Create a Master Control Dataset

# Import Population County dataset
pop_cnty <- LOAD(dfinput = "12_thesis_mpt_databasics_us_pop")


# Import Population State dataset
pop_state <- LOAD(dfinput = "pop_state")

# Import Unemployment Rate
bls_ur <- LOAD(dfinput = "10_thesis_mpt_databasics_us_ur")
bls_ur$year <- as.numeric(bls_ur$year)

# Import Controls from sod
sod_msa_main_office <- LOAD(dfinput = "sod_msa_main_office")
sod_top5_banks <- LOAD(dfinput = "sod_top5_banks")

# Population density
gazette <- LOAD(dfinput = "13_thesis_mpt_databasics_us_landarea")

# Median Household Income and Poverty Rate
saipe <- LOAD(dfinput = "16_thesis_mpt_databasics_saipe")

# Accounting for Changes in FIPS Codes

# Link New Fips Codes for CT to Old Ones for 2020+
df_crosswalk_ct <- LOAD(dfinput = "22_thesis_mpt_databasics_ct_crosswalk_file")

pop_cnty <- pop_cnty |> 
  left_join(df_crosswalk_ct, by = "fips", , relationship = "many-to-many") |> 
  mutate(fips = if_else(!is.na(fips_old), fips_old, fips)) |> 
  select(-fips_old)

pop_cnty[str_sub(fips %in% df_crosswalk_ct$fips_old)]

# Manually Change the FIPS code for Oglala Lakota for the Period before 2010 from
# 46113 to 46102 (46102 is the uptodata code and was changed due to a county name change 
# from Shannon County to Oglala Lakota County)
datasets_list <- c("pop_cnty", "bls_ur", "sod_msa_main_office", "sod_top5_banks", "gazette", "saipe")

for (i in datasets_list) {
  
  # Retrieve object
  data <- get(i)
  setDT(data)
  
  # Change FIPS code
  data[,  fips := if_else(fips == "46113", "46102", fips)]
  data <- data[fips != "51515"] # Drop Bredford City, VA
  data <- data[fips != "15005"] # Drop Kalawao County, HI
  
  # Assign to original object name
  assign(i, data)
  
}

# Combining datasets by county and year
data_merged <- left_join(pop_cnty, sod_msa_main_office, by = c("fips", "year"))
data_merged <- left_join(data_merged, sod_top5_banks, by = c("fips", "year"))
data_merged <- left_join(data_merged, bls_ur, by = c("fips", "year"))
data_merged <- left_join(data_merged, saipe, by = c("fips", "year"))
data_merged <- left_join(data_merged, gazette, by = c("fips", "year"))



# 3. Variable Creation =========================================================

setDT(data_merged)

# Filter for the year 2004 to 2023
data_merged <- data_merged[inrange(year, 2004, 2023)]

##  Dealing with Missings ---

# For SOD data: If no information in SOD than there wont be any missings information but 
# there is no branch in this county
data_merged[, c("share_main_office_cnty", "nr_main_office_cnty", "d_top_bank") := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), 
            .SDcols = c("share_main_office_cnty", "nr_main_office_cnty", "d_top_bank")]

# Fill landarea data
data_merged <- data_merged[order(fips, year)] # Ensure data is sorted
landarea_vars <- c("landarea_sqm", "landarea_smi", "landarea_sqkm", "landarea_sqkm")
data_merged[, (landarea_vars) := nafill(.SD, type = "nocb"), .SDcols = landarea_vars, by = fips] # Backward fill within each county

# Calculate population density of a county
data_merged[, pop_density := cnty_pop / landarea_sqkm]

# Imputate missings for Alaska: 02105, 02195, 02198, 02230, 02270, 02275
# Impute by using mean
data_merged[, ur := ifelse(is.na(ur), mean(ur, na.rm = T), ur)]
data_merged[, median_household_income := ifelse(is.na(median_household_income), mean(median_household_income, na.rm = T), median_household_income)]
data_merged[, poverty_percent_all_ages := ifelse(is.na(poverty_percent_all_ages), mean(poverty_percent_all_ages, na.rm = T), poverty_percent_all_ages)]


data_sum <- data_merged |> 
  filter(fips %in% c("02105", "02195", "02198", "02230", "02270", "02275")) 
  group_by(fips) |> 
  reframe(
    mean_ur = mean(ur, na.rm = TRUE),
    mean_income = mean(median_household_income, na.rm = TRUE),
    mean_pov = mean(poverty_percent_all_ages, na.rm = TRUE)
  )

data_sum_imp <- data_merged |> 
  filter(fips %in% c("02105", "02195", "02198", "02230", "02270", "02275")) |> 
  group_by(fips) |> 
  mutate(
    ur = ifelse(is.na(ur), mean(ur, na.rm = T), ur),
    median_household_income = ifelse(is.na(median_household_income), mean(median_household_income, na.rm = T), median_household_income),
    poverty_percent_all_ages = ifelse(is.na(poverty_percent_all_ages), mean(poverty_percent_all_ages, na.rm = T), poverty_percent_all_ages)
  ) |> 
  reframe(
    mean_ur = mean(ur, na.rm = TRUE),
    mean_income = mean(median_household_income, na.rm = TRUE),
    mean_pov = mean(poverty_percent_all_ages, na.rm = TRUE)
  )

data_merged |> filter(fips %in% c("02105", "02195", "02198", "02230", "02270", "02275"))
data_merged |> filter(is.na(ur))
pov_cnty <- data_merged |> filter(is.na(poverty_percent_all_ages))
data_merged |> filter(is.na(median_household_income))

imputed_data <- kNN(data_merged, k = 5, variable = c("ur"))



aggr(data_merged, col = c("skyblue", "red"), numbers = TRUE)
marginplot(data_merged[, c("fips", "ur")])
marginplot(data_merged[, c("fips", "poverty_percent_all_ages")])
marginplot(data_merged[, c("fips", "median_household_income")])
marginplot(data_merged[, c("fips", "ur")])




test <- data_merged |> 
  filter(is.na(ur))

fips_in <- unique(test$fips)

bls_ur |> 
  filter(fips %in% fips_in) |> arrange( fips, year)

test_landarea <- data_merged |> filter(is.na(landarea_sqm)) |> filter(year > 2019)
unique_landare <- unique(test_landarea$fips)

test <- distinct(data_merged, fips, msabr)
test |> 
  filter(is.na(msabr)) |> 
  print()

data_merged |> 
  filter(is.na(landarea_sqm))

counties <- data_merged |> 
  filter(is.na(landarea_sqm))

cnty <- unique(counties$fips)

data_merged |> 
  filter(fips %in% cnty)

DEBUG <- F
# 4. Imputation of Missings ====================================================
if (DEBUG) {
# Basic Imputation of Data
df_imputation <- data_merged |> 
  select(-c("state")) |> 
  mutate(fips = as.numeric(fips)) |> 
  select(-c("cnty_main_office", "landarea_sqm", "landarea_sqkm", "fips", "year" ))

imp <- mice(df_imputation, seed = SEED, print = F)

test <- complete(imp)


test <- data_merged |> 
  mutate(
    ur_imputed = missForest(df_imputation)$ximp$ur
  )


missing <- data_merged |> 
  filter(is.na(ur)) |> 
  arrange(fips) |> 
  mutate(ones = 1) |> 
  group_by(state, fips) |> 
  mutate(na_by_state = sum(ones))
}
# 4. Saving ====================================================================


missing <- data_merged |> 
  filter(is.na(ur)) 
fips_na <- unique(missing$fips)

test <- data_merged |> 
  filter(fips %in% fips_na)



# save dataset
SAVE(dfx = merged_data)

########################## ENDE ###############################################+