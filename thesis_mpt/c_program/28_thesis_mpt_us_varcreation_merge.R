# TARGET: Merging Outcome, Treatment and Control Dataset
# INDATA:  hmda_banks, hmda_all, mp_transmission_treatment, mp_transmission_control
# OUTDATA/ OUTPUT: merged_banks_data, merged_allfin_data

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

# Import later needed dataset besides the outcome, treatment and control dataset

# Load CT Crosswalk file
df_crosswalk_ct <- LOAD("22_thesis_mpt_databasics_ct_crosswalk_file")
df_pop <- LOAD(dfinput = "12_thesis_mpt_databasics_us_pop")

# Determine all 51 States of Interst
STATE <- c(
  "01", "02", "04", "05", "06", "08", "09", "10", "12", "13",
  "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
  "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
  "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
  "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "11"
)

# 1. Merging the dataset from outcome, treatment and control script ============

# Loading Mortgage Datasets: All four variants ---
vector_outcomes <- list.files(path = TEMP)
vector_outcomes <- vector_outcomes[grepl("25", vector_outcomes) & grepl("(hp|ref)", vector_outcomes)]
vector_outcomes <- str_replace_all(vector_outcomes, ".rda", "")

purrr::walk(vector_outcomes, function(dataset) {
  
  # dataset <- vector_outcomes[1]
  # Import data
  data <- LOAD(dfinput = dataset)
  
  # Assign name to dataset
  name <- str_replace_all(dataset, "25_thesis_mpt_us_varceation_outcome_", "") 
  assign(
    paste0("df_", name),
    data,
    envir = .GlobalEnv
  )
  
  return(data)
  
})

all_outcome <- ls(envir = .GlobalEnv, pattern = "df")
all_outcome <- all_outcome[str_detect(all_outcome, pattern = "(ref|hp)")]

## Outcome Data ---

# Adjusting code for changes in county

purrr::walk(all_outcome, function(i) {
  # i <- all_outcome[1]
  # Retrieve data object
  data <- get(i)
  setDT(data)
  
  # Change FIPS code
  data[,  fips := if_else(fips == "46113", "46102", fips)]
  data <- data[fips != "51515"] # Drop Bredford City, VA
  data <- data[fips != "15005"] # Drop Kalawao County, HI
  
  # Save in global environment
  assign(i, data, envir = .GlobalEnv)
  
})

# Depository Inst. - Home Purchase
df_hp_depository <- df_hp_depsitory
setDT(df_hp_depository)

# Depositiry Inst. - Refinancing
df_ref_depository <- df_ref_depository


## Loading treatment data from Treatment Script --- 

# (important as this contains counties, which are observed over all time periods in the SOD)
data_treatment <- LOAD(dfinput = "26_thesis_mpt_us_varcreation_treatment")
setDT(data_treatment)

# Adjusting code for changes in county 

# Load CT Crosswalk file
df_crosswalk_ct <- LOAD("22_thesis_mpt_databasics_ct_crosswalk_file")

# Change FIPS code
data_treatment[,  fips := if_else(fips == "46113", "46102", fips)]
data_treatment <- data_treatment[fips != "51515"] # Drop Bredford City, VA
data_treatment <- data_treatment[fips != "15005"] # Drop Kalawao County, HI

# Adjust CT from New Counties to Old Counties for 2021+
data_treatment <- data_treatment |> 
  left_join(df_crosswalk_ct, by = "fips", , relationship = "many-to-many") |>
  mutate(fips = if_else(!is.na(fips_old), fips_old, fips)) |>
  dplyr::select(-fips_old) |> 
  group_by(fips, year) |> 
  # Adjust for many-to-many merge - some counties are observed double after
  # the linking new with old counties - not the most cleanest approach but the 
  # the best possible w/o excluding the whole state of Connecticut 
  mutate(
    hhi               = mean(hhi),
    ffr               = round(mean(ffr , na.rm = TRUE), 0),
    d_ffr_mean_1perc  = round(mean(d_ffr_mean_1perc , na.rm = TRUE), 0),
    d_ffr_mean_2perc  = round(mean(d_ffr_mean_2perc , na.rm = TRUE), 0),
    d_ffr_last_1perc  = round(mean(d_ffr_last_1perc  , na.rm = TRUE), 0),
    d_ffr_last_2perc  = round(mean(d_ffr_last_2perc  , na.rm = TRUE), 0),
    d_great_recession = round(mean(d_great_recession, na.rm = TRUE), 0),
  ) |> 
  ungroup() |> 
  distinct(fips, year, .keep_all = TRUE)

# Loading control data from the control scripts ---
data_controls <- LOAD(dfinput = "27_thesis_mpt_us_varcreation_controls")
setDT(data_controls)
data_controls <- data_controls[, state := NULL]


# 2. Merge Outcome, Treatment and Controls =====================================

# Perform Full Join between Mortgage Data, treatment (SOD + FFR) & control data if only 
# mortgages of commercial banks are observed

purrr::walk(all_outcome, ~ {
  
  data <- get(.x)
    
  data_merge <- full_join(data, data_treatment, by = c("fips", "year"))
  data_merge <- full_join(data_merge, data_controls, by = c("fips", "year"))
  setDT(data_merge)
  data_merge <- data_merge[inrange(year, 2004, 2023)]
  
  assign(
    paste0(.x, "_full"),
    data_merge,
    envir = .GlobalEnv
    )
  
})

# 3. House Purchase ============================================================

# Create a balanced dataset for all home purchases for 3001 counties for the period
# between 2004 to 2023

# Determine df as dt and filter for relevant time period
df_hp_depository_full <- df_hp_depsitory_full
setDT(df_hp_depository_full)
df_hp_depository_full <- df_hp_depository_full[inrange(year, 2004, 2023)]

# Determine number of years
year_length <- length(unique(df_hp_depository$year))

# Determine the Counties with missing observation
df_hp_balanced <- df_hp_depository_full |> 
  mutate(id_available = 1) |> 
  dplyr::select(fips, year, id_available) |> 
  pivot_wider(
    id_cols = "fips",
    names_from = "year",
    values_from = "id_available",
    names_prefix = "year_"
  ) |> 
  mutate(nr_year = rowSums(across(starts_with("year_")), na.rm = TRUE))

# Determine Counties which are not observed over the whole period
df_counties_missing <- df_hp_balanced |> 
  filter(nr_year != year_length) |> 
  pivot_longer(
    cols = starts_with("year"),
    names_to = "year",
    values_to = "years_available",
    names_prefix = "year_"
  ) |> 
  dplyr::select(fips, year) |> 
  mutate(year = as.numeric(year))

unique_fips <- unique(df_counties_missing$fips)

# Determine Missing Counties in Outcome after Merge
df_outcome_unique_fips <- df_hp_depository_full |> 
  filter(is.na(loan_amount))
outcome_unique_fips <- unique(df_outcome_unique_fips$fips)

# Determine Missing Counties in Treatment after Merge
df_treatment_unique_fips <- df_hp_depository_full |> 
  filter(is.na(hhi))
treatment_unique_fips <- unique(df_treatment_unique_fips$fips)

# Determine combined unique missing counties (in at least on period)
fips_combined <- c(treatment_unique_fips, outcome_unique_fips)
unique_fips_combined <- unique(fips_combined)

# Document missing Counties with corresponding population
df_document_fips <- df_pop |> 
  filter(fips %in% unique_fips_combined) |> 
  filter(fips %in% unique_fips)

# Save as .csv
write.csv(df_document_fips, file = paste0(FIGURE, "hp_excluded_counties.csv"))

# Document missing countues by state
df_document_state <- df_document_fips |> 
  group_by(state) |> 
  summarise(
    mean_pop = mean(cnty_pop),
    nr_fips = n_distinct(fips)
  )

# Filter counties as they are small and thus in some years do not experience mortgage
df_hp_depository_panel <- df_hp_depository_full |> 
  filter(!fips %in% unique_fips_combined) |> # Filter for partly missing observation of counties in outcome and treatment variables
  filter(!fips %in% unique_fips) |> # Filter for counties which are either missing, not observed over all periods (because they have small population or there is a change in county code) or do not exist 
  mutate(state = str_sub(fips, 1, 2), .after = "year") |> 
  filter(state %in% STATE) |> 
  arrange(fips, year) |> 
  select(-ends_with("applicant"))

# Check if panel is balanced
is.pbalanced(df_hp_depository_panel)

# 4. Refinancing ===============================================================

# Determine df as dt
setDT(df_ref_depository_full)

# Filter for time range 2004 to 2023
df_ref_depository_full <- df_ref_depository_full[inrange(year, 2004, 2023)]

year_length <- length(unique(df_ref_depository_full$year))

# Determine the Counties with missing observation
df_ref_balanced <- df_ref_depository_full |> 
  mutate(id_available = 1) |> 
  dplyr::select(fips, year, id_available) |> 
  pivot_wider(
    id_cols = "fips",
    names_from = "year",
    values_from = "id_available",
    names_prefix = "year_"
  ) |> 
  mutate(nr_year = rowSums(across(starts_with("year_")), na.rm = TRUE))

# Determine Counties which are not observed over the whole period
df_ref_counties_missing <- df_ref_balanced |> 
  filter(nr_year != year_length) |> 
  pivot_longer(
    cols = starts_with("year"),
    names_to = "year",
    values_to = "years_available",
    names_prefix = "year_"
  ) |> 
  dplyr::select(fips, year) |> 
  mutate(year = as.numeric(year))

unique_ref_fips <- unique(df_ref_counties_missing$fips)

# Determine Missing Counties in Outcome & Treatement after Merge
df_outcome_unique_fips <- df_ref_depository_full |> 
  filter(is.na(loan_amount))
outcome_unique_fips <- unique(df_outcome_unique_fips$fips)

df_treatment_unique_fips <- df_hp_depository_full |> 
  filter(is.na(hhi))
treatment_unique_fips <- unique(df_treatment_unique_fips$fips)

# Determine combined unique missing counties (in at least on period)
fips_combined <- c(treatment_unique_fips, outcome_unique_fips)
unique_fips_combined <- unique(fips_combined)

# Document missing Counties with corresponding population
df_ref_document_fips <- df_pop |> 
  filter(fips %in% unique_fips_combined)

# Save as .csv
write.csv(df_ref_document_fips, file = paste0(FIGURE, "ref_excluded_counties.csv"))

# Document missing countues by state
df_document_state <- df_ref_document_fips |> 
  group_by(state) |> 
  summarise(
    mean_pop = mean(cnty_pop),
    nr_fips = n_distinct(fips)
  )

# Filter counties as they are small and thus in some years do not experience mortgage
df_ref_depository_panel <- df_ref_depository_full |> 
  filter(!fips %in% unique_fips_combined) |> # Filter for partly missing observation of counties in outcome and treatment variables
  filter(!fips %in% unique_ref_fips) |> # Filter for counties which are either missing, not observed over all periods (because they have small population or there is a change in county code) or do not exist 
  mutate(state = str_sub(fips, 1, 2), .after = "year") |> 
  filter(state %in% STATE) |> 
  select(-ends_with("applicant"))

# Check if panel is balanced
is.pbalanced(df_ref_depository_panel)








# 
# library(VIM)
# marginplot(df_hp_depository_panel[, c("fips", "log_loan_amount")])
# marginplot(dt_50000[, c("fips", "log_loan_amount")])
# 
# # 3. Exclude non-relevant variables ============================================
# 
# # Deselect variables
# data_merge <- data_merge[, c("date", "landarea_sqm", "ffr") := NULL]
# data_merge <- data_merge[state %in% c(
#   "01", "02", "04", "05", "06", "08", "09", "10", "12", "13",
#   "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
#   "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
#   "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
#   "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "11"
# )]
# data_merge <- data_merge[inrange(year, 2004, 2023)]
# 
# 
# library(VIM)
# marginplot(data_merge[, c("fips", "total_amount_loan")])
# marginplot(data_merge[, c("fips", "poverty_percent_all_ages")])



# 4. Saving ====================================================================

# Save: House Purchase - 2004 bis 2023
SAVE(dfx = df_hp_depository_panel, namex = paste0(MAINNAME, "_hp"))

# Save: Refinancing - 2004 bis 2023
SAVE(dfx = df_ref_depository_panel, namex = paste0(MAINNAME, "_ref"))
# 
# # Save: House Purchase - 2018 bis 2023
# SAVE(dfx = df_hp_depositor0y_reform_panel, namex = paste0(MAINNAME, "_hp_small"))
# 
# # Save: Refinancing - 2018 bis 2023
# SAVE(dfx = df_ref_depository_reform_panel, namex = paste0(MAINNAME, "_ref_small"))


########################## ENDE ###############################################+
