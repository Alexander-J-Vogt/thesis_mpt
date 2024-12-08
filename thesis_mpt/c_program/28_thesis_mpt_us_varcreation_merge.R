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

# 1. Merging the dataset from outcome, treatment and control script ============

# Loading Mortgage Data from Outcome Script
# Dataset that only includes Commercial Banks and their Mortgage Subdivisions
# that lended mortgages
outcome_banks_data <- LOAD(dfinput = "25_thesis_mpt_us_varceation_outcome")
setDT(outcome_banks_data)

# Loading treatment data from Treatment Script
# (important as this contains counties, which are observed over all time periods in the SOD)
treatment_data <- LOAD(dfinput = "26_thesis_mpt_us_varcreation_treatment")
setDT(treatment_data)

# Loading control data from the control scripts
controls_data <- LOAD(dfinput = "27_thesis_mpt_us_varcreation_controls")
setDT(controls_data)
controls_data <- controls_data[, state := NULL]

# 2. Commercial Banks ==========================================================

# Perform Full Join between Mortgage Data, treatment (SOD + FFR) & control data if only 
# mortgages of commercial banks are observed
banks_data <- full_join(outcome_banks_data, treatment_data, by = c("fips", "year"))
banks_data <- full_join(banks_data, controls_data, by = c("fips", "year"))

# 3. Exclude non-relevant variables ============================================

# Deselect variables
banks_data <- banks_data[, c("date", "landarea_sqm", "ffr") := NULL]

# 4. Saving ====================================================================

# Save
SAVE(dfx = banks_data, namex = MAINNAME)


########################## ENDE ###############################################+
