# TARGET: Import FIPS code
# INDATA: US_FIPS_Codes.xls
# OUTDATA/ OUTPUT: mp_transmission_databasics_fips

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

# 1. Import US FIPS County Codes ===============================================

# FIPS Code is the unique identifier of a county and consist of a 5-digit number.
# The first two digits is the state code, while the last three digits are county-code.

# This data is used to verify fips code in the Summary of Deposits
# SOD contians fips code that do not exist.
fips_data <- read_xls(paste0(A, "g_census/", "US_FIPS_Codes.xls"), skip = 1)
setDT(fips_data)

# Rename columns
colnames(fips_data) <-  c("state_name", "county_name", "state_code", "county_code")

# Creates fips code by combining state and county code
fips_data <- fips_data[, fips := paste0(state_code, county_code)]

# Save dataset
SAVE(dfx = fips_data, namex = MAINNAME)

############################ END ##############################################+
