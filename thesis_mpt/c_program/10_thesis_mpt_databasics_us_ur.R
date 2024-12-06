# TARGET: Import Unemployment Rate data
# INDATA: Unemployment.xlsx
# OUTDATA/ OUTPUT: mp_transmission_databasics_ur

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

# 1. Import Unemployment Data ==================================================

# Import unemployment data on county level from the economic research service
unemp_data <- read_xlsx(paste0(A, "i_economic_research_service/", "Unemployment.xlsx"), skip = 4, col_types = "text")
setDT(unemp_data)

# Extract Unemployment Rates for all counties
unemp_data <- EXTRACTVAR(data = unemp_data, state_col = "State", county_col = "FIPS_Code", indicator = "Unemployment_rate")

# Standardize variable names
colnames(unemp_data) <- str_to_lower(names(unemp_data))
setnames(unemp_data, old = c("fips_code"), new = c("fips"))

# Shift data from wide to long format
unemp_data <- melt(
  unemp_data,
  measure.vars = patterns("unemployment_rate_"),
  variable.name = "year",
  value.name = "ur"
)

# Clean the year variable
unemp_data <- unemp_data[, year := sub("unemployment_rate_", "", year)]

# Delete observation for states and US Totals
unemp_data <- unemp_data[fips != "00000"]
unemp_data <- unemp_data[
  substr(get("fips"), nchar(get("fips")) - 2, nchar(get("fips"))) != "000", 
  .(fips, state, year, ur)
]

# format variables
unemp_data <- unemp_data[, year := as.integer(year)]
unemp_data <- unemp_data[, ur := round(as.double(ur), 2)]

# Select relevant variable
unemp_data <- unemp_data[, c("fips", "year", "ur")]

# Save
SAVE(dfx = unemp_data, namex = MAINNAME)

############################## END ############################################+