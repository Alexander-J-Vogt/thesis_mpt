# TARGET: Import GDP United States 
# INDATA: qwi_e14db0de913c427aa12de971a73eb389.csv
# OUTDATA/ OUTPUT: mp_transmission_databasics_qwi

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

# 1. GDP of the U.S. ===========================================================

# Import data on GDP
gdp_data <- read.csv(paste0(A, "h_fred/", "GDP.csv"))
setDT(gdp_data)

# cahnge names 
setnames(gdp_data, old = c("DATE", "GDP"), new = c("year", "gdp"))

# format variables
gdp_data <- gdp_data[, year := year(year)]
gdp_data <- gdp_data[, gdp := as.numeric(gdp)]

# create lag and growth variable
gdp_data <- gdp_data[, gdp_lag := shift(gdp, type = "lag")]
gdp_data <- gdp_data[, gdp_growth := (gdp - gdp_lag) / gdp_lag * 100]

# Save 
SAVE(dfx = gdp_data, namex = MAINNAME)

################################### END #######################################+