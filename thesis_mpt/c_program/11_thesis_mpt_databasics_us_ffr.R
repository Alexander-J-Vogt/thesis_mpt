# TARGET: Importing Federal Funds Rate Data
# INDATA: FEDFUNDS.csv
# OUTDATA/ OUTPUT: mp_transmission_databasics_ffr.rda

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


# 1. Federal Funds Rate ========================================================

# Import raw monthly data on the Federal Funds Rate
ffr_data <- read.csv(paste0(A, "h_fred/", "FEDFUNDS.csv"))
setDT(ffr_data)

# Rename columns
setnames(ffr_data, old = c("date", "ffr"))

# Create year variable
ffr_data <- ffr_data[, year := as.integer(substr(date, 1, 4))]

# Format date variable
ffr_data <- ffr_data[, date := as.Date(date)]


# 2. Save ======================================================================

SAVE(dfx = ffr_data, name = MAINNAME)


################################ END ##########################################+