# TARGET: Import Landarea of each county
# INDATA: Gaz_counties_national.txt
# OUTDATA/ OUTPUT: mp_transmission_databasics_gaz

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

# 1. Land Area of Counties =====================================================

# Import data
landarea <- read_delim(paste0(A, "j_us_census_bureau/", "Gaz_counties_national.txt"), col_types = cols(.default = "c"))
setDT(landarea)

# Select columns
landarea <- landarea[, c("GEOID", "ALAND")]

# Rename var
setnames(landarea, old = c("GEOID", "ALAND"), new = c("fips",  "landarea_sqm"))

# Format variables
landarea <- landarea[, landarea_sqm := as.numeric(landarea_sqm)]

# Calculate land are per km
landarea <- landarea[, landarea_sqkm := landarea_sqm / 1000000]


# 2. Save ======================================================================

SAVE(dfx = landarea, namex = MAINNAME)


############################ ENDE #############################################+