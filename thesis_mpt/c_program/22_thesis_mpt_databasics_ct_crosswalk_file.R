# TARGET: Crosswalk Files for the Conneticute - Signifcant Changes in the FIPS Borders
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

# 1. Import Crosswalk file for Conneticut ======================================

# Import 
df_crosswalk <- read_excel(path = paste0(A, "s_crosswalk_files/ct_cou_to_cousub_crosswalk.xlsx"))

# Clean col names
col_names <- colnames(df_crosswalk)
col_names <- str_replace_all(col_names, "\r\n", "_")
col_names <- str_replace_all(col_names, "\\(", "")
col_names <- str_replace_all(col_names, "\\)", "")
colnames(df_crosswalk) <- col_names

# Select new and old FIPS
df_crosswalk <- df_crosswalk |> 
  filter(!is.na(OLD_COUNTYFP_INCITS31)) |> 
  select(1:2,4) |> 
  mutate(
    fips_old = paste0(STATEFP_INCITS38, OLD_COUNTYFP_INCITS31),
    fips = paste0(STATEFP_INCITS38, NEW_COUNTYFP_INCITS31)
  ) |> 
  select(4, 5) |> 
  distinct(fips_old, fips)

# SAVE
SAVE(dfx = df_crosswalk)

######################################## END ###################################