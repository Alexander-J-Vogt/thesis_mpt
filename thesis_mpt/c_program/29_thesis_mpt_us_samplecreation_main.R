# TARGET: Get dataset with no missing variables
# INDATA: mp_transmission_merge
# OUTDATA/ OUTPUT: mp_transmission_main

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

# 0. Determine Periods of Dataset ==============================================

# Load: Home Purchase for large sample
df_hp_depository_large <- LOAD("28_thesis_mpt_us_varcreation_merge_hp")

# Load: Refinancing for large sample
df_ref_depository_large <- LOAD("28_thesis_mpt_us_varcreation_merge_ref")

# Further Steps are not necessary to implement in order to create the main sample
# as all steps are happening in Script 28.
# Script is included for completeness purposes in the project structure. 

# 1. Saving ====================================================================

# Save: House Purchase - 2004 bis 2023
SAVE(dfx = df_hp_depository_large, namex = paste0(MAINNAME, "_hp_large"))

# Save: Refinancing - 2004 bis 2023
SAVE(dfx = df_ref_depository_large, namex = paste0(MAINNAME, "_ref_large"))

########################## ENDE ###############################################+