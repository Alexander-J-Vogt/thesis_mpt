# TARGET: 
# INDATA: 
# OUTDATA/ OUTPUT:  

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

# 01. Import datasets ==========================================================

# Import Outcome Dataset
df_outcome <- LOAD(dfinput = "36_thesis_mpt_eu_varcreation_outcome")

# Import Treatment Dataset
df_treatment <- LOAD(dfinput = "35_thesis_mpt_eu_varcreation_treatment")

# Import control dataset
df_controls <- LOAD(dfinput = "37_thesis_mpt_eu_varcreation_controls")

# 02. Merge Datasets ===========================================================

df_merged <- df_outcome |> 
  full_join(df_treatment, by = c("country", "year")) |> 
  full_join(df_controls, by = c("country", "month"))

# 03. Save =====================================================================

SAVE(dfx = df_merged)

###############################################################################+
################################# ENDE ########################################+
###############################################################################+