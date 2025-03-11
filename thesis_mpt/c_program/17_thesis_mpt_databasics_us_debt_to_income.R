# TARGET: Import Debt to Income Ratio
# INDATA: Debt-to-Income Rate 
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
#### MAIN ####


# 01. Import Data ==============================================================

# Import 
dti <- fread(
  file = paste0(A, "/h_fred/household-debt-by-county.csv"),
  colClasses = "character"
)

# 02. Data Wrangling ===========================================================
df_dti <- dti |> 
  mutate(
    quarter = paste0(year, "-Q",qtr),
    low = as.numeric(low),
    high = as.numeric(high)
  ) |> 
  # format time variable
  mutate(
    quarter = yq(quarter)
  ) |> 
  # calculate mean of debt-to-income ratio via low and high DTI
  mutate(
    dti = ((high - low) / 2) + low
  ) |> 
  # Rename Variables + Select Relevant Variable
  dplyr::select(year, qtr, quarter, area_fips, dti) |> 
  rename(fips = area_fips) |> 
  # Take the Average over all quarter to get annual values
  group_by(year, fips) |> 
  summarise(
    dti = mean(dti)
  ) |> 
  mutate(year = as.numeric(year))


# 03. Save =====================================================================

SAVE(dfx = df_dti, namex = MAINNAME)



##################################### END ######################################