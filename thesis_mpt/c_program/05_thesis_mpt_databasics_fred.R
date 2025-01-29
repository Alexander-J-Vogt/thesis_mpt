# TARGET: FRED data on CPI and GDP Growth
# INDATA:
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


# 1. Import nation CPI data =====================================================

# Import 
inflation <- read_csv(
  file = paste0(A, "h_fred/FPCPITOTLZGUSA.csv"),
  col_types = "c"
)

# Basic transformation: Rename, Format
df_inflation <- inflation |> 
  rename(
    year = DATE,
    inflation_us = FPCPITOTLZGUSA
  ) |> 
  mutate(
    inflation_us = as.numeric(inflation_us),
    year = as.numeric(str_sub(year, 1, 4))
  )

# 2. Import national GDP =======================================================

# Import
gdp_pc <- read_csv(
  file = paste0(A, "h_fred/GDP_PCH.csv"),
  col_types = "c"
)

# Basic transformation: Rename, Format
df_gdp_pc <- gdp_pc |> 
  rename(
    year = observation_date,
    gdp_growth_us = GDP_PCH
  ) |> 
  mutate(
    gdp_growth_us = as.numeric(gdp_growth_us),
    year = as.numeric(str_sub(year, 1, 4))
  )
  
  
# 3. Combine to Dataset ========================================================

# Merge
df_fred <- df_inflation |> 
  left_join(df_gdp_pc, by = "year")

# 4. Save ======================================================================

SAVE(dfx = df_fred)


############################## END ############################################+