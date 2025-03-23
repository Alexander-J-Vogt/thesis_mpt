# TARGET: Import FHFA - House Price Index 
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

# 1. Import FHFA data on House Price Index =====================================

# Import House Price Index data
fhfa <- read_excel(
  path = paste0(A, "u_fhfa/hpi_at_bdl_county.xlsx"),
  sheet = "county",
  col_types = "text",
  skip = 6
)


# 2. Basic Cleaning ============================================================

# Rename Var names
df_fhfa <- fhfa |> 
  rename(
    state = State,
    county = County,
    fips = `FIPS code`,
    year = Year,
    hpi_annual_change_perc = `Annual Change (%)`,
    hpi_1986 = HPI,
    hpi_1990 = `HPI with 1990 base`,
    hpi_2000 = `HPI with 2000 base`
  ) |> 
  mutate(
    year = as.numeric(year),
    hpi_annual_change_perc = if_else(hpi_annual_change_perc == ".", NA_character_, hpi_annual_change_perc),
    hpi_1986 = if_else(hpi_1986 == ".", NA_character_, hpi_1986),
    hpi_1990 = if_else(hpi_1990 == ".", NA_character_, hpi_1990),
    hpi_2000 = if_else(hpi_2000 == ".", NA_character_, hpi_2000)
  ) |> 
  mutate(
    hpi_annual_change_perc = as.numeric(hpi_annual_change_perc),
    hpi_1986 = as.numeric(hpi_1986),
    hpi_1990 = as.numeric(hpi_1990),
    hpi_2000 = as.numeric(hpi_2000)
  ) |> 
  filter(inrange(year, 2004, 2023))

# Extrapolate values for Var
df_fhfa <- df_fhfa |> 
  group_by(fips) |>  # Correct grouping function
  mutate(
    hpi_annual_change_perc = na.approx(hpi_annual_change_perc, year, rule = 2, na.rm = FALSE),
    hpi_1986 = na.approx(hpi_1986, year, rule = 2, na.rm = FALSE),
    hpi_1990 = na.approx(hpi_1990, year, rule = 2, na.rm = FALSE),
    hpi_2000 = na.approx(hpi_2000, year, rule = 2, na.rm = FALSE)
  ) |> 
  fill(hpi_annual_change_perc, .direction = "down") |>
  fill(hpi_1986, .direction = "down") |>
  fill(hpi_1990, .direction = "down") |>
  fill(hpi_2000, .direction = "down") |>
  ungroup()

# 3. Save ======================================================================

SAVE(dfx = df_fhfa)


################################# END #########################################+
