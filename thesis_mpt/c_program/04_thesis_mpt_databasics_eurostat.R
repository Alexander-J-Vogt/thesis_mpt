# TARGET: Download Call Reports from FFIEC & Perform Basic Data Cleaning
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

# 01. HICP - Monthly ===========================================================

# Import data (Index = 2010)
df_hicp_raw <- read_excel(
  path = paste0(A, "c_eurostat/prc_hicp_midx__custom_14042520_page_spreadsheet.xlsx"),
  sheet = "Sheet 1",
  range = cell_rows(9:40),
  col_types = "text"
  )

# Filter empty columns
df_hicp <- df_hicp_raw |> 
  select(-c(starts_with("..."))) |>
  rename_with(~ paste0("month_", .), -1) |> 
  filter(TIME %in% c("Austria",
                     "Belgium",
                     "Finland",
                     "France",
                     "Germany",
                     "Greece",
                     "Ireland",
                     "Italy",
                     "Netherlands",
                     "Portugal",
                     "Spain",
                     "Slovenia",
                     "Slovakia")) |> 
  # Reshape into long format
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "hicp"
  ) |> 
  # Format variables + Standardize country variable
  mutate(
    month = as.Date(paste0(str_sub(month, start = 7), "-01")),
    hicp = as.numeric(hicp),
    country = case_when(
      TIME == "Austria" ~ "AT",
      TIME == "Belgium" ~ "BE",
      TIME == "Finland" ~ "FI",
      TIME == "France" ~ "FR",
      TIME == "Germany" ~ "DE",
      TIME == "Greece" ~ "GR",
      TIME == "Ireland" ~ "IE",
      TIME == "Italy" ~ "IT",
      TIME == "Netherlands" ~ "NL",
      TIME == "Portugal" ~ "PT",
      TIME == "Spain" ~ "ES",
      TIME == "Slovenia" ~ "SI",
      TIME == "Slovakia" ~ "SK"
    )
  ) |> 
  select(country, month, hicp)

# 02. Effective Exchange Rate Index - Monthly ==================================

# Import data (Index = 2015)
df_reer_raw <- read_excel(
  path = paste0(A, "c_eurostat/ert_eff_ic_m_spreadsheet.xlsx"),
  sheet = "Sheet 6",
  range = cell_rows(9:55),
  col_types = "text"
)

# Basic Cleaning of the data
df_reer <- df_reer_raw |> 
  select(-c(starts_with("..."))) |>
  rename_with(~ paste0("month_", .), -1) |> 
  filter(TIME %in% c("Austria",
                     "Belgium",
                     "Finland",
                     "France",
                     "Germany",
                     "Greece",
                     "Ireland",
                     "Italy",
                     "Netherlands",
                     "Portugal",
                     "Spain",
                     "Slovenia",
                     "Slovakia")) |> 
  # Reshape into long format
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "reer"
  ) |> 
  # Format variables + Standardize country variable
  mutate(
    month = as.Date(paste0(str_sub(month, start = 7), "-01")),
    reer = as.numeric(reer),
    country = case_when(
      TIME == "Austria" ~ "AT",
      TIME == "Belgium" ~ "BE",
      TIME == "Finland" ~ "FI",
      TIME == "France" ~ "FR",
      TIME == "Germany" ~ "DE",
      TIME == "Greece" ~ "GR",
      TIME == "Ireland" ~ "IE",
      TIME == "Italy" ~ "IT",
      TIME == "Netherlands" ~ "NL",
      TIME == "Portugal" ~ "PT",
      TIME == "Spain" ~ "ES",
      TIME == "Slovenia" ~ "SI",
      TIME == "Slovakia" ~ "SK"
    )
  ) |> 
  select(country, month, reer)


# 03. GDP - Quarterly ==========================================================

# # Import data (Index = 2015)
# df_gdp_raw <- read_excel(
#   path = paste0(A, "c_eurostat/namq_10_gdp__custom_14043017_spreadsheet.xlsx"),
#   sheet = "Sheet 1",
#   range = cell_rows(10:54),
#   col_types = "text"
# )
# 
# # Basic Cleaning of the data
# df_gdp <- df_gdp_raw |>
#   # Select the right time and country
#   select(-c(starts_with("..."))) |>
#   rename_with(~ paste0("quarter_", .), -1) |> 
#   filter(TIME %in% c("Austria",
#                      "Belgium",
#                      "Finland",
#                      "France",
#                      "Germany",
#                      "Greece",
#                      "Ireland",
#                      "Italy",
#                      "Netherlands",
#                      "Portugal",
#                      "Spain",
#                      "Slovenia",
#                      "Slovakia")) |> 
#   # Reshape into long format
#   pivot_longer(
#     cols = starts_with("quarter_"),
#     names_to = "quarter",
#     values_to = "gdp_index"
#   ) |> 
#   mutate(gdp_index = ifelse(gdp_index == ":", NA, gdp_index)) |> 
#   # Format variables + Standardize country variable
#   mutate(
#     quarter = yq(str_sub(quarter, start = 9)),
#     gdp_index = as.numeric(gdp_index),
#     country = case_when(
#       TIME == "Austria" ~ "AT",
#       TIME == "Belgium" ~ "BE",
#       TIME == "Finland" ~ "FI",
#       TIME == "France" ~ "FR",
#       TIME == "Germany" ~ "DE",
#       TIME == "Greece" ~ "GR",
#       TIME == "Ireland" ~ "IE",
#       TIME == "Italy" ~ "IT",
#       TIME == "Netherlands" ~ "NL",
#       TIME == "Portugal" ~ "PT",
#       TIME == "Spain" ~ "ES",
#       TIME == "Slovenia" ~ "SI",
#       TIME == "Slovakia" ~ "SK"
#     )
#   ) |> 
#   select(country, quarter, gdp_index) #|> 
#   # Filter for period between 2000 and 2023
#   # filter(quarter > as.Date("1999-12-01") & quarter < as.Date("2024-01-01"))
#   
# # Calculate the average index for year 2015 (based on the data indexed to the year 2010)
# df_gdp_index <- df_gdp |>
#   mutate(year = year(quarter), .after = "quarter") |> 
#   group_by(country, year) |> 
#   mutate(
#     gdp_index_mean = mean(gdp_index)
#   ) |> 
#   filter(year == 2015) |>
#   select(country, year, gdp_index_mean) |> 
#   distinct(country, year, gdp_index_mean) 
# 
# # Index the data to the year 2015
# df_gdp <- df_gdp |> 
#   left_join(df_gdp_index, by = c("country")) |> 
#   mutate(
#     gdp_2015 = gdp_index / gdp_index_mean * 100
#   ) |> 
#   rename(
#     gdp_2010 = gdp_index
#   ) |> 
#   select(country, quarter, gdp_2010, gdp_2015)
# 
# Import Quarterly GDP
gdp <- fread(
  file = paste0(A, "c_eurostat/GDP/estat_namq_10_gdp_en.csv"),
  colClasses = "character"
  )

# Data Wrangling
df_gdp <- gdp |> 
  filter(unit %in% c("CLV_I15", # Chain Linked Volumes, Index 2015 # real GDP 
                   "CP_MEUR", # Current Price in EUR
                   "PD15_EUR")) |> # Implicit GDP Deflator
  filter(s_adj == "SCA") |> 
  filter(na_item == "B1GQ") |> 
  select(unit, geo, TIME_PERIOD, OBS_VALUE) |> 
  rename(
    country = geo,
    quarter = TIME_PERIOD,
    obs_value = OBS_VALUE
  ) |> 
  # Create time variable 
  mutate(
    quarter = yq(quarter),
    ovs_value = as.double(obs_value)
    ) |> 
  # Filter countries
  filter(country %in% c("AT", "BE", "FI", "FR", "DE", "IE", "IT", 
                        "NL", "PT", "ES", "SI", "SK", "EA" )) |> 
  # Filter for relevant time period
  filter(!(country == "GR" & quarter < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & quarter < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & quarter < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(quarter > as.Date("2002-12-01") & quarter < as.Date("2024-01-01")) # Data only available from 2003 on

# Get Real GDP
df_real_gdp <- df_gdp |>
  filter(unit == "CLV_I15") |> 
  rename(real_gdp = obs_value) |> 
  select(country, quarter, real_gdp) 
  


  

# 04. Unemployment Rate - Monthly - Total ======================================

# Import data
df_ur_raw <- read_excel(
  path = paste0(A, "c_eurostat/ei_lmhr_m_spreadsheet.xlsx"),
  sheet = "Sheet 1",
  range = cell_rows(10:48),
  col_types = "text"
)

# Basic Cleaning of the data
df_ur <- df_ur_raw |>
  # Select the right time and country
  select(-c(starts_with("..."))) |>
  rename_with(~ paste0("month_", .), -1) |> 
  filter(TIME %in% c("Austria",
                     "Belgium",
                     "Finland",
                     "France",
                     "Germany",
                     "Greece",
                     "Ireland",
                     "Italy",
                     "Netherlands",
                     "Portugal",
                     "Spain",
                     "Slovenia",
                     "Slovakia")) |> 
  # Reshape into long format
  pivot_longer(
    cols = starts_with("month_"),
    names_to = "month",
    values_to = "ur"
  ) |> 
  mutate(ur = ifelse(ur == ":", NA, ur)) |> 
  # Format variables + Standardize country variable
  mutate(
    month = as.Date(paste0(str_sub(month, start = 7), "-01")),
    ur = as.numeric(ur), # Potentially rescaling to decimal instead of %
    country = case_when(
      TIME == "Austria" ~ "AT",
      TIME == "Belgium" ~ "BE",
      TIME == "Finland" ~ "FI",
      TIME == "France" ~ "FR",
      TIME == "Germany" ~ "DE",
      TIME == "Greece" ~ "GR",
      TIME == "Ireland" ~ "IE",
      TIME == "Italy" ~ "IT",
      TIME == "Netherlands" ~ "NL",
      TIME == "Portugal" ~ "PT",
      TIME == "Spain" ~ "ES",
      TIME == "Slovenia" ~ "SI",
      TIME == "Slovakia" ~ "SK"
    )
  ) |> 
  select(country, month, ur)
# |> 
#   filter(month > as.Date("1999-12-01") & month < as.Date("2024-01-01"))


# 05. Industrial Production ====================================================

# Import 
ip <- fread(
  file = paste0(A, "c_eurostat/industrial_production/estat_sts_inpr_m_en.csv"),
  colClasses = "character"
)

# Data Wrangling
df_ip <- ip |> 
  filter(s_adj == "SCA") |> # Seasonally and calendar adjusted
  filter(nace_r2 == "B-D") |> # Mining and quarrying; manufacturing; electricity, gas, steam and air conditioning supply
  filter(unit == "I15") |> # Index to 2015
  # select relebant variables
  dplyr::select(geo, TIME_PERIOD, OBS_VALUE) |>
  mutate(
    month = as.Date(paste0(TIME_PERIOD, "-01")),
    ip = as.double(OBS_VALUE)
  ) |> 
  rename(country = geo) |> 
  # Filter countries
  filter(country %in% c("AT", "BE", "FI", "FR", "DE", "IE", "IT", 
                        "NL", "PT", "ES", "SI", "SK", "EA" )) |> 
  # Filter for relevant time period
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01"))  |> # Data only available from 2003 on
  select(country, month, ip)



# 05. Monthly - Dataset ========================================================

# Merge monthly data
df_eurostat_m <- df_hicp |> 
  full_join(df_reer, by = c("country", "month")) |> 
  full_join(df_ur, by = c("country", "month")) |>  
  full_join(df_ip, by = c("country", "month")) |> 
  # Filter for relevant time period
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01")) # Data only available from 2003 on

# Save monthly data
SAVE(dfx = df_eurostat_m, namex = paste0(MAINNAME, "_m"))

# 06. Quarterly - Dataset ======================================================

# Quaterly data
df_eurostat_q <- df_real_gdp

# Save quaterly data
SAVE(dfx = df_eurostat_1, namex = paste0(MAINNAME, "_q"))


###############################################################################+
################################# ENDE ########################################+
###############################################################################+