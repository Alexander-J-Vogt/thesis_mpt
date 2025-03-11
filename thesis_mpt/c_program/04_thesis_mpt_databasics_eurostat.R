# TARGET: Import Eurostat data for HICP, HDP, IP, UR, Homeownership Rate, and House Price Index by BSI
# OUTDATA/ OUTPUT: MAINNAME for quarterly and monthly data

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

# Import Quarterly GDP
hicp <- fread(
  file = paste0(A, "c_eurostat/HICP/estat_prc_hicp_midx_en.csv"),
  colClasses = "character"
)

# Data Wrangling
df_hicp <- hicp |> 
  filter(coicop == "CP00") |> # Total Inflation Index
  filter(unit == "I15") |> # Indexed to 2015
  # Filter countries
  filter(geo %in% c("AT", "BE", "FI", "FR", "DE", "IE", "EL", "IT", 
                        "NL", "PT", "ES", "SI", "SK", "EA" )) |> 
  # Select & Rename
  dplyr::select(geo, TIME_PERIOD, OBS_VALUE) |> 
  rename(
    country = geo,
    month = TIME_PERIOD,
    hicp = OBS_VALUE
  ) |> 
  mutate(country = if_else(country == "EL", "GR", country)) |> 
  # Format
  mutate(
    month = as.Date(paste0(month, "-01")),
    hicp = as.double(hicp)
  ) |> 
  group_by(country) |> 
  mutate(
    hicp_inflation = (hicp - lag(hicp, 12)) / hicp * 100,
  ) |> 
  ungroup() |> 
  # Filter for relevant time period
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01")) # Data only available from 2003 on
  

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
  dplyr::select(-c(starts_with("..."))) |>
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
  dplyr::select(country, month, reer)



# 03. GDP - Quarterly ==========================================================

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
  dplyr::select(unit, geo, TIME_PERIOD, OBS_VALUE) |> 
  rename(
    country = geo,
    quarter = TIME_PERIOD,
    obs_value = OBS_VALUE
  ) |> 
  # Create time variable 
  mutate(
    quarter = yq(quarter),
    ovb_value = as.double(obs_value)
    ) |> 
  # Filter countries
  filter(country %in% c("AT", "BE", "FI", "FR", "DE", "IE","EL", "IT", 
                        "NL", "PT", "ES", "SI", "SK", "EA" )) |> 
  mutate(country = if_else(country == "EL", "GR", country)) |> 
  # Filter for relevant time period
  filter(!(country == "GR" & quarter < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & quarter < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & quarter < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(quarter > as.Date("2002-12-01") & quarter < as.Date("2024-01-01")) # Data only available from 2003 on

# Get Real GDP
df_real_gdp <- df_gdp |>
  filter(unit == "CLV_I15") |> 
  rename(real_gdp = obs_value) |> 
  dplyr::select(country, quarter, real_gdp) |> 
  rename(month = quarter) |> 
  mutate(real_gdp = as.numeric(real_gdp))
  

# 04. Unemployment Rate - Monthly - Total ======================================

# Import data
df_ur_raw <- read_excel(
  path = paste0(A, "c_eurostat/UR/ei_lmhr_m_spreadsheet.xlsx"),
  sheet = "Sheet 1",
  range = cell_rows(10:48),
  col_types = "text"
)

# Basic Cleaning of the data
df_ur <- df_ur_raw |>
  # Select the right time and country
  dplyr::select(-c(starts_with("..."))) |>
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
  dplyr::select(country, month, ur)


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
  filter(country %in% c("AT", "BE", "FI", "FR", "DE", "EL", "IT", "IE",
                        "NL", "PT", "ES", "SI", "SK", "EA" )) |> 
  mutate(country = if_else(country == "EL", "GR", country)) |> 
  # Filter for relevant time period
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01"))  |> # Data only available from 2003 on
  dplyr::select(country, month, ip)


# 06. USD/EUR Exchange Rate ====================================================

# Import 
exr <- fread(
  file = paste0(A, "c_eurostat/EU_US_exchange_rate/estat_ert_bil_eur_m_en.csv"),
  colClasses = "character"
)

# Data Wrangling
df_exr <- exr |> 
  filter(currency == "USD") |> # EUR/US Exchange Rate
  filter(statinfo == "AVG") |> # AVERAGE
  # Select and Rename
  dplyr::select(TIME_PERIOD, OBS_VALUE) |>  
  rename(
    month = TIME_PERIOD,
    exr = OBS_VALUE
  ) |> 
  # Format
  mutate(
    month = as.Date(paste0(month, "-01")),
    exr = as.double(exr)
  )

# 07. Homeownership Rate =======================================================

hosr <- fread(
  file = paste0(A, "c_eurostat/Homeownership/estat_ilc_lvho02_filtered_en.csv"),
  colClasses = "character"
)

# Data Wrangling Homeownership with mortgage loan
df_hosr <- hosr |> 
  filter(tenure == "Owner") |> 
  filter(geo %in% c("Austria",
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
  mutate(
    hosr = as.numeric(OBS_VALUE), # Potentially rescaling to decimal instead of %
    year = as.numeric(TIME_PERIOD),
    country = case_when(
      geo == "Austria" ~ "AT",
      geo == "Belgium" ~ "BE",
      geo == "Finland" ~ "FI",
      geo == "France" ~ "FR",
      geo == "Germany" ~ "DE",
      geo == "Greece" ~ "GR",
      geo == "Ireland" ~ "IE",
      geo == "Italy" ~ "IT",
      geo == "Netherlands" ~ "NL",
      geo == "Portugal" ~ "PT",
      geo == "Spain" ~ "ES",
      geo == "Slovenia" ~ "SI",
      geo == "Slovakia" ~ "SK"
    )
  ) |> 
  dplyr::select(year, country, hosr) |> 
  arrange(country, year)


# Impute Missings
time <- rep(seq(2003, 2023, by = 1), 13)

EA <- c("AT", "BE", "FI", "FR", "DE", "GR", "IE", "IT", 
        "NL", "PT", "ES", "SI", "SK")

df <- data.frame(year = time, country = rep(EA, each = 21))

df_hosr <-  df |> 
  left_join(df_hosr, by = c("country", "year")) |> 
  group_by(country) |> 
  fill(hosr, .direction = "up") |> 
  fill(hosr, .direction = "down") |> 
  ungroup()


# 8. House Price Index (BSI) ===================================================

hpi1 <- read_excel(
  path = paste0(A, "c_eurostat/House Price Index/bis_dp_search_export_20250302-120036.xlsx"),
  sheet = "timeseries observations",
  col_types = "text"
)

hpi2 <- read_excel(
  path = paste0(A, "c_eurostat/House Price Index/bis_dp_search_export_20250302-120215.xlsx"),
  sheet = "timeseries observations",
  col_types = "text"
)

# Bind Datasets
hpi <- bind_rows(hpi1, hpi2)

# Data Wrangling
df_hpi <- hpi |> 
  dplyr::select(`REF_AREA:Reference area`, `TIME_PERIOD:Period`, `OBS_VALUE:Value`) |> 
  rename(
    country = `REF_AREA:Reference area`,
    quarter = `TIME_PERIOD:Period`,
    hpi = `OBS_VALUE:Value`
  ) |> 
  mutate(country = str_sub(country, 1, 2),
         month = as.Date(paste0(str_sub(quarter, 1, 7), "-01")),
         year = year(quarter),
         hpi = as.numeric(hpi)) |> 
  dplyr::select(country, month, hpi)

# Calculate Index for year 2015
index <- df_hpi |> 
  filter(year(month) == 2015) |> 
  group_by(country) |> 
  summarize(index = mean(hpi))

# Reindex to year 2015
df_hpi <- df_hpi |> 
  left_join(index, by = "country") |> 
  mutate(hpi = hpi / index * 100) |> 
  dplyr::select(-index) |> 
  # mutate(month = as.character(month))  %>%
  mutate(month = case_when(
    month(month) %in% c(1, 2, 3) ~ make_date(year(month), 1, 1),
    month(month) %in% c(4, 5, 6) ~ make_date(year(month), 4, 1),
    month(month) %in% c(7, 8, 9) ~ make_date(year(month), 7, 1),
    month(month) %in% c(10, 11, 12) ~ make_date(year(month), 10, 1)
  )) |> 
  distinct(.keep_all = TRUE)



# 09. House Price-to-Income Ratio ==============================================

# Import data
hpti_ratio_raw <- read_excel(
  path = paste0(A, "c_eurostat/House_Price_to_Income_Ratio/tipsho60_spreadsheet.xlsx"),
  sheet = "Sheet 2",
  range = cell_rows(8:38),
  col_types = "text"
)

df_hpti_ratio <- hpti_ratio_raw |> 
  dplyr::select(-c(starts_with("..."))) |>
  rename_with(~ paste0("year_", .), -1) |> 
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
    cols = starts_with("year_"),
    names_to = "year",
    values_to = "hpti_ratio"
  ) |> 
  # Format variables + Standardize country variable
  mutate(
    year = as.numeric(str_sub(year, 6, 9)),
    hpti_ratio = as.numeric(hpti_ratio),
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
  dplyr::select(country, year, hpti_ratio)


# 09. Monthly - Dataset ========================================================

# Merge monthly data
df_eurostat_m <- df_hicp |> 
  full_join(df_reer, by = c("country", "month")) |> 
  full_join(df_ur, by = c("country", "month")) |>  
  full_join(df_ip, by = c("country", "month")) |> 
  full_join(df_exr, by = "month") |> 
  # Filter for relevant time period
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01")) # Data only available from 2003 on

# Save monthly data
SAVE(dfx = df_eurostat_m, namex = paste0(MAINNAME, "_m"))

# 10. Quarterly - Dataset ======================================================

# Merge quarterly data
df_quarterly <- df_real_gdp |> 
  left_join(df_hpi, by = c("country", "month"))

# Save quarterly data
SAVE(dfx = df_quarterly, namex = paste0(MAINNAME, "_q"))

# 11. Annual Dataset ===========================================================

# Merge Annual Data
df_annual <- df_hosr |> 
  full_join(df_hpti_ratio, by = c("year", "country"))

# SAVE
SAVE(dfx = df_annual, namex = paste0(MAINNAME, "_a"))


###############################################################################+
################################# ENDE ########################################+
###############################################################################+