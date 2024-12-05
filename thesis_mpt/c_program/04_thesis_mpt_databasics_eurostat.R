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

# 01. HICP - monthly ===========================================================

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

# 02. Effective Exchange Rate Index - monthly ==================================

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

# Import data (Index = 2015)
df_gdp_raw <- read_excel(
  path = paste0(A, "c_eurostat/namq_10_gdp__custom_14043017_spreadsheet.xlsx"),
  sheet = "Sheet 1",
  range = cell_rows(10:54),
  col_types = "text"
)

# Basic Cleaning of the data
df_gdp <- df_gdp_raw |>
  # Select the right time and country
  select(-c(starts_with("..."))) |>
  rename_with(~ paste0("quarter_", .), -1) |> 
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
    cols = starts_with("quarter_"),
    names_to = "quarter",
    values_to = "gdp_index"
  ) |> 
  mutate(gdp_index = ifelse(gdp_index == ":", NA, gdp_index)) |> 
  # Format variables + Standardize country variable
  mutate(
    quarter = yq(str_sub(quarter, start = 9)),
    gdp_index = as.numeric(gdp_index),
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
  select(country, quarter, gdp_index) |> 
  # Filter for period between 2000 and 2023
  filter(quarter > as.Date("1999-12-01") & quarter < as.Date("2024-01-01"))
  
# Calculate the average index for year 2015 (based on the data indexed to the year 2010)
df_gdp_index <- df_gdp |>
  mutate(year = year(quarter), .after = "quarter") |> 
  group_by(country, year) |> 
  mutate(
    gdp_index_mean = mean(gdp_index)
  ) |> 
  filter(year == 2015) |>
  select(country, year, gdp_index_mean) |> 
  distinct(country, year, gdp_index_mean) 

# Index the data to the year 2015
df_gdp <- df_gdp |> 
  left_join(df_gdp_index, by = c("country")) |> 
  mutate(
    gdp_2015 = gdp_index / gdp_index_mean * 100
  ) |> 
  rename(
    gdp_2010 = gdp_index
  ) |> 
  select(country, quarter, gdp_2010, gdp_2015)

# 04. Unemployment Rate - monthly - Total ======================================

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
  select(country, month, ur) |> 
  filter(month > as.Date("1999-12-01") & month < as.Date("2024-01-01"))

