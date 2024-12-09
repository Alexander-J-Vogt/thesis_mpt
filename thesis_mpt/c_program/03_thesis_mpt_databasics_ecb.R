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


# 1. Banking Structural Statistic Indicator ====================================

# Read bulk download dataset
df_ssi_raw <- read.csv(paste0(A, "b_ecb/structural_finance_indicator.csv"), colClasses = "character")

# Create list of countries
euro_cntry <- c("AT", "BE", "CY", "DE", "EE", "ES", "FI", "FR", "GR", "IE", 
                "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK")
euro_cntry_relevant <- c("AT", "BE", "FI", "FR", "DE", "GR", "IE", "IT", "NL", 
                         "PT", "ES", "SI", "SK")

# Read, filter and bring data into wide forma
df_ssi <- df_ssi_raw |> 
  filter(REF_AREA %in% euro_cntry_relevant) |> 
  filter(SSI_INDICATOR %in% c("H10", "H20", "S10")) |> # Include only H10 and H20
  select(REF_AREA, TIME_PERIOD, SSI_INDICATOR, OBS_VALUE) |> 
  pivot_wider(names_from = "SSI_INDICATOR", values_from = "OBS_VALUE") |> # Wide Format
  rename(hhi_ci_total_assets = H10,
         hhi_total_credit = H20,
         share_top5_largest_ci_total_asset = S10,
         country = REF_AREA,
         year = TIME_PERIOD
  ) |> 
  mutate(across(-1, as.numeric))

# 2. Consolidated Banking Data =================================================

## 2.1 CBD (Discontinued) ------------------------------------------------------
# Data from 2007 and 2013

df_cbd <- read.csv(paste0(A, "b_ecb/cbd.csv"), colClasses = "character")


## 2.2 CBD2 --------------------------------------------------------------------
# Data from 2014 on
# Read bulk download dataset
df_cbd2 <- read.csv(paste0(A, "b_ecb/cbd2.csv"), colClasses = "character")

test <- df_cbd |> 
  filter(substr(CB_ITEM, 1, 3) == "D12")



# 3. Monetary Finanical Institutions (MFI) =====================================

# Import bulk download dataset
df_mfi_raw <- read.csv(paste0(A, "b_ecb/mfi.csv"), colClasses = "character")

# Read, filter and reshape data into wide format
df_mfi <- df_mfi_raw |> 
  filter(REF_AREA %in% euro_cntry_relevant) |> 
  filter(BS_ITEM %in% c("A22", "A2C", "A2CC")) |> # "A20" is total loan but is excluded
  filter(!MATURITY_NOT_IRATE %in% c("AM", "FM", "HHL", "HL","KF", "KKF", "KM")) |> 
  filter(DATA_TYPE_MIR == "R") |> 
  select(REF_AREA, TIME_PERIOD, BS_ITEM, MATURITY_NOT_IRATE, BS_COUNT_SECTOR, 
         CURRENCY_TRANS, IR_BUS_COV, OBS_VALUE) |> 
  mutate(
    maturity = case_when(
      MATURITY_NOT_IRATE == "A" ~ "total",
      MATURITY_NOT_IRATE == "F" ~ "up_to_1_year",
      MATURITY_NOT_IRATE == "H" ~ "over_2_years",
      MATURITY_NOT_IRATE == "I" ~ "over_1_and_up_to_5_years",
      MATURITY_NOT_IRATE == "J" ~ "over_5_years",
      MATURITY_NOT_IRATE == "K" ~ "over_1_year",
      MATURITY_NOT_IRATE == "O" ~ "over_5_and_up_to_10_years",
      MATURITY_NOT_IRATE == "P" ~ "over_10_years",
      TRUE ~ "Unknown"
    ),
    item = case_when(
      BS_ITEM == "A20" ~ "loans",
      BS_ITEM == "A22" ~ "lending_hp",
      BS_ITEM == "A2C" ~ "lending_hp_excl_v1",
      BS_ITEM == "A2CC" ~ "lending_hp_excl_v2"
    ), # Clear definition in NOTION
    coverage = case_when( # Interest Rate Business Sector Coverage Indicator
      IR_BUS_COV == "N" ~"nb", # New Business
      IR_BUS_COV == "O" ~ "outst_amount", # Outstanding Amount
      IR_BUS_COV == "P" ~ "pnl", # "Pure New Loans
      IR_BUS_COV == "R" ~ "reneg" # Renegotiation
    )
  ) |> 
  select(-c("BS_ITEM", "MATURITY_NOT_IRATE", "BS_COUNT_SECTOR", "IR_BUS_COV")) |> 
  pivot_wider(names_from = c("item", "maturity", "coverage"), 
              values_from = "OBS_VALUE",
              values_fill = NA) |> 
  rename(
    country = REF_AREA,
    month = TIME_PERIOD,
    currency = CURRENCY_TRANS
  ) |> 
  filter(currency == "EUR") |> 
  select(-currency) |> 
  mutate(across(-c(1:2), as.numeric)) |> 
  mutate(month = as.Date(paste0(month, "-01")))

# Filter for years, where countries were actually part of the Eurozone
df_mfi <- df_mfi |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01"))

# Create quarterly data 
df_mfi_q <- df_mfi |> 
  mutate(quarter = paste0(year(month), "-Q", quarter(month)), .after = "month") |> 
  group_by(country, quarter) |> 
  mutate(across(-c(1:2), ~ mean(.x, na.rm = TRUE), .names = "Q_{.col}")) |> 
  select(-month) |> 
  select(country, quarter, starts_with("Q_")) |> 
  distinct(country, quarter, .keep_all = TRUE) |> 
  rename_with(~ sub("^Q_", "", .), starts_with("Q_")) |> 
  mutate(quarter_date = lubridate::yq(quarter), .after = "quarter") |> 
  arrange(country, quarter)

lapply(colnames(df_mfi), function(x){
  cat(paste0("Variables: ", x))
  print(table(is.na(df_mfi[[x]])))
  cat("\n")
})


if (DEBUG) {
df_mfi |> 
  group_by(across(-DATA_TYPE_MIR)) |> 
  filter(n_distinct(DATA_TYPE_MIR) > 1) |> 
  ungroup()

df_mfi |> 
  summarise(across(-c(1:4), ~ sum(is.na(.)), .names = "missing_{col}")) |> 
  pivot_longer(everything(), names_to = "Column", values_to = "Missing_Count")

df_mfi_q |> 
  group_by(country) |> 
  summarise(across(-c(1:4), ~ sum(is.na(.)), .names = "missing_{.col}")) |> 
  pivot_longer(-country, names_to = "Column", values_to = "Missing_Count") |>
  pivot_wider(names_from = country, values_from = Missing_Count)

df_mfi_q |> 
  mutate(year = year(quarter)) |> 
  group_by(country, year) |> 
  summarise(across(-c(country, month, year), ~ sum(is.na(.)), .names = "missing_{.col}"), .groups = "drop") |> 
  pivot_longer(-c(country, year), names_to = "Column", values_to = "Missing_Count") |> 
  pivot_wider(names_from = country, values_from = "Missing_Count")

missings <- df_mfi |> 
  mutate(year = year(month), .after = "month") |> 
  group_by(country, year) |> 
  summarise(across(where(is.numeric), ~ sum(is.na(.)), .names = "missing_{.col}"), .groups = "drop") |> 
  pivot_longer(-c(country, year), names_to = "Column", values_to = "Missing_Count") |> 
  pivot_wider(names_from = country, values_from = "Missing_Count") |> 
  group_by(year) |> 
  summarise(across(-Column, ~ sum(.), .names = "total_missing_{.col}"), .groups = "drop")
}
# 04. Balance Sheet Items (BSI) ================================================

# Import bulk download
df_bsi_raw <- read_csv(paste0(A, "b_ecb/bsi.csv"), col_types = cols(.default = "c"))

df_bsi <- df_bsi_raw |> 
  filter(REF_AREA %in% euro_cntry_relevant) 

# 04.1 Lending for House Purchases ---------------------------------------------
df_bsi_a22 <- df_bsi |> 
  filter(BS_ITEM == "A22") |> # Loan for house purchases 
  filter(MATURITY_ORIG == "A") |>  # Total Maturity
  filter(COUNT_AREA == "U6") |> # Concentrate on Domestic Area and not EU Changing Composition (U2)
  mutate(data_type = case_when(
    DATA_TYPE == "1" ~ "hp_outst_amount_EUR", # Outstanding Amount at the end of the period (stocks)
    DATA_TYPE == "4" ~ "hp_fin_transaction_EUR", # Financial Transactions
    DATA_TYPE == "I" ~ "hp_index_notional_stocks_PPCH" # Index of Notional Stocks
  ), .after = DATA_TYPE) |> 
  select(c("REF_AREA", "TIME_PERIOD", "data_type", "OBS_VALUE", "OBS_STATUS")) |> 
  pivot_wider(names_from = "data_type", 
              values_from = "OBS_VALUE") |>
  mutate(across(-c(1:4), as.numeric)) |> 
  arrange(REF_AREA, TIME_PERIOD) |> 
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01" )), .after = TIME_PERIOD) |> # Format Data Variable
  rename(country = REF_AREA) |> 
  select(-c("TIME_PERIOD", "OBS_STATUS"))

# Filter for years, where countries were actually part of the Eurozone + Exclude the year 2014
df_bsi_a22 <- df_bsi_a22 |> 
  filter(month < as.Date("2024-01-01")) |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) # Filter for years with SK being part of the Eurozone

## 04.2 Deposit Liabilities ----------------------------------------------------

# Create dataset in deposit liabilities and reshape into wide format
df_bsi_l20 <- df_bsi |> 
  filter(BS_ITEM == "L20") |> 
  filter(FREQ == "M") |> # Also data on quarterly basis available
  filter(COUNT_AREA == "U6") |> # Concentrate on Domestic Area and not EU Changing Composition (U2)
  filter(BS_COUNT_SECTOR == "2250") |> # Interested in Households and non-profit institutions serving households
  mutate(data_type = case_when(
    DATA_TYPE == "1" ~ "dl_outst_amount_EUR", # Outstanding Amount at the end of the period (stocks)
    DATA_TYPE == "4" ~ "dl_fin_transaction_EUR", # Financial Transactions
    DATA_TYPE == "I" ~ "dl_index_notional_stocks_PPCH" # Index of Notional Stocks
  ), .after = DATA_TYPE) |> 
  select(c("REF_AREA", "TIME_PERIOD", "data_type", "OBS_VALUE")) |> 
  pivot_wider(names_from = "data_type",
              values_from = "OBS_VALUE") |> # Put Dataset into wide format
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01")), .after = TIME_PERIOD) |>  # Format Date Variable
  mutate(across(c(4:6), as.integer)) |> 
  rename(country = REF_AREA) |> 
  arrange(country, month) |> 
  select(-c("TIME_PERIOD"))
  
   
# Filter for years, where countries were actually part of the Eurozone + Exclude the year 2014
df_bsi_l20 <- df_bsi_l20 |> 
  filter(month < as.Date("2024-01-01")) |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) # Filter for years with SK being part of the Eurozone

## 04.3 Total Assets / Liability -----------------------------------------------

# Create dataset for Total Assets / Liabilities and reshape into wide format
df_bsi_t00 <- df_bsi |> 
  filter(BS_ITEM == "T00") |>
  filter(FREQ == "M") |>  # Monthly Variable
  filter(BS_REP_SECTOR == "A") |>  # MFI exld ESCB
  mutate(data_type = case_when(
    DATA_TYPE == "1" ~ "tl_outst_amount_EUR", # Outstanding Amount at the end of the period (stocks)
    DATA_TYPE == "4" ~ "tl_fin_transaction_EUR", # Financial Transactions
    DATA_TYPE == "I" ~ "tl_index_notional_stocks_PPCH" # Index of Notional Stocks
  ), .after = DATA_TYPE) |> 
  select(c("REF_AREA", "TIME_PERIOD", "data_type", "OBS_VALUE")) |>
  pivot_wider(names_from = "data_type",
              values_from = "OBS_VALUE"
              ) |> 
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01")), .after = TIME_PERIOD) |>  # Format Date Variable
  mutate(across(c(4:6), as.integer)) |> 
  rename(country = REF_AREA) |> 
  arrange(country, month) |> 
  select(-c("TIME_PERIOD"))

df_bsi_t00 <- df_bsi_t00 |> 
  filter(month > as.Date("1999-09-01") & month < as.Date("2023-12-01")) |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) # Filter for years with SK being part of the Eurozone

## 04.4 Capital & Reserves -----------------------------------------------------

# Create dataset for Capital & Reserves and reshape into wide format
df_bsi_l60 <- df_bsi |> 
  filter(BS_ITEM == "L60") |> 
  filter(FREQ == "M") |>  # Monthly Variable
  filter(BS_REP_SECTOR == "A") |> # Credit Institutions
  mutate(data_type = case_when(
    DATA_TYPE == "1" ~ "cr_outst_amount_EUR", # Outstanding Amount at the end of the period (stocks)
    DATA_TYPE == "4" ~ "cr_fin_transaction_EUR", # Financial Transactions
    DATA_TYPE == "I" ~ "cr_index_notional_stocks_PPCH" # Index of Notional Stocks
  ), .after = DATA_TYPE) |>  # MFI exld ESCB
  select(c("REF_AREA", "TIME_PERIOD", "data_type", "OBS_VALUE")) |>
  pivot_wider(names_from = "data_type",
              values_from = "OBS_VALUE"
  ) |> 
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01")), .after = TIME_PERIOD) |>  # Format Date Variable
  mutate(across(c(4:6), as.integer)) |> 
  rename(country = REF_AREA) |> 
  arrange(country, month) |> 
  select(-c("TIME_PERIOD"))

df_bsi_l60 <- df_bsi_l60 |> 
  filter(month > as.Date("1999-01-01") & month < as.Date("2023-12-01")) |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) # Filter for years with SK being part of the Eurozone


if (DEBUG) {
View(df_bsi_t00)

df_bsi_l60 |> 
  filter(is.na(cr_fin_transaction_EUR))

  filter(BS_ITEM %in% c("A22", "T00", "L60", "L20")) 

df_bsi_m <- df_bsi |> 
  filter(FREQ == "M") |> 
  filter(BS_ITEM %in% c("L60", "T00"))

df_bsi_q <- df_bsi |> 
  filter(FREQ == "Q")

names <- colnames(df_bsi_l60)[-c(1, 14, 15, 33, 34, 35)]
lapply(names, function(x) {
  cat(paste0("Variable: ", x))
  print(table(df_bsi_l60[[x]]))
  cat("\n")
})

table(df_bsi_m$BS_ITEM, df_bsi_m$MATURITY_ORIG)
table(df_bsi_m$BS_ITEM, df_bsi_m$DATA_TYPE)

table(df_bsi_m$TIME_PERIOD, df_bsi_m$BS_ITEM)
table(df_bsi_m$TIME_PERIOD, df_bsi_m$DATA_TYPE)

table(df_bsi_a22$REF_AREA, df_bsi_a22$CURRENCY_TRANS)

table(df_bsi_a22$MATURITY_ORIG, df_bsi_a22$CURRENCY_TRANS)
table(df_bsi_a22$TIME_PERIOD, df_bsi_a22$MATURITY_ORIG)
}

# 05. Monthly Dataset ==========================================================

# Merge Monthly dataset
df_ecb_monthly <- df_bsi_a22 |> 
  full_join(df_bsi_l20, by = c("country", "month")) |> 
  full_join(df_bsi_l60, by = c("country", "month")) |> 
  full_join(df_bsi_t00, by = c("country", "month")) |> 
  full_join(df_mfi, by = c("country", "month"))

# SAVE
SAVE(dfx = df_ecb_monthly, namex = paste0(MAINNAME, "_m"))

# 06. Annual ===================================================================

# Annual Dataset
df_ecb_annual <- df_ssi

# SAVE
SAVE(dfx = df_ecb_annual, namex = paste0(MAINNAME, "_a"))



###############################################################################+
################################# ENDE ########################################+
###############################################################################+