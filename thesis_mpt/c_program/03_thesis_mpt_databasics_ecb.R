# TARGET: Import data from BSI, MFI & SSI
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
  filter(SSI_INDICATOR %in% c("H10", "H20", "S10")) |> # Include only H10, H20 & S10
  dplyr::select(REF_AREA, TIME_PERIOD, SSI_INDICATOR, OBS_VALUE) |> 
  pivot_wider(names_from = "SSI_INDICATOR", values_from = "OBS_VALUE") |> # Wide Format
  rename(hhi_ci_total_assets = H10, # Herfindahl index for Credit institutions (CIs) total assets
         hhi_total_credit = H20, # Herfindahl index for total credit
         share_top5_largest_ci_total_asset = S10, # Share of the 5 largest CIs in total assets (CR5)
         country = REF_AREA,
         year = TIME_PERIOD
  ) |> 
  mutate(across(-1, as.numeric))


# 2.1 Monetary Finanical Institutions (MFI) =====================================

# Import bulk download dataset
df_mfi_raw <- read.csv(paste0(A, "b_ecb/mfi.csv"), colClasses = "character")

# 2.1 Lending Rate for Total Lending and House Purchases -----------------------
df_mfi <- df_mfi_raw |> 
  filter(REF_AREA %in% euro_cntry_relevant) |> 
  filter(BS_ITEM %in% c("A22", "A2C", "A2CC")) |> # "A20" is total loan but is excluded
  filter(!MATURITY_NOT_IRATE %in% c("AM", "FM", "HHL", "HL","KF", "KKF", "KM")) |> 
  filter(DATA_TYPE_MIR == "R") |> 
  dplyr::select(REF_AREA, TIME_PERIOD, BS_ITEM, MATURITY_NOT_IRATE, BS_COUNT_SECTOR, 
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
      BS_ITEM == "A20" ~ "loans", # Total Lending
      BS_ITEM == "A22" ~ "lending_hp", # Lending for House Purchases
      BS_ITEM == "A2C" ~ "lending_hp_excl_v1", # Lending for House Purchases excl. loans with maturity above 1 year 
      BS_ITEM == "A2CC" ~ "lending_hp_excl_v2" # Lending for House Purchases excl. loan with maturiy abover 5 years
    ), # Clear definition in NOTION
    coverage = case_when( # Interest Rate Business Sector Coverage Indicator
      IR_BUS_COV == "N" ~"nb", # New Business
      IR_BUS_COV == "O" ~ "outst_amount", # Outstanding Amount
      IR_BUS_COV == "P" ~ "pnl", # "Pure New Loans
      IR_BUS_COV == "R" ~ "reneg" # Renegotiation
    )
  ) |> 
  dplyr::select(-c("BS_ITEM", "MATURITY_NOT_IRATE", "BS_COUNT_SECTOR", "IR_BUS_COV")) |> 
  pivot_wider(names_from = c("item", "maturity", "coverage"), 
              values_from = "OBS_VALUE",
              values_fill = NA) |> 
  rename(
    country = REF_AREA,
    month = TIME_PERIOD,
    currency = CURRENCY_TRANS
  ) |> 
  filter(currency == "EUR") |> 
  dplyr::select(-currency) |> 
  mutate(across(-c(1:2), as.numeric)) |> 
  mutate(month = as.Date(paste0(month, "-01")))

# Filter for years, where countries were actually part of the Eurozone
df_mfi <- df_mfi |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01")) # Data only available from 2003 on

# Create quarterly data 
df_mfi_q <- df_mfi |> 
  mutate(quarter = paste0(year(month), "-Q", quarter(month)), .after = "month") |> 
  group_by(country, quarter) |> 
  mutate(across(-c(1:2), ~ mean(.x, na.rm = TRUE), .names = "Q_{.col}")) |> 
  dplyr::select(-month) |> 
  dplyr::select(country, quarter, starts_with("Q_")) |> 
  distinct(country, quarter, .keep_all = TRUE) |> 
  rename_with(~ sub("^Q_", "", .), starts_with("Q_")) |> 
  mutate(quarter_date = lubridate::yq(quarter), .after = "quarter") |> 
  arrange(country, quarter)

lapply(colnames(df_mfi), function(x){
  cat(paste0("Variables: ", x))
  print(table(is.na(df_mfi[[x]])))
  cat("\n")
})


## 2.2 Deposit Rate ------------------------------------------------------------

df_deposit_rate <- df_mfi_raw |> 
  filter(REF_AREA %in% euro_cntry_relevant) |> 
  filter(BS_ITEM %in% c("L21")) |> # "A20" is total loan but is excluded
  filter(BS_COUNT_SECTOR == "2240") |> 
  dplyr::select(TIME_PERIOD, REF_AREA, OBS_VALUE) |> 
  rename(
    month = TIME_PERIOD,
    country = REF_AREA,
    deposit_rate = OBS_VALUE
  ) |> 
  mutate(month = as.Date(paste0(month, "-01")))

# Filter for years, where countries were actually part of the Eurozone
df_deposit_rate <- df_deposit_rate |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  filter(month > as.Date("2002-12-01") & month < as.Date("2024-01-01")) # Data only available from 2003 on

# 04. Balance Sheet Items (BSI) ================================================

# Import bulk download
df_bsi_raw <- read_csv(paste0(A, "b_ecb/bsi.csv"), col_types = cols(.default = "c"))

df_bsi <- df_bsi_raw |> 
  filter(REF_AREA %in% euro_cntry_relevant) 


# 04.1 Lending for House Purchases ---------------------------------------------

df_bsi_a22 <- df_bsi |> 
  filter(BS_ITEM == "A22") |> # Loan for house purchases 
  filter(MATURITY_ORIG == "A") |>  # Total Maturity
  filter(COUNT_AREA == "U6") |>  # Concentrate on Domestic Area and not EU Changing Composition (U2)
  mutate(data_type = case_when(
    DATA_TYPE == "1" ~ "hp_outst_amount_EUR", # Outstanding Amount at the end of the period (stocks)
    DATA_TYPE == "4" ~ "hp_fin_transaction_EUR", # Financial Transactions
    DATA_TYPE == "I" ~ "hp_index_notional_stocks_PPCH" # Index of Notional Stocks
  ), .after = DATA_TYPE) |> 
  dplyr::select(c("REF_AREA", "TIME_PERIOD", "data_type", "OBS_VALUE", "OBS_STATUS")) |> 
  pivot_wider(names_from = "data_type", 
              values_from = "OBS_VALUE") |>
  mutate(across(-c(1:3), as.numeric)) |> 
  arrange(REF_AREA, TIME_PERIOD) |> 
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01" )), .after = TIME_PERIOD) |> # Format Data Variable
  rename(country = REF_AREA) |> 
  dplyr::select(-c("TIME_PERIOD", "OBS_STATUS"))

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
  dplyr::select(c("REF_AREA", "TIME_PERIOD", "data_type", "OBS_VALUE")) |> 
  pivot_wider(names_from = "data_type",
              values_from = "OBS_VALUE") |> # Put Dataset into wide format
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01")), .after = TIME_PERIOD) |>  # Format Date Variable
  mutate(across(c(4:6), as.integer)) |> 
  rename(country = REF_AREA) |> 
  arrange(country, month) |> 
  dplyr::select(-c("TIME_PERIOD"))
  
   
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
  dplyr::select(c("REF_AREA", "TIME_PERIOD", "data_type", "OBS_VALUE")) |>
  pivot_wider(names_from = "data_type",
              values_from = "OBS_VALUE"
              ) |> 
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01")), .after = TIME_PERIOD) |>  # Format Date Variable
  mutate(across(c(4:6), as.integer)) |> 
  rename(country = REF_AREA) |> 
  arrange(country, month) |> 
  dplyr::select(-c("TIME_PERIOD"))

df_bsi_t00 <- df_bsi_t00 |> 
  filter(month > as.Date("1999-09-01") & month < as.Date("2024-01-01")) |> 
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
  dplyr::select(c("REF_AREA", "TIME_PERIOD", "data_type", "OBS_VALUE")) |>
  pivot_wider(names_from = "data_type",
              values_from = "OBS_VALUE"
  ) |> 
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01")), .after = TIME_PERIOD) |>  # Format Date Variable
  mutate(across(c(4:6), as.integer)) |> 
  rename(country = REF_AREA) |> 
  arrange(country, month) |> 
  dplyr::select(-c("TIME_PERIOD"))

df_bsi_l60 <- df_bsi_l60 |> 
  filter(month > as.Date("1999-01-01") & month < as.Date("2024-01-01")) |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) # Filter for years with SK being part of the Eurozone


## 04.5 Loan Volumes -----------------------------------------------------------

df_bsi_a20 <- df_bsi |>
  filter(BS_ITEM == "A20") |> # Loan for house purchases
  filter(MATURITY_ORIG == "A") |>  # Total Maturity
  filter(COUNT_AREA == "U2") |>  # Concentrate on Domestic Area and not EU Changing Composition (U2)
  filter(FREQ == "M") |>
  filter(DATA_TYPE == "1") |>  # Outstanding Amount
  filter(BS_COUNT_SECTOR == "0000") |> # Deposit-taking corporations except the central bank (S.122) (19)
  filter(BS_REP_SECTOR == "A")  |> # MFIs excluding ESCB
  dplyr::select(REF_AREA, TIME_PERIOD, OBS_VALUE) |> 
  mutate(across(-c(1:2), as.numeric)) |>
  arrange(REF_AREA, TIME_PERIOD) |>
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01" )), .after = TIME_PERIOD) |> # Format Data Variable
  rename(country = REF_AREA, total_loan = OBS_VALUE) |>
  dplyr::select(-c("TIME_PERIOD")) 

df_bsi_a20 <- df_bsi_a20 |> 
  filter(month > as.Date("1999-01-01") & month < as.Date("2024-01-01")) |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) # Filter for years with SK being part of the Eurozone


## 04.6 Deposit Volume ---------------------------------------------------------

df_bsi_l21 <- df_bsi |>
  filter(BS_ITEM == "L21") |> # Overnight deposits
  filter(MATURITY_ORIG == "A") |> # Total Maturity
  filter(COUNT_AREA == "U2") |>  # Concentrate on Domestic Area and not EU Changing Composition (U2)
  filter(FREQ == "M") |>
  filter(DATA_TYPE == "1") |> 
  filter(BS_COUNT_SECTOR == "2250") |>  # Households and non-profit organizations that serve households (s.14 and S.15)
  dplyr::select(REF_AREA, TIME_PERIOD, OBS_VALUE) |> 
  mutate(across(-c(1:2), as.numeric)) |>
  arrange(REF_AREA, TIME_PERIOD) |>
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01" )), .after = TIME_PERIOD) |> # Format Data Variable
  rename(country = REF_AREA, overnight_deposits = OBS_VALUE) |>
  dplyr::select(-c("TIME_PERIOD")) 

df_bsi_l21 <- df_bsi_l21 |> 
  filter(month > as.Date("1999-01-01") & month < as.Date("2024-01-01")) |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) # Filter for years with SK being part of the Eurozone


## 04.7 MMF Volume -------------------------------------------------------------

df_bsi_a42 <- df_bsi |>
  filter(BS_ITEM == "L21") |> # MMF
  filter(MATURITY_ORIG == "A") |> # Total Maturity
  filter(COUNT_AREA == "U2") |>  # Concentrate on Domestic Area and not EU Changing Composition (U2)
  filter(FREQ == "M") |>
  filter(DATA_TYPE == "1") |> 
  filter(BS_COUNT_SECTOR == "2250") |>  # Households and non-profit organizations that serve households (s.14 and S.15)
  dplyr::select(REF_AREA, TIME_PERIOD, OBS_VALUE) |> 
  mutate(across(-c(1:2), as.numeric)) |>
  arrange(REF_AREA, TIME_PERIOD) |>
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01" )), .after = TIME_PERIOD) |> # Format Data Variable
  rename(country = REF_AREA, MMF = OBS_VALUE) |>
  dplyr::select(-c("TIME_PERIOD"))

df_bsi_a42 <- df_bsi_a42 |> 
  filter(month > as.Date("1999-01-01") & month < as.Date("2024-01-01")) |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) # Filter for years with SK being part of the Eurozone


## 04.8 Debt Volume ------------------------------------------------------------

df_bsi_a30 <- df_bsi |>
  filter(BS_ITEM == "A30") |> # Debt Securities Held - (Measure for bonds volume)
  filter(MATURITY_ORIG == "A") |> # Total Maturity
  filter(COUNT_AREA == "U6") |>  # Concentrate on Domestic Area and not EU Changing Composition (U2)
  filter(FREQ == "M") |>
  filter(DATA_TYPE == "1") |> 
  filter(BS_COUNT_SECTOR == "1000") |>  # Monetary financial institutions (MFIs) (4679)
  dplyr::select(REF_AREA, TIME_PERIOD, OBS_VALUE) |> 
  mutate(across(-c(1:2), as.numeric)) |>
  arrange(REF_AREA, TIME_PERIOD) |>
  mutate(month = as.Date(paste0(TIME_PERIOD, "-01" )), .after = TIME_PERIOD) |> # Format Data Variable
  rename(country = REF_AREA, bonds = OBS_VALUE) |>
  dplyr::select(-c("TIME_PERIOD"))

df_bsi_a30 <- df_bsi_a30 |> 
  filter(month > as.Date("1999-01-01") & month < as.Date("2024-01-01")) |> 
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) # Filter for years with SK being part of the Eurozone


# 05. ECB's Policy Rates =======================================================

## 05.1 Main Refinancing Rate --------------------------------------------------

# Import
mpr <- read_csv(file = paste0(A, "b_ecb/", "ECB Data Portal - Main Refinancing Operator.csv"))

# Create monthly vector from 1999 to 2024
month <- tibble(month = seq(as.Date("1999-01-01"), as.Date("2024-12-01"), by = "month"))

# Create monthly dataset
df_mpr <- mpr |>
  # Create month variable
  mutate(
    month = as.Date(paste0(str_sub(DATE, 1, 7), "-01"))
  ) |> 
  # Rename for readbility
  rename(
    mpr = `Main refinancing operations - fixed rate tenders (fixed rate) (date of changes) - Level (FM.B.U2.EUR.4F.KR.MRR_FR.LEV)`,
    date = DATE
  ) |> 
  # Collapse by month
  group_by(month) |> 
  summarise(
    mpr = mean(mpr)
  ) |> 
  ungroup()

# Merge
df_mpr <- month |> 
  left_join(df_mpr, by = c("month")) |> 
  mutate(mpr = nafill(mpr, type = "locf") / 100)

## 05.2 Deposit Facility Rate --------------------------------------------------

# Import 
dfr <- read_csv(file = paste0(A, "b_ecb/", "ECB Data Portal - Deposit Facility Rate.csv"))

# Create monthly vector from 1999 to 2024
month <- tibble(month = seq(as.Date("1999-01-01"), as.Date("2024-12-01"), by = "month"))

# Create monthly dataset
df_dfr <- dfr |>
  # Create month variable
  mutate(
    month = as.Date(paste0(str_sub(DATE, 1, 7), "-01"))
  ) |> 
  # Rename for readbility
  rename(
    dfr = `Deposit facility - date of changes (raw data) - Level (FM.B.U2.EUR.4F.KR.DFR.LEV)`,
    date = DATE
  ) |> 
  # Collapse by month
  group_by(month) |> 
  summarise(
    dfr = mean(dfr)
  ) |> 
  ungroup()

# Merge
df_dfr <- month |> 
  left_join(df_dfr, by = c("month")) |> 
  mutate(dfr = nafill(dfr, type = "locf") / 100)

## 05.3  SAVE ------------------------------------------------------------------

# Merge to final Policy Rate Dataset
df_ecb_policy_rate <- df_mpr |> 
  full_join(df_dfr, by = "month")

# Save
SAVE(dfx = df_ecb_policy_rate, namex = paste0(MAINNAME, "_policy_rates"))


# 06. Monthly Dataset ==========================================================

# Merge Monthly dataset
df_ecb_monthly <- df_bsi_a22 |> 
  full_join(df_bsi_l20, by = c("country", "month")) |> # Deposit & Liabilities
  full_join(df_bsi_l60, by = c("country", "month")) |> # Capital & Reserves
  full_join(df_bsi_t00, by = c("country", "month")) |> # Total Assets / Liabilities
  full_join(df_bsi_a20, by = c("country", "month")) |> # Loan Volume 
  full_join(df_bsi_l21, by = c("country", "month")) |> # Deposit Volume
  full_join(df_bsi_a42, by = c("country", "month")) |> # MMF Volume
  full_join(df_bsi_a30, by = c("country", "month")) |> # Debt Volume - Measure for Bond Volume
  full_join(df_deposit_rate, by = c("country", "month")) |> # Overnight Deposit Rate 
  full_join(df_mfi, by = c("country", "month")) # Mortgage Lending Rate

# SAVE
SAVE(dfx = df_ecb_monthly, namex = paste0(MAINNAME, "_m"))

# 07. Annual Dataset ===========================================================

# Annual Dataset
df_ecb_annual <- df_ssi

# SAVE
SAVE(dfx = df_ecb_annual, namex = paste0(MAINNAME, "_a"))



###############################################################################+
################################# ENDE ########################################+
###############################################################################+