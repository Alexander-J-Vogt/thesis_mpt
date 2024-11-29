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
  mutate(month = paste0(month, "-01"))

  


list <- df_mfi |> 
  distinct(country, month, currency)


sapply(colnames(df_mfi)[-c(1, 13, 33, 34)], function(x){
  table(df_mfi[[x]])
})
  

table(df_mfi$BS_ITEM, df_mfi$MATURITY_NOT_IRATE)
table(df_mfi$BS_ITEM, df_mfi$DATA_TYPE_MIR) 
table(df_mfi$BS_ITEM, df_mfi$CURRENCY_TRANS)

table(df_mfi$BS_ITEM, df_mfi$COLLECTION)
table(df_mfi$BS_ITEM, df_mfi$OBS_PRE_BREAK) 
table(df_mfi$BS_ITEM, df_mfi$OBS_STATUS)
table(df_mfi$BS_ITEM, df_mfi$OBS_CONF)


table(df_mfi$item_descr, df_mfi$IR_BUS_COV)
table(df_mfi$item_descr, df_mfi$BS_COUNT_SECTOR) 
table(df_mfi$item_descr, df_mfi$CURRENCY_TRANS) 

# check
df_mfi |> 
  dplyr::group_by(!!!syms(colnames(df_mfi))) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) |> 
  # ungroup() |> 
  head()

df_mfi |> 
  group_by(across(-DATA_TYPE_MIR)) |> 
  filter(n_distinct(DATA_TYPE_MIR) > 1) |> 
  ungroup()

df_mfi |> 
  summarise(across(-c(1:4), ~ sum(is.na(.)), .names = "missing_{col}")) |> 
  pivot_longer(everything(), names_to = "Column", values_to = "Missing_Count")


###############################################################################+
################################# ENDE ########################################+
###############################################################################+