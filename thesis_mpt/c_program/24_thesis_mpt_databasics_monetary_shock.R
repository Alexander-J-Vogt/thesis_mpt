# TARGET: Importing Monetary Shocks Datasets
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
# MAIN ###

# 1. Import U.S. Monetary Shock ================================================

## 1.1 Import Nakamura & Steinsson + GSS ----------------------------------------

# Import NS + GSS measures for monetary shocks by Acost et al. (2024) by SOFR Futures
df_us_hf <- read_excel(
  path = paste0(A, "t_policy_shocks_us/NS_GSS_monetary_shock/ABJ-2024-monetary-policy-surprises.xlsx"),
  sheet = "Data"
)

# Create a yearly monetary shock measure by taking the mean by year
df_us_hf_year <- df_us_hf |> 
  mutate(
    date = date(date),
    year = year(date)
  ) |> 
  group_by(year) |> 
  summarise(
    GSS_target_total = sum(GSS_target, na.rm = TRUE),
    GSS_target_negativ = sum(GSS_target[GSS_target  < 0], na.rm = TRUE),
    GSS_target_positiv = sum(GSS_target[GSS_target  > 0], na.rm = TRUE),
    GSS_path = sum(GSS_path),
    NS_total = sum(NS),
    NS_total_mean = mean(NS, na.rm = TRUE),
    NS_negativ = sum(NS[NS  < 0], na.rm = TRUE),
    NS_positiv = sum(NS[NS  > 0], na.rm = TRUE),
  )
  

## 1.2 Jarocinski (2024) - Unconvetionaö ==============================================================

# Import Jarocinski's measure on monetary shock (available as one sd shocj and 1 bps shock)
df_jarocinski_bp <- read_csv(
  file = paste0(A, "t_policy_shocks_us/Jarocinski_monetary_shock/Jarocinski_2024_Unconventional/U1bp.csv")
)

df_jarocinski_sd <- read_csv(
  file = paste0(A, "t_policy_shocks_us/Jarocinski_monetary_shock/Jarocinski_2024_Unconventional/U1s.csv")
)

#' Explanation:
#' Standard Monetary Policy: u1
#' Odyssean forward guidance: u2
#' Long term rate shock: u3
#' Delphic forward guidance: u4


# BPS: Create a yearly monetary shock measure by taking the mean by year
df_jarocinski_bp <- df_jarocinski_bp |> 
  mutate(
    date = date(Time),
    year = year(Time)
    ) |>
  rename_with(~ paste0("bp_", .), .cols = 2:5) |> 
  group_by(year) |> 
  summarise(
    bp_u1_total = sum(bp_u1), #
    bp_u1_positiv = sum(bp_u1[bp_u1 > 0], na.rm = TRUE),
    bp_u1_negativ = sum(bp_u1[bp_u1 < 0], na.rm = TRUE),
    bp_u2 = sum(bp_u2),
    bp_u3 = sum(bp_u3),
    bp_u4 = sum(bp_u4)
  )
  
# SD: Create a yearly monetary shock measure by taking the mean by year
df_jarocinski_sd <- df_jarocinski_sd |> 
  mutate(
    date = date(Time),
    year = year(Time)
  ) |>
  rename_with(~ paste0("sd_", .), .cols = 2:5) |> 
  group_by(year) |> 
  summarise(
    sd_u1_total = sum(sd_u1), #
    sd_u1_positiv = sum(sd_u1[sd_u1 > 0], na.rm = TRUE),
    sd_u1_negativ = sum(sd_u1[sd_u1 < 0], na.rm = TRUE),
    sd_u2 = sum(sd_u2),
    sd_u3 = sum(sd_u3),
    sd_u4 = sum(sd_u4)
  )


## 1.3 Jarocinski & Karadi (2020) - Conventional MS ============================

# Import 
ms_jk <- read_csv(
  file = paste0(A, "t_policy_shocks_us/Jarocinski_monetary_shock/Jarosinki_Karadi_2020_Conventional/shocks_fed_jk_m.csv"),
  col_types = cols(.default = "c")
)

df_ms_jk <- ms_jk |> 
  # Create month variable
  mutate(month = if_else(nchar(month) == 1, str_pad(month, width = 2, side = "left", pad = "0"), month)) |> 
  mutate(month = as.Date(paste0(year, "-", month, "-01"))) |> 
  # Format shock variable
  mutate(across(3:ncol(ms_jk), as.numeric)) |> 
  dplyr::select(-year) |> 
  group_by(month) |> 
  mutate(
    MP_median_total = MP_median,
    MP_median_positiv = sum(MP_median[MP_median > 0], na.rm = TRUE),
    MP_median_negativ = sum(MP_median[MP_median < 0], na.rm = TRUE),
  ) |> 
  dplyr::select(month, starts_with("MP_median")) |> 
  mutate(year = year(month), .after = month)

df_ms_jk_annual <- df_ms_jk |> 
  group_by(year) |> 
  mutate(
    MP_median_sum = sum(MP_median_total),
    MP_median_mean = mean(MP_median_total)
  ) |> 
  dplyr::select(year, MP_median_sum, MP_median_mean)

results <- lm(MP_median_total ~ lag(MP_median_total), data = df_ms_jk)
summary(results)


results <- lm(MP_median_mean ~ lag(MP_median_mean), data = df_ms_jk_annual)
summary(results)

results <- lm(MP_median_sum ~ lag(MP_median_sum), data = df_ms_jk_annual)
summary(results)


## 1.4 Save US Monetary Shocks =================================================

# Save Annual U.S. monetary shocks
df_us_shock <- df_us_hf_year |>
  left_join(df_ms_jk_annual, by = "year") |> 
  left_join(df_jarocinski_bp, by = c("year")) |> 
  left_join(df_jarocinski_sd, by = c("year")) |> 
  dplyr::select(-c("sd_u2", "sd_u3", "sd_u4", "bp_u2", "bp_u3", "bp_u4", "GSS_path")) |> 
  distinct(year, .keep_all = TRUE)

# Save
SAVE(dfx = df_us_shock, namex = paste0(MAINNAME, "_us"))

# Save Monthly U.S. Monetary Shocks
df_us_hf_month <- df_us_hf |> 
  mutate(month = date(date)) |> 
  dplyr::select(-date) |> 
  mutate(month = as.character(month)) |> 
  mutate(month = paste0(str_sub(month, 1, 7), "-01")) |> 
  mutate(month = as.Date(month))

df_us_shock_monthly <- df_ms_jk |> 
  left_join(df_us_hf_month, by = "month") |> 
  mutate(across(everything(), ~ if_else(is.na(.x), 0, .x)))

# Save
SAVE(dfx = df_us_shock_monthly, namex = paste0(MAINNAME, "_us_monthly"))


# 2. Import Eur zone Measure for Monetary Policy Shock =========================

## 2.1 Altavilla et al. (2019) (with package from Martin Baumgärtner) -----------

# devtools::install_github("https://github.com/martinbaumgaertner/hfdshocks.git")

# Load Implementation of Altavilla et al. (2019) by Baumgärtner 
library(hfdshocks)

# Let the main function do the estimation
df_eurozone_shock <- ecb_shocks(
  "https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx",
  path = paste0(A, "t_policy_shocks_us/Altavilla_monetary_shock/"),
  exclude_dates= c("2001-09-17","2008-10-08","2008-11-06"),
  date_range=c("2000-01-01","2023-12-12"),
  crisis_date="2008-09-04",
  reproduce = TRUE,
  return_data_type = "ois"
  )

# Get the measure for the conventional monetary shock
df_eurozone_target <- data.frame(df_eurozone_shock$factors$release)

# df_eurozone <- df_eurozone_target |> 
#   mutate(
#     target = as.numeric(format(target, scientific = FALSE)),
#     year = as.numeric(year(date))
#     ) |> 
#   group_by(year) |> 
#   summarise(
#     target = sum(target)
#   )
# 
# # Get the measure for unconventional monetary shock
# df_eurozone_unconv_mp <- data.frame(df_eurozone_shock$factors$conference)

# Data Manipulating
df_eurozone <- df_eurozone_target |> 
  # Format variables
  mutate(
    target = as.numeric(format(target, scientific = FALSE)),
    month = as.Date(paste0(str_sub(date, 1, 7), "-01"))
  ) |> 
  # Sum up by month
  group_by(month) |> 
  summarise(
    altavilla_total = sum(target, na.rm = TRUE),
    altavilla_positiv = sum(target[target > 0], na.rm = TRUE),
    altavilla_negativ = sum(target[target < 0], na.rm = TRUE)
    )

# Create monthly row 
df_month <- tibble(month = seq(as.Date("2000-01-01"), as.Date("2023-12-01"), by = "month"))

# Create monthly df with monetary shocks of 0 if there was no press release
df_eurozone <- df_month |> 
  left_join(df_eurozone, by = c("month")) |> 
  mutate(
    altavilla_total = if_else(is.na(altavilla_total), 0, altavilla_total),
    altavilla_positiv = if_else(is.na(altavilla_positiv), 0, altavilla_positiv),
    altavilla_negativ = if_else(is.na(altavilla_negativ), 0, altavilla_negativ)
    )


# 2.2 Jarocinski & Karadi (2020) -----------------------------------------------

# Reproduced in Matlab with the replication files available on:
# https://github.com/marekjarocinski/jkshocks_update_ecb_202310

# Import data
jarocinski <- read_csv(
  file = paste0(A, "t_policy_shocks_us/Jarocinski_eu/shocks_ecb_mpd_me_m.csv"),
  col_types = cols(.default = "c")
  )

# Creat month variable & format variables
df_jarocinski_eu <- jarocinski |> 
  # Create month variable
  mutate(month = if_else(nchar(month) == 1, str_pad(month, width = 2, side = "left", pad = "0"), month)) |> 
  mutate(month = as.Date(paste0(year, "-", month, "-01"))) |> 
  # Format shock variable
  mutate(across(3:ncol(jarocinski), as.numeric)) |> 
  dplyr::select(-year) |> 
  group_by(month) |> 
  mutate(
    MP_median_total = MP_median,
    MP_median_positiv = sum(MP_median[MP_median > 0], na.rm = TRUE),
    MP_median_negativ = sum(MP_median[MP_median < 0], na.rm = TRUE),
  ) |> 
  dplyr::select(month, starts_with("MP_median"))

# Add last two month to df
df_jarocinski_eu <- df_month |> 
  left_join(df_jarocinski_eu, by = "month") |> 
  mutate(across(-1, ~ if_else(is.na(.), 0, .)))


# 2.3 Merge Eurozone Monetary Shocks --------------------------------------------

# Merge
df_eurozone_shock <- df_jarocinski_eu |> 
  left_join(df_eurozone, by = "month")

# 3. Save ====================================================================== 

SAVE(dfx = df_eurozone_shock, namex = paste0(MAINNAME, "_eurozone"))



############################### END ###########################################+