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
    GSS_target = sum(GSS_target),
    GSS_path = sum(GSS_path),
    NS = sum(NS)
  )
  

## 1.2 Jarocinski ==============================================================

# Import Jarocinski's measure on monetary shock (available as one sd shocj and 1 bps shock)
df_jarocinski_bp <- read_csv(
  file = paste0(A, "t_policy_shocks_us/Jarocinski_monetary_shock/U1bp.csv")
)

df_jarocinski_sd <- read_csv(
  file = paste0(A, "t_policy_shocks_us/Jarocinski_monetary_shock/U1s.csv")
)

# BPS: Create a yearly monetary shock measure by taking the mean by year
df_jarocinski_bp <- df_jarocinski_bp |> 
  mutate(
    date = date(Time),
    year = year(Time)
    ) |>
  rename_with(~ paste0("bp_", .), .cols = 2:5) |> 
  group_by(year) |> 
  summarise(
    bp_u1 = sum(bp_u1),
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
    sd_u1 = sum(sd_u1),
    sd_u2 = sum(sd_u2),
    sd_u3 = sum(sd_u3),
    sd_u4 = sum(sd_u4)
  )

# Save U.S. monetary shocks
df_us_shock <- df_us_hf_year |> 
  left_join(df_jarocinski_bp, by = c("year")) |> 
  left_join(df_jarocinski_sd, by = c("year"))

# Save
SAVE(dfx = df_us_shock, namex = paste0(MAINNAME, "_us"))


# 2. Import Eurzone Measure for Monetary Policy Shoc ===========================

# 2.1 Altavilla et al. (2019) (with package from Martin Baumgärtner) -----------

# devtools::install_github("https://github.com/martinbaumgaertner/hfdshocks.git")

# Load Implementation of Altavilla et al. (2019) by Baumgärtner 
library(hfdshocks)

# Let the main function do the estimation
df_eurozone_shock <- ecb_shocks(
  "https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx",
  path = paste0(A, "t_policy_shocks_us/Altavilla_monetary_shock/"),
  exclude_dates=c("2001-09-17","2008-10-08","2008-11-06"),
  date_range=c("2000-01-01","2023-12-12"),
  crisis_date="2008-09-04",
  reproduce = TRUE,
  return_data_type = "ois"
  )

# Get the measure for the coventional monetary shock
df_eurozone_target <- data.frame(df_eurozone_shock$factors$release)

df_eurozone <- df_eurozone_target |> 
  mutate(
    target = as.numeric(format(target, scientific = FALSE)),
    year = as.numeric(year(date))
    ) |> 
  group_by(year) |> 
  summarise(
    target = sum(target)
  )

# Get the measure for unconventional monetary shock
df_eurozone_unconv_mp <- data.frame(df_eurozone_shock$factors$conference)

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
    target = sum(target)
  )

# Create monthly row 
df_month <- tibble(month = seq(as.Date("2000-01-01"), as.Date("2023-12-01"), by = "month"))

# Create monthly df with monetary shocks of 0 if there was no press release
df_eurozone <- df_month |> 
  left_join(df_eurozone, by = c("month")) |> 
  mutate(Altavilla_target = if_else(is.na(target), 0, target)) |> 
  dplyr::select(-target)


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
  dplyr::select(-year)

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