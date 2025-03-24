# TARGET: Varcreation for Outcome Variable
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

# 01. Import Data ==============================================================

ecb <- LOAD(dfinput = "03_thesis_mpt_databasics_ecb_m")

# 02. Select Relevant Variables ================================================

# Create quarter variable # select relevant variables
df_ecb <- ecb |> 
  select(country, month, hp_outst_amount_EUR, lending_hp_total_outst_amount,
         lending_hp_over_1_year_outst_amount, lending_hp_over_5_years_outst_amount) |> 
  filter(month >= as.Date("2003-01-01")) |>
  filter(!(country == "GR" & month < as.Date("2001-01-01"))) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & month < as.Date("2007-01-01"))) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & month < as.Date("2009-01-01"))) |> # Filter for years with SK being part of the Eurozone
  mutate(
    country_factor = as.factor(country),
    month_num = as.numeric(month)
    ) |> 
  arrange(country, month)

# 03. Impute missings via missForest ===========================================

# Prepare Dataset for Imputation (no date or character variables)
df_ecb_impute <- df_ecb |> 
  select(-c("country", "month"))

# Impute
df_ecb_imputed <- missForest(df_ecb_impute)
df_ecb_imputed_imp <- df_ecb_imputed$ximp

# Get the data into its original data formats
df_outcome <- df_ecb_imputed_imp |> 
  # month to date format & country to character format
  mutate(
    month = as.Date(month_num),
    country = as.character(country_factor)
  ) |> 
  # Select and arrange variables
  arrange(country, month) |> 
  select(country, month, hp_outst_amount_EUR, lending_hp_total_outst_amount, lending_hp_over_1_year_outst_amount, lending_hp_over_5_years_outst_amount)
  
# 03.1 Check Imputations -------------------------------------------------------

# Values for BE in lending_hp_over_5_years_outst_amount are imputed (before 2006)
# Non-parametrical Imputation by missing Forest package

df_ecb_5years <- df_ecb |> 
  filter(country %in% c("BE", "DE", "FR", "NL")) |> 
  select(month, country, lending_hp_over_5_years_outst_amount ) |> 
  rename(interest_rate_5years = lending_hp_over_5_years_outst_amount )

df_outcome_5years <- df_outcome |> 
  filter(country %in% c("BE", "DE", "FR", "NL")) |>
  select(country, month, lending_hp_over_5_years_outst_amount) |> 
  rename(interest_rate_5years_imp = lending_hp_over_5_years_outst_amount)

df_combined <- df_outcome_5years |> 
  full_join(df_ecb_5years, by = c("country", "month"))

# Imputed Variables
ggplot(df_combined, aes(x = month, y = interest_rate_5years_imp, color = country)) +
  geom_point() + 
  geom_line(data = df_combined %>% filter(!is.na(interest_rate_5years_imp))) +  # Connect only non-missing values
  labs(title = "Interest rate on 5 years lending",
       x = "Month", 
       y = "Interest Rate") +
  theme_minimal()

# Non-Imputed Variables
ggplot(df_combined, aes(x = month, y = interest_rate_5years, color = country)) +
  geom_point() + 
  geom_line(data = df_combined %>% filter(!is.na(interest_rate_5years))) +  # Connect only non-missing values
  labs(title = "Interest rate on 5 years lending",
       x = "Month", 
       y = "Interest Rate") +
  theme_minimal()

# Values for GR in lending_hp_total_outst_amount are imputed
# High spike in imputed interest rates seem to be reasonable compared ES and IT
# which are more comparable than the Northern Eurzone Countries

df_ecb_total <- df_ecb |> 
  filter(country %in% c("GR", "DE", "FR", "NL", "IT", "ES")) |> 
  select(month, country, lending_hp_total_outst_amount ) |> 
  rename(interest_rate_total = lending_hp_total_outst_amount )

df_outcome_total <- df_outcome |> 
  filter(country %in% c("GR", "DE", "FR", "NL", "IT", "ES")) |>
  select(country, month, lending_hp_total_outst_amount) |> 
  rename(interest_rate_total_imp = lending_hp_total_outst_amount)

df_combined_total <- df_outcome_total |> 
  full_join(df_ecb_total, by = c("country", "month"))

# Imputed Variables
ggplot(df_combined_total, aes(x = month, y = interest_rate_total_imp, color = country)) +
  geom_point() + 
  geom_line(data = df_combined_total %>% filter(!is.na(interest_rate_total_imp))) +  # Connect only non-missing values
  labs(title = "Interest rate on total lending | Not Imputed",
       x = "Month", 
       y = "Interest Rate") +
  theme_minimal()

# Non-Imputed Variables
ggplot(df_combined_total, aes(x = month, y = interest_rate_total, color = country)) +
  geom_point() + 
  geom_line(data = df_combined_total %>% filter(!is.na(interest_rate_total))) +  # Connect only non-missing values
  labs(title = "Interest rate on total lending | Not Imputed",
       x = "Month", 
       y = "Interest Rate") +
  theme_minimal()

# 35. Save =====================================================================

SAVE(dfx = df_outcome)


################################# ENDE ########################################+
###############################################################################+