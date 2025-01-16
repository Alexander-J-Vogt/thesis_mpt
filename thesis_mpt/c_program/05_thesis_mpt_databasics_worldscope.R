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

# 01. Importing bank-level data on European Banks ==============================
df_wroldscope_raw <- read_delim((paste0(A, "d_worldscope/xtguamoeiysdhecn.txt")), col_types =  cols(.default = "c"))

# Rename variables for clarification, standardize country code, create date variable
df_worldscope_raw <- df_wroldscope_raw |> 
  rename(
    total_assets = ITEM2999,
    total_liabilities = ITEM3351,
    wrldscp_id = ITEM6035,
    company = ITEM6001,
    industy_clarification = ITEM6010,
    nation = ITEM6026,
    nation_code = ITEM6027,
    region = ITEM6028,
    tr_business_code = ITEM7041
         ) |> 
  mutate(country = case_when(
    nation_code == "40" ~ "AT",
    nation_code == "56" ~ "BE",
    nation_code == "246" ~ "FI",
    nation_code == "250" ~ "FR",
    nation_code == "280" ~ "DE",
    nation_code == "300" ~ "GR",
    nation_code == "372" ~ "IE",
    nation_code == "380" ~ "IT",
    nation_code == "528" ~ "NL",
    nation_code == "620" ~ "PT",
    nation_code == "724" ~ "ES",
    nation_code == "705" ~ "SI",
    nation_code == "703" ~ "SK"
  )) |>   
  mutate(quarter = yq(paste0(year_, "-", freq, seq))) |> 
  select(country, quarter, code, company, total_assets, total_liabilities, tr_business_code) |> 
  mutate(
    total_assets = as.numeric(total_assets),
    total_liabilities = as.numeric(total_liabilities)
    )

# 02. Basic Data Cleaning ======================================================

# Filter for the right banks: Banks, Corporate Banks, Retail & Mortgage Banks
df_worldscope <- df_worldscope_raw |> 
  filter(str_detect(tr_business_code, "55101010")) |>  # Banks, Corporate Banks, Retail & Mortgage Banks & Private Banks
  # filter(!str_detect(tr_business_code, "14")) |> # Exclude Private Banks as they are not participating in the mortgage market
  arrange(country, quarter)

# 03. Save =====================================================================

SAVE(dfx = df_worldscope)

################################ END ##########################################+