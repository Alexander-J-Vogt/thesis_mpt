# TARGET: Import IMF data on Commodity Indexs by country
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

# 01. Commodity Net Export Index ===============================================

# List all files
files <- list.files(paste0(A, "e_imf/"))
files <- files[str_detect(files, pattern = "\\.xlsx$")]

# Import data
for ( i in seq_along(files) ) {
  assign(
    paste0("data_", i), 
    read_excel(
      path = paste0(A, "e_imf/", files[i]),
      sheet = "Data",
      range = cell_rows(1:2),
      col_types = "text"   
      )
    )
}

# 02. Basic Data Cleaning ======================================================

# Reshape data into wide format
envir <- ls()
envir <- envir[str_detect(envir, "data_")]

for (i in envir) {
  
  # Retrieve object
  data <- get(i)
  
  # Get name of country
  country <- str_to_lower(data[[1]])
  
  # Prepare data for reshaping to long format
  data <- data |> 
    rename_with(~ paste0("month_", .), -1)
  
  # Create dataset "imf_" + "countryname"
  assign(
    paste0("imf_", country),
    pivot_longer(
      data,
      cols = starts_with("month_"),
      names_to = "month",
      values_to = "index"
    )
  )       
}

# 03. Append data on different countries =======================================

# List all datasets with imf at the beginning
list <- ls()
imf <- list[str_detect(list, "imf")]

# Create main raw dataset by appending datasets
df_imf_raw <- list()
for (i in seq_along(imf)) {
  df_imf_raw[[i]] <- get(imf[i])
}

df_imf_raw <- bind_rows(df_imf_raw)
colnames(df_imf_raw)[1] <- "country"

# Basic data cleaning
df_imf <- df_imf_raw |> 
  # Create date variable
  mutate(
    year = str_sub(month, start = 11),
    month_3 = str_sub(month, start = 7, end = 9)
    ) |> 
  mutate(
   month_nr = case_when(
    month_3 == "Jan" ~ "01",
    month_3 == "Feb" ~ "02",
    month_3 == "Mar" ~ "03",
    month_3 == "Apr" ~ "04",
    month_3 == "May" ~ "05",
    month_3 == "Jun" ~ "06",
    month_3 == "Jul" ~ "07",
    month_3 == "Aug" ~ "08",
    month_3 == "Sep" ~ "09",
    month_3 == "Oct" ~ "10",
    month_3 == "Nov" ~ "11",
    month_3 == "Dec" ~ "12"
  )) |> 
  mutate(month = as.Date(paste0(year, "-", month_nr, "-01"))) |> 
  # Create Standardized country variable
  mutate(
    country = case_when(
      country == "Austria" ~ "AT",
      country == "Belgium" ~ "BE",
      country == "Finland" ~ "FI",
      country == "France" ~ "FR",
      country == "Germany" ~ "DE",
      country == "Greece" ~ "GR",
      country == "Ireland" ~ "IE",
      country == "Italy" ~ "IT",
      country == "Netherlands" ~ "NL",
      country == "Portugal" ~ "PT",
      country == "Spain" ~ "ES",
      country == "Slovenia" ~ "SI",
      country == "Slovak Republic" ~ "SK"
    )
  ) |> 
  # Format to numeric
  mutate(commodity_index = as.numeric(index)) |> 
  select(country, month, commodity_index)

# 04. Index = 2015 =============================================================

# Create Index ny calculating the mean over the year 2015 
df_index <- df_imf |> 
  filter(year(month) == 2015) |> 
  group_by(country) |> 
  mutate(mean_index = mean(commodity_index)) |> 
  ungroup() |> 
  select(country, mean_index) |> 
  distinct(country, mean_index)

# Index the variable from the year 2012 to 2015
df_imf <- df_imf |> 
  left_join(df_index, by = c("country")) |> 
  mutate(commodity_index = commodity_index / mean_index * 100) |> 
  select(country, month, commodity_index)

# 04. SAVE =====================================================================

# Save
SAVE(dfx = df_imf)


###############################################################################+
################################# ENDE ########################################+
###############################################################################+