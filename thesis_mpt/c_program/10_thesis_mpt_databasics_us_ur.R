# TARGET: Import Unemployment Rate data
# INDATA: Unemployment.xlsx
# OUTDATA/ OUTPUT: mp_transmission_databasics_ur

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

# 1. Import Unemployment Data ==================================================

# # Import unemployment data on county level from the economic research service
# unemp_data <- read_xlsx(paste0(A, "i_economic_research_service/", "Unemployment.xlsx"), skip = 4, col_types = "text")
# setDT(unemp_data)
# 
# # Extract Unemployment Rates for all counties
# unemp_data <- EXTRACTVAR(data = unemp_data, state_col = "State", county_col = "FIPS_Code", indicator = "Unemployment_rate")
# 
# # Standardize variable names
# colnames(unemp_data) <- str_to_lower(names(unemp_data))
# setnames(unemp_data, old = c("fips_code"), new = c("fips"))
# 
# # Shift data from wide to long format
# unemp_data <- melt(
#   unemp_data,
#   measure.vars = patterns("unemployment_rate_"),
#   variable.name = "year",
#   value.name = "ur"
# )
# 
# # Clean the year variable
# unemp_data <- unemp_data[, year := sub("unemployment_rate_", "", year)]
# 
# # Delete observation for states and US Totals
# unemp_data <- unemp_data[fips != "00000"]
# unemp_data <- unemp_data[
#   substr(get("fips"), nchar(get("fips")) - 2, nchar(get("fips"))) != "000", 
#   .(fips, state, year, ur)
# ]
# 
# # format variables
# unemp_data <- unemp_data[, year := as.integer(year)]
# unemp_data <- unemp_data[, ur := round(as.double(ur), 2)]
# 
# # Select relevant variable
# unemp_data <- unemp_data[, c("fips", "year", "ur")]
# 
# # Save
# SAVE(dfx = unemp_data, namex = MAINNAME)


# 1.1 API Call for monthly UR --------------------------------------------------

# Get a full list of counties in order to retrieve data
df_gazette <- LOAD(dfinput = "13_thesis_mpt_databasics_us_landarea")
df_gazette <- df_gazette |> 
  mutate(state_code = str_sub(fips, 1, 2)) |> 
  filter(state_code %in%  c(
    "01", "02", "04", "05", "06", "08", "09", "10", "12", "13",
    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
    "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
    "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
    "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "11"
  )) |> 
  select(-state_code)
fips_master <- data.frame(fips = unique(df_gazette$fips))

# Create series ids for BLS 
fips_master <- fips_master |> 
  mutate(series_id = paste0("LAUCN", fips, "0000000003"))

# Add data
new_fips <- c("02105", "02195", "02198", "02230", "02270", "02275", 
              "15005", "46113", "51515", "09110", "09120", "09130", 
              "09140", "09150", "09160", "09170", "09180", "09190")

fips_master <- c(fips_master, new_fips)


# Define a function to fetch unemployment rate using the BLS API (version 2)
fetch_unemployment_rate_v2 <- function(series_ids, start_year = 2004, end_year = 2023, api_key) {
  # """
  # Fetch unemployment rate data using the BLS API (version 2).
  # 
  # Parameters:
  #     series_ids (vector): A vector of BLS series IDs for unemployment rates.
  #     start_year (int): The starting year for the data range.
  #     end_year (int): The ending year for the data range.
  #     api_key (str): Your BLS API key.
  # 
  # Returns:
  #     DataFrame: A DataFrame (tibble) containing the unemployment rates.
  # """
  tryCatch({
    # Split series IDs into batches of 50 (BLS API limit)
    batches <- split(series_ids, ceiling(seq_along(series_ids) / 50))
    
    # Initialize an empty tibble to store results
    results <- tibble()
    
    # Loop through batches to handle requests efficiently
    for (i in seq_along(batches)) {
      batch <- batches[[i]]
      
      # Construct API request payload
      payload <- list(
        seriesid = batch,
        startyear = start_year,
        endyear = end_year,
        registrationkey = api_key
      )
      
      # Make API request and directly retrieve a data frame
      df <- blsAPI(payload, api_version = 2, return_data_frame = TRUE)
      
      # Append to results
      results <- bind_rows(results, df)
      
      # Pause to respect API rate limits
      if (i %% 5 == 0) {
        Sys.sleep(10)  # Pause for 10 seconds every 50 requests
      } else {
        Sys.sleep(2)  # Pause for 2 seconds between smaller batches
      }
      
      # Update Message
      message(paste0("Finished with iteration: ", i, " out of ", length(batches), "."))
    }
    
    return(results)
    
  }, error = function(e) {
    message("Error fetching unemployment rate data: ", e$message)
    return(NULL)
  })
}

# Example usage
if (interactive()) {
  # Define parameters
  api_key <- BLS_API_KEY  # Replace with your actual BLS API key
  
  # Example: Create a vector of series IDs for 3000+ counties (use actual series IDs for your case)
  series_ids <- fips_master$series_id  # Replace with actual county series IDs
  
  # Fetch data for years 2004 to 2023
  unemployment_data <- fetch_unemployment_rate_v2(series_ids, api_key = api_key)
  
  # Check if data was successfully fetched
  if (!is.null(unemployment_data)) {
    print(head(unemployment_data))
  } else {
    message("Failed to fetch unemployment rate data.")
  }
}

# SAVE raw API data
SAVE(dfx = unemployment_data, namex = "bls_api_ur")

## 1.2 Clean & Collapse Data ---------------------------------------------------

# Get saved unemployment rate
unemployment_rate <- LOAD(dfinput = "bls_api_ur")

# Clean and Collapse Data 
df_ur <- unemployment_data |> 
  mutate(
    fips = str_sub(seriesID, 6, 10), # Retrieve fips
    value = as.integer(value) # format values
    ) |> 
  select(year, fips, value, period) |> 
  group_by(year, fips) |> # Collapse data to annual-county level (from month-county level)
  summarise(ur = mean(value, na.rm = TRUE), .groups = "drop") # Rem. half of the years monthly ur in 2005 and 2006 are missing for the counties: 22051, 22071, 22075, 22087, 22089, 22095, 22071

# SAVE
SAVE(dfx = df_ur)

############################## END ############################################+