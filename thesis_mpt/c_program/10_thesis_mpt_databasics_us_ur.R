# TARGET: Import Unemployment Rate data
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
  dplyr::select(-state_code)



# Create series ids for BLS 
fips_master <- data.frame(fips = unique(df_gazette$fips))
fips_master <- fips_master |> 
  mutate(series_id = paste0("LAUCN", fips, "0000000003"))

# Add data
new_fips <- c("02105", "02195", "02198", "02230", "02270", "02275", 
              "15005", "46113", "51515", "09110", "09120", "09130", 
              "09140", "09150", "09160", "09170", "09180", "09190")

fips_master <- c(fips_master, new_fips)


# Define a function to fetch unemployment rate using the BLS API (version 2)
fetch_unemployment_rate_v2 <- function(series_ids, start_year = 2004, end_year = 2023, api_key) {
  
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

# Download BLS API
if (interactive()) {
  # Define parameters
  api_key <- BLS_API_KEY  # Replace with your actual BLS API key
  
  # Create a vector of series IDs for 3000+ counties
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


# 1.2 UR - US ------------------------------------------------------------------

ur_country <- fread(
  file = paste0(A, "h_fred/UNRATE.csv"),
  colClasses = "character"
)

# avg by year
df_ur_country <-ur_country |> 
  mutate(
    year = year(observation_date),
    UNRATE = as.numeric(UNRATE)) |> 
  group_by(year) |> 
  summarise(
    ur_national = mean(UNRATE, na.rm = T)
  ) |> 
  ungroup()


## 1.3 Clean & Collapse Data ---------------------------------------------------

# Get saved unemployment rate
unemployment_data <- LOAD(dfinput = "bls_api_ur")

# Clean and Collapse Data 
df_ur <- unemployment_data |> 
  dplyr::mutate(
    fips = str_sub(seriesID, 6, 10), # Retrieve fips
    value = as.integer(value), # format values
    year = as.numeric(year)
    ) |> 
  dplyr::select(year, fips, value, period) |> 
  group_by(year, fips) |> # Collapse data to annual-county level (from month-county level)
  summarise(ur_county = mean(value, na.rm = TRUE), .groups = "drop")  |> # Rem. half of the years monthly ur in 2005 and 2006 are missing for the counties: 22051, 22071, 22075, 22087, 22089, 22095, 22071
  ungroup() |> 
  full_join(df_ur_country, by = "year")


## 1.4 SAVE --------------------------------------------------------------------

# SAVE
SAVE(dfx = df_ur)

############################## END ############################################+