# TARGET: Import Landarea of each county
# INDATA: Gaz_counties_national.txt
# OUTDATA/ OUTPUT: mp_transmission_databasics_gaz

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

# 1. Land Area of Counties =====================================================

# List all decennial datasets
gazette <- list.files(path = paste0(A, "j_us_census_bureau/"), pattern = ".txt")
gazette_loop <- gazette[str_detect(gazette, pattern = "Gaz")]
gazette_2000 <- gazette[!str_detect(gazette, pattern = "Gaz")]

## Decennial Census: 2000 ----
fwf_widths <-  c(2, 2, 3, 64, 9, 9, 14, 14, 12, 12, 10, 11)

# Import
landarea_2000 <- read_fwf(
  file = paste0(A,"j_us_census_bureau/", gazette_2000),
  col_positions = fwf_widths(fwf_widths)
)

# Rename columns
col_names <- c("USPS", "state_code", "county_code", "NAME1", "pop", "housing_units", "landarea_sqm", "AWATER", "landarea_smi", "AWATER_SQMI", "INTPTLAT", "INTPTLONG")
colnames(landarea_2000) <- col_names

# Create FIPS
landarea_2000 <- FIPSCREATOR(landarea_2000, state_col = "state_code", county_col = "county_code" )

# Create sqkm + year
landarea_2000 <- landarea_2000 |> 
  select(fips, landarea_sqm, landarea_smi) |>
  mutate(landarea_sqkm = landarea_sqm / 1000000) |> 
  mutate(year = 2000, .before = fips)


## Decennial Census: 2010 ----
fwf_widths <-  c(2, 2, 3, 64, 9, 9, 14, 14, 12, 12, 10, 11)

# Import
landarea_2010 <- read_fwf(
  file = paste0(A,"j_us_census_bureau/", gazette[2]),
  col_positions = fwf_widths(fwf_widths)
)

# Rename columns
colnames(landarea_2010) <- col_names

# FIPS
landarea_2010 <- FIPSCREATOR(landarea_2010, state_col = "state_code", county_col = "county_code" )

# Create sqkm + year
landarea_2010 <- landarea_2010 |> 
  select(fips, landarea_sqm, landarea_smi) |>
  mutate(landarea_sqkm = landarea_sqm / 1000000) |> 
  mutate(year = 2010, .before = fips)

# Decennial Census: 2020

# Import
landarea_2020 <- fread(
  input = paste0(A, "j_us_census_bureau/", gazette[1]), 
  data.table = TRUE,
  colClasses = "character"
)

# Create sqkm + year
landarea_2020 <- landarea_2020 |> 
  select(GEOID, ALAND, ALAND_SQMI) |> 
  rename(
    fips = GEOID,
    landarea_sqm = ALAND,
    landarea_smi = ALAND_SQMI
  ) |> 
  mutate(across(-1, as.numeric)) |> 
  mutate(landarea_sqkm = landarea_sqm / 1000000) |> 
  mutate(year = 2020, .before = fips)

# Create Master File with all three different years
df_gazette <- bind_rows(landarea_2000, landarea_2010)
df_gazette <- bind_rows(df_gazette, landarea_2020)


# Create a dataset on gazette files on annual-county level
list_gazette <- purrr::map(2004:2023, function(x) {
  
  if (x %in% 2004:2009) {
    
    data <- df_gazette |> 
      filter(year == 2000) |> 
      mutate(year = x)
    
  } else if (x %in% 2010:2019) {
    
    data <- df_gazette |> 
      filter(year == 2010) |> 
      mutate(year = x)
    
  } else if (x > 2019) {
    
    data <- df_gazette |> 
      filter(year == 2020) |> 
      mutate(year = x)
  }
  
  return(data)
  
})


data_gazette <- bind_rows(list_gazette)


# 2. Save ======================================================================

SAVE(dfx = data_gazette, namex = MAINNAME)


############################ ENDE #############################################+