# TARGET: Reading-In all Raw Datasets  & Perform basic data cleaning
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

# 1. Import crosswalk files ====================================================

df_crosswalk_2000 <- read_xlsx(
  path = paste0(A, "s_crosswalk_files/qcew-county-msa-csa-crosswalk.xlsx"),
  sheet = "Dec. 2003 Crosswalk",
  col_types = "text"
)


df_crosswalk_2010 <- read_xlsx(
  path = paste0(A, "s_crosswalk_files/qcew-county-msa-csa-crosswalk.xlsx"),
  sheet = "Feb. 2013 Crosswalk",
  col_types = "text"
)

df_crosswalk_2020 <- read_xlsx(
  path = paste0(A, "s_crosswalk_files/qcew-county-msa-csa-crosswalk.xlsx"),
  sheet = "Jul. 2023 Crosswalk",
  col_types = "text"
)

# 2. Create Main Dataset =======================================================

# Basic Cleaning
list_crosswalk <- purrr::map(c("df_crosswalk_2000", "df_crosswalk_2010", "df_crosswalk_2020"), function(x) {
  
  # Retrieve Object
  data <- get(x)
  
  # Clean col names
  names <- colnames(data)
  names <- str_replace_all(names, " ", "_")
  colnames(data) <- names
  
  # Basic Manipulation
  data_m <- data |>
    select(1:3, 5) |> 
    rename(
      fips = County_Code,
      fips_name = County_Title,
      msa = MSA_Code,
      csa = CSA_Code
    ) |> 
    separate(fips_name, c("fips_name", "state_name"), sep = ", ", fill = "right") |> # separate into county nad state name
    mutate(year = str_replace_all(x, "[^0-9]", ""), .before = fips) # introduce year var
  
  return(data_m)
  
})

df_crosswalk <- bind_rows(list_crosswalk)

# Create a dataset on msa-county crosswalk files on annual-county level
list_crosswalk_annual <- purrr::map(2004:2023, function(x) {
  
  if (x %in% 2004:2009) {
    
    data <- df_crosswalk |> 
      filter(year == 2000) |> 
      mutate(year = x)
    
  } else if (x %in% 2010:2019) {
    
    data <- df_crosswalk |> 
      filter(year == 2010) |> 
      mutate(year = x)
    
  } else if (x > 2019) {
    
    data <- df_crosswalk |> 
      filter(year == 2020) |> 
      mutate(year = x)
  }
  
  return(data)
  
})

df_crosswalk_annual <- bind_rows(list_crosswalk_annual)
df_crosswalk_annual <- df_crosswalk_annual |> 
  select(-csa) |> 
  mutate(msa = str_sub(msa, 2, nchar(msa))) |> 
  mutate(msa = paste(msa, "0", sep = ""))

# SAVE
SAVE(df_crosswalk_annual)

################################## END ########################################+

