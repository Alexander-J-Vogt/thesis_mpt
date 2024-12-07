# TARGET: Import QWI 
# INDATA: qwi_e14db0de913c427aa12de971a73eb389.csv
# OUTDATA/ OUTPUT: mp_transmission_databasics_qwi

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

# 01. Import of SAIPE ==========================================================
# Small Area Income and Poverty Estimates (SAIPE)

files <- list.files(paste0(A, "l_saipe/"))
files_xls <- files[str_detect(files, ".xls$")]
files_txt <- files[str_detect(files, ".txt$")]


# Import .text files -----------------------------------------------------------
file_txt_1 <- files_txt[1:3]
widths <- c(3, 4, 9, 9, 9, 5, 5, 5, 9, 9, 9, 5, 5, 5, 9, 9, 9, 5, 5, 5, 7, 7, 7, 8, 8, 8, 5, 5, 5, 45, 4)
for (i in file_txt_1) {
  # Extract year
  year <- str_sub(i, start = 4, end = 5)
  
  # Import Data
  assign(
    paste0("data_", year),
    read_fwf(
      file = paste0(A, "l_saipe/", i),
      col_positions = fwf_widths(widths)
    )
  )
  
  # Create year variable
  assign(
    paste0("data_", i),
    mutate(get(paste0("data_", year)), year = as.numeric(paste0("20", year)))
  )
}

## Import .txt files: 1999 and 1999 --------------------------------------------
file_txt_2 <- files_txt[4:length(files_txt)]
for (i in file_txt_2) {
  # Extract year
  year <- str_sub(i, start = 4, end = 5)
  
  # Import Data
  assign(
    paste0("data_", year),
    read_fwf(
      file = paste0(A, "l_saipe/", i),
      col_positions = fwf_widths(widths),
      col_types = cols(.default = "c")
    )
  )
  
  # Create year variable
  assign(
    paste0("data_", year),
    mutate(get(paste0("data_", year)), year = as.numeric(paste0("19", year)))
  )
}
                 
## Import .xls files: 2003 to 2004 ---------------------------------------------
files_xls_1 <- files_xls[1:2]
for (i in files_xls_1) {
  
  # Extract year
  year <- str_sub(i, start = 4, end = 5)
  
  # Import Data
  assign(
    paste0("data_", year),
    read_xls(
      path = paste0(A, "l_saipe/", i),
      skip = 1,
      col_types = "text"
      )
  )
  
  # Create year variable
  assign(
    paste0("data_", year),
    mutate(get(paste0("data_", year)), year = as.numeric(paste0("20", year)))
  )
}


## Import .xls files: 2005 to 2012 ---------------------------------------------

files_xls_2 <- files_xls[3:10]
for (i in files_xls_2) {
  
  # Extract year
  year <- str_sub(i, start = 4, end = 5)
  
  # Import Data
  assign(
    paste0("data_", year),
    read_xls(
      path = paste0(A, "l_saipe/", i),
      skip = 2,
      col_types = "text"
    )
  )
  
  # Create year variable
  assign(
    paste0("data_", year),
    mutate(get(paste0("data_", year)), year = as.numeric(paste0("20", year)))
  )
}


## Import .xls files: 2005 to 2012 ---------------------------------------------

files_xls_3 <- files_xls[11:length(files_xls)]
for (i in files_xls_3) {
  
  # Extract year
  year <- str_sub(i, start = 4, end = 5)
  
  # Import Data
  assign(
    paste0("data_", year),
    read_xls(
      path = paste0(A, "l_saipe/", i),
      skip = 3,
      col_types = "text"
    )
  )
  
  # Create year variable
  assign(
    paste0("data_", year),
    mutate(get(paste0("data_", year)), year = as.numeric(paste0("20", year)))
  )
}


# 02. Create Dataset on Poverty and Median Houehold Income =====================
