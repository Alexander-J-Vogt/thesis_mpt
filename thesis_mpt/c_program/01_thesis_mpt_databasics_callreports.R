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

# List all files in a_callreports
file_names <- list.files(paste0(A, "a_callreports"))

file_names <- file_names[1:4]

# Create empty list
df_callreports <- list()

data <- read_delim(paste0(A, "a_callreports/", file_names[1]), col_types = cols(.default = col_character()))
data1 <- read_delim(paste0(A, "a_callreports/", file_names[2]), col_types = cols(.default = col_character()))

data2 <- read_delim(paste0(A, "a_callreports/", file_names[3]), col_types = cols(.default = col_character()))
data3 <- read_delim(paste0(A, "a_callreports/", file_names[4]), col_types = cols(.default = col_character()))


for (i in seq_along(file_names)) {
  
  i <- 1
  # Choose file name 
  files <- file_names[i]
  
  # Read in file
  data <- read_delim(paste0(A, "a_callreports/", files))
  
  df_callreports[i] <- data
  
  # Print
  print(paste0("Finished loading: ", files))
}
