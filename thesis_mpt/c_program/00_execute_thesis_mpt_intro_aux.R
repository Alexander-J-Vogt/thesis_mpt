# pooling set-up scripts

##############################################   
# project-specific globals ####

#more paths ####
ADO <-  paste0(HOME,"/project_00generalproject/00_r_userwritten/") #here: path to subfolder with all userwritten routines like function scripts, etc.
#
A <- paste0(HOME,"/thesis_mpt/a_microdata/") #here: path to folder with data
TEMP <- paste0(A, "temp",sep="") # path to subfolder with intermediate output, optionally reset/ deleted/created anew
#
D <- paste0(HOME,"/thesis_mpt/d_results/") #here: path to folder with output (tables, figures, logs)
GEO <- D
FIGURE <- D
LATEX <- D
LOG <- D

# Scripts
S <- paste0(HOME, "/thesis_mpt/c_program/")

#shortcuts to central datasets ####
DFBASE <- "thesis_mpt_databasics"
DFMERGE <- "thesis_mpt_varcreation_merge"
DFMAIN <- "thesis_mpt_samplecreation_main"

#miscellaneous ####
DATEORIGIN <- "1970-01-01"
DEBUG <- F
if(DEBUG){
  print("We are in debug mode")
}
LANGUAGE <- "English_United States.1252" #set language (weekday labels,..)
PRINTMAX <- 10000 # options()$max.print + Inf  #or add something larger
options("max.print" = PRINTMAX) #nrows of e.g. head(df) to be printed (not for tibbles)
options("width"=PRINTMAX) #ncols max dto. [circumvents terminal but capped by monitor screen widths]
options(tibble.print_max = PRINTMAX) #tibble nrows 
options(tibble.width = PRINTMAX) #tibble ncols 
PROJECTNAME <- 'thesis_mp'
PROJECT <- paste0(HOME,"projects/",PROJECTNAME,"/") #direct path to directory of specific project
SEED <- 1234567
STARNOTE <- "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"
TIMEZONE <- "Europe/Berlin" #set time zone for environment
VISUALSEP <- "*********************************************"


##############################################   
# launch basic preparation ####
#load programs,...

########################
# ensure max RAM allocation to R
memory.limit(size=32000) #guessed for 32 RAM

########################
#ensure in mainfolder there exists a subfolder Temp 
#NOT emptied here in case it already exists, resetting is optional 
dir.create(paste0(A,"temp",sep="/")) #just a warning if folder already exists

########################
# load index of programs (packages / libraries + myfunctions)
source(paste0(ADO,"00_PROGRAMS.R"), echo=TRUE, max=1000)  

########################
# date and time settings
#could be overridden by ind scripts
Sys.setenv(TZ=TIMEZONE) #to NOT depend on local time zone where script is executed
#Sys.timezone()

#change labels to US EN 
#no impact on time zone!
Sys.setlocale(category = "LC_ALL", locale = LANGUAGE)
#weekdays(Sys.Date()+1:7)

########################
# Load Passwords
name <- list.files(path = S, pattern = "passwords")
if (!is.na(name)) source(paste0(S, name), echo = TRUE, max = 1000)

########################
#save session info for backward compatibility
DATE <- format(Sys.time(),format='%Y%m%d') #needed to append to logfile at the end of master dofile
sink(paste0(D,"/",MAINNAME,"_","sessionInfo","_",DATE), split = TRUE) #log on
sessionInfo()
sink()
