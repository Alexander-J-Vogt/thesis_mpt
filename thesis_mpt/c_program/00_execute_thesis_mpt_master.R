# PROJECT'S FULL NAME

# TARGET (of this R.script): ordered execution of R.script(s) within public file routine
#NO detailed descriptions of data, method, etc. HERE -> done in R.scripts called upon

# CONVENTIONS of file naming, abbreviations:

#R.scripts: 	in general 		-> named like the dataset they produce, i.e., _varcreation_x.R
#PROGRAMS: 	-> CAPITAL LETTERS, automatically called upon by R.script executed

#df dataframe
#l list
#var variable

# BASIC STRUCTURE:

#1) DATA PREP
# create base df
#input: 
#output: 

# create separate dfs for each var type
#input: base df
#output: 

# create analysis df and subsamples
#input: combine var type dfs
#output: xx.rda and subsamples

#2) ANALYSIS
#for each sample: sumstats, regs, figures


################################################################################################################+
# INTRO ####

#clear console
cat("\014")

#clear all globals in memory
rm(list = ls()) #needs to go before user-written functions (not libraries) are loaded
sink()

######################+
# non-automatable globals #####

#for master scriptname and extension #####
library(rstudioapi)
MAINNAME <- rstudioapi::getActiveDocumentContext()$path #returns path+name
MAINNAME <- sub(".*/|^[^/]*$", "", MAINNAME)
MAINNAME <- substr(MAINNAME,1,nchar(MAINNAME)-2) #cut off .R

# paths ####
HOME <- "C:/Users/al8in/R_Projects/thesis_mpt" #here: path to 'projects' dir 
DO <- paste0(HOME,"/thesis_mpt/c_program/") #here: path to folder with R.code

######################+
# launch set-up scripts #####
input <- '00_execute_thesis_mpt_intro_aux.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)
#DEBUG <- T

################################################################################################################+
# MAIN PART ####

# 0. Parameters to run the code #############

## RUN APIs 
RUNAPI <- FALSE

## RUN HMDA IMPORT - Caution: Takes approximately 30-60 mins depending on whether 
#  the Import only or together with sample creation is run.
RUNHMDA <- FALSE

## CREATA ALL GRAPHS
ALLGRAPHS <- TRUE



#############################################+
# 01. Linear Projection Function for Panel Data ####
input <- '70_thesis_mpt_function_lp.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

#############################################+
# 02. Data Analysis for United States ############
#############################################+

#############################################+
## 02.1 read-in + basic editing of raw data: create base df ####

# FRED --------------------------------------+
input <- '05_thesis_mpt_databasics_fred.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# Summary of Deposits -----------------------+
input <- '07_thesis_mpt_databasics_sod.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# GDP --------------------------------------+
input <- '09_thesis_mpt_databasics_us_gdp.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# Bureau of Labor Statistics ---------------+
# CAVEAT: Need for API KEY from BLS
# see: https://www.bls.gov/developers/home.htm
input <- '10_thesis_mpt_databasics_us_ur.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# FFR --------------------------------------+
input <- '11_thesis_mpt_databasics_us_ffr.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# U.S. Census Bureau: Population -----------+
input <- '12_thesis_mpt_databasics_us_pop.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# U.S. Census Bureau: Gazette Files  -------+
input <- '13_thesis_mpt_databasics_us_landarea.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# U.S. Census Bureau: FIPS -----------------+
input <- '14_thesis_mpt_databasics_fips.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# FHFA: House Price Index ------------------+
input <- '15_thesis_mpt_databasics_fhfa.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# SAPIE Program: Median HH Income ----------+
input <- '16_thesis_mpt_databasics_saipe.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# SAPIE Program: Median HH Income ----------+
input <- '17_thesis_mpt_databasics_us_debt_to_income.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# HMDA --------------------------------------+
input <- '19_thesis_mpt_databasics_hmda.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# U.S.Census Bureau: Conneticute Crosswalk Fles -+
input <- '22_thesis_mpt_databasics_ct_crosswalk_file.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# Monetary Shock for U.S. & EA -------------+
input <- '24_thesis_mpt_databasics_monetary_shock.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# Monetary Shock for U.S. & EA -------------+
input <- '24_thesis_mpt_databasics_monetary_shock.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)



#############################################+
## 02.2 variable grouping, selection, creation ####
input <- '25_thesis_mpt_us_varceation_outcome.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- '26_thesis_mpt_us_varcreation_treatment.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- '27_thesis_mpt_us_varcreation_controls.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)



#############################################+
## 02.3 main & subsample creation ####
input <- '28_thesis_mpt_us_varcreation_merge.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- '29_thesis_mpt_us_samplecreation_main.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)


#############################################+
## 02.4 var distribution checks (inherent and coding quality) ####
input <- '60_thesis_mpt_us_analysis_vardistribution.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

#############################################+
## figures ####
input <- '62_thesis_mpt_us_analysis_figures.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

#############################################+
## Local Projection ####
input <- '63_thesis_mpt_us_analysis_local_projection.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)


#############################################+
# 03. Data Analysis for the Euro Area ############
#############################################+

#############################################+
## 03.1 read-in + basic editing of raw data: create base df ####

# Eurostat  --------------------------------+
input <- '03_thesis_mpt_databasics_ecb.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# ECB - BSI | MFI | SSI  -------------------+
input <- '04_thesis_mpt_databasics_eurostat.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

# IMF - Commodity Index ------ -------------+
input <- '06_thesis_mpt_databasics_imf.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)


#############################################+
## 03.2 variable grouping, selection, creation ####
input <- '36_thesis_mpt_eu_varcreation_outcome.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- '35_thesis_mpt_eu_varcreation_treatment.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- '37_thesis_mpt_eu_varcreation_controls.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)


#############################################+
## 03.3 main & subsample creation ####
input <- '38_thesis_mpt_eu_varcreation_merge.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- '39_thesis_mpt_eu_samplecreation_main.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)


#############################################+
## 03.4 var distribution checks (inherent and coding quality) ####
input <- '41_thesis_mpt_eu_analysis_vardistribution'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

#############################################+
## figures ####
input <- '40_thesis_mpt_eu_analysis_figures'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

#############################################+
## Local Projection ####
input <- '42_thesis_mpt_eu_analysis_local_projection.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)


############################### END ###########################################+