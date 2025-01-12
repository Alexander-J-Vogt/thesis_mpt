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

# 1. Lar Data More Precise Inforatm

DEBUG <- T
if(DEBUG) stop("Do not run import code!")
##  1.1 List Panel and Loan Application Records Files (LRA) --------------------   

# list all files for after the reform
lar_files <- list.files(paste0(A, "p_hmda_lra/"), pattern = "lar{1,100}")
lar_files <- lar_files[!str_detect(lar_files, ".zip")]
lar_files <- lar_files[as.integer(str_replace_all(lar_files, "[^0-9]", "")) > 2017]

# Select relevant variables
lar_columns <- c("activity_year", "lei", "derived_msa_md", "county_code", "action_taken", "derived_dwelling_category",
                 "purchaser_type", "loan_type", "loan_purpose", "lien_status", "reverse_mortgage", 
                 "open_end_line_of_credit", "business_or_commercial_purpose", "loan_amount",
                 "interest_rate", "rate_spread", "hoepa_status", "total_loan_costs", "total_points_and_fees",
                 "origination_charges", "lender_credits", "loan_term", "negative_amortization", "interest_only_payment",
                 "balloon_payment", "property_value", "occupancy_type", "total_units", "applicant_age", 
                 "income", "debt_to_income_ratio", "applicant_credit_score_type", "applicant_race_1", "applicant_sex")

# 
purrr::walk(seq_along(lar_files), function(file){
  
  # Determine file
  file <- lar_files[file]
  
  # Determine year of file
  year <- as.integer(str_replace_all(file, "[^0-9]", ""))
  
  # Add the right LTV var
  lar_columns_dyn <- if (year > 2018) c(lar_columns, "combined_loan_to_value_ratio") else c(lar_columns, "loan_to_value_ratio")
  
  r <- Inf
  
  # Update message
  message(VISUALSEP)
  message(paste0("START TO IMPORT DATA FOR THE YEAR ", year, ".\n"))
  message("\nStart actual import...")
  # Load data
  data <- fread(
    paste0(A, "p_hmda_lra/", file),
    colClasses = "character",
    select = lar_columns_dyn,
    nrows = r,
    data.table = TRUE,
    nThread = 4,
    showProgress = FALSE
  )
  
  # Update message
  message("Start to do basic filtering.")
  
  # Select only originated loans
  data <- data[action_taken == 1]
  
  # One-to-Four Single Family
  data <- data[derived_dwelling_category %in% c("Single Family (1-4 Units):Site-Built", "Single Family (1-4 Units):Manufactured")]
  
  # First Mortgage Lien (No second mortgage on a house - interest rate are higher on these)
  data <- data[lien_status == 1]
  
  # Delete not used variables
  data[, `:=`(
    action_taken = NULL,
    lien_status = NULL
    )]
  
  # Save
  SAVE(dfx = data, namex = paste0("hmda_reform_", year))
  
  # Save sample
  frac <- 0.01
  data_sample <- data[, .SD[sample(.N, size = max(1, .N * frac))], by = county_code]
  SAVE(dfx = data_sample, namex = paste0("hmda_reform_sample_", year))
  
  # Update message
  message("End of Import Process.\n")
  
  rm(data)
  gc()
  
})





# Basic Data Cleaning

library(dlookr)
library(flextable)
library(var)

data <- LOAD(dfinput = "hmda_reform_2018")
setDT(data)

col_names_old <- colnames(data)
chr_cols <- c("lei", "derived_msa_md", "county_code", "derived_dwelling_category","applicant_age", "debt_to_income_ratio")
num_cols <- setdiff(col_names_old, chr_cols)

data[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
data[, log_loan_amount := log(loan_amount)]
data[, total_points_and_fees := NULL]
data <- data[!is.na(county_code)]
data <- data[loan_term == 360]




# purchaser type
data |> 
  filter(purchaser_type < 10 ) |> 
  ggplot(aes(x = purchaser_type)) +
    geom_bar() +
  scale_x_continuous(
    limits = c(-1, 10),
    breaks = seq(0, 10, by = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 2000000),
    breaks = seq(0, 2000000, by = 100000)
  )

# loan type
data |> 
  ggplot(aes(x = loan_type, fill = as.factor(loan_type))) +
  geom_bar() +
  # scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(
    limits = c(0, 5000000),
    breaks = seq(0, 5000000, by = 500000)
  )

# loan purpose
data |> 
  mutate(
    loan_purpose = if_else(loan_purpose %in% c(31, 32), 3, loan_purpose)
  ) |> 
  ggplot(aes(x = loan_purpose, fill = as.factor(loan_purpose))) +
  geom_bar() +
  scale_y_continuous(
    limits = c(0, 4500000),
    breaks = seq(0, 5000000, by = 500000)
  )

# reverse mortgage
data |> 
  mutate(
    reverse_mortgage = if_else(reverse_mortgage == 1111, 3, reverse_mortgage)
  ) |> 
  ggplot(aes(x = reverse_mortgage, fill = as.factor(reverse_mortgage))) +
  geom_bar() +
  # scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(
    limits = c(0, 6500000),
    breaks = seq(0, 6500000, by = 500000)
  )

# Open-End Line of Credit
data |> 
 mutate(
   open_end_line_of_credit = if_else(open_end_line_of_credit == 1111, 3, open_end_line_of_credit)
 ) |> 
 ggplot(aes(x = open_end_line_of_credit, fill = as.factor(open_end_line_of_credit))) +
 geom_bar() +
# scale_fill_brewer(palette = "Set3") +
scale_y_continuous(
 limits = c(0, 6000000),
 breaks = seq(0, 6000000, by = 500000)
)

describe(data) |> flextable()
# Business or Commercial Purpose
data |> 
  mutate(
    business_or_commercial_purpose = if_else(business_or_commercial_purpose == 1111, 3, business_or_commercial_purpose)
  ) |> 
  ggplot(aes(x = business_or_commercial_purpose, fill = as.factor(business_or_commercial_purpose))) +
  geom_bar() +
  # scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(
    limits = c(0, 6500000),
    breaks = seq(0, 6000000, by = 500000)
  )

# Interest Rate
data |> 
  filter(interest_rate > 8.5) |> 
  ggplot(aes(x = interest_rate)) +
  geom_histogram()

# HOEPA status
data |> 
  ggplot(aes(hoepa_status)) +
  geom_bar()

# Negative Amoritazation
data |> 
  mutate(negative_amortization = if_else(negative_amortization == 1111, 3, negative_amortization)) |> 
  ggplot(aes(negative_amortization)) +
  geom_bar()

# interest only payment
data |> 
  mutate(interest_only_payment = if_else(interest_only_payment == 1111, 3, interest_only_payment)) |> 
  ggplot(aes(interest_only_payment)) +
  geom_bar()
  

# ballon payment
data |> 
  # mutate(balloon_payment = if_else(balloon_payment == 1111, 3, balloon_payment)) |> 
  ggplot(aes(balloon_payment)) +
  geom_bar()

# occupany type
data |> 
  ggplot(aes(occupancy_type)) +
  geom_bar()

table(data$occupancy_type)

#applicant age
data |> 
  filter(!applicant_age %in% c("8888", "9999")) |> 
  mutate(applicant_age = as.factor(applicant_age)) |> 
  ggplot(aes(applicant_age)) +
  geom_bar()


data |> 
  filter(debt_to_income_ratio != "Exempt") |> 
  mutate(debt_to_income_ratio = as.factor(debt_to_income_ratio)) |> 
  ggplot(aes(debt_to_income_ratio)) +
  geom_bar()

describe(data, interest_rate)

data |> 
  filter(interest_rate > 10) |> 
  arrange(interest_rate) |> 
  describe()

# loan_term
data |> 
  ggplot(aes(loan_term)) +
  geom_histogram()

# loan ammount

data <- data |> 
  mutate(
  loan_amount_mean = imputate_outlier(data, loan_amount, method = "mean", no_attrs = T),
  loan_amount_median = imputate_outlier(data, loan_amount, method = "median", no_attrs = T),
  loan_amount_mode = imputate_outlier(data, loan_amount, method = "mode", no_attrs = T),
  loan_amount_capping = imputate_outlier(data, loan_amount, method = "capping", no_attrs = T)
)

data <- data |> 
  mutate(
    log_loan_amount_mean = log(loan_amount_mean),
    log_loan_amount_median = log(loan_amount_median),
    log_loan_amount_mode = log(loan_amount_mode),
    log_loan_amount_capping = log(loan_amount_capping)
  )

data |> 
  # mutate(log_loan_amount = log(loan_amount)) |> 
  ggplot(aes(x = loan_amount_capping)) +
  geom_density()

plot(imputate_outlier(data, loan_amount, method = "mean"))

plot(imputate_outlier(data, loan_amount, method = "median"))

plot(imputate_outlier(data, loan_amount, method = "mode"))

plot(imputate_outlier(data, loan_amount, method = "capping"))

iqr <- IQR(data$loan_amount)
q3 <- quantile(data$loan_amount, probs = .75)
q1 <- quantile(data$loan_amount, probs = .25)

data_without_outlier <- data |> 
  filter(loan_amount > q3[[1]] + 1.5 * iqr & loan_amount > q1[[1]] + 1.5 * iqr 
  ) 

data_outliers <- data |> 
  filter(loan_amount > q3[[1]] + 1.5 * iqr) |> 
  arrange(loan_amount)

data_test <- data |> 
  filter(loan_amount < 100000000)

data_without_outlier |> 
  ggplot(aes(loan_amount)) +
  geom_density()

diagnose(data) |> flextable()

diagnose_outlier(data_outliers) |> flextable()
plot_outlier(data, loan_amount)

plot_na_intersect(data)

diagnose_report(data)

norm_check <- data |> 
  group_by(county_code) |> 
  normality(log_loan_amount) |> 
  mutate(normality_fulfilled = p_value > .05)

data |> 
  plot_normality(loan_amount)

ggplot(data, aes(loan_to_value_ratio)) +
  geom_density()


data |> 
  filter(loan_to_value_ratio > 103) |> 
  arrange(loan_to_value_ratio) |> 
  select(county_code, loan_amount, property_value, loan_to_value_ratio) |> 
  View()

data |> 
  arrange(loan_to_value_ratio) |> 
  select(county_code, loan_amount, property_value, loan_to_value_ratio) |> 
  View()
  

data_ltv <- data |> 
  select(lei, county_code, loan_amount, property_value, loan_to_value_ratio) |> 
  mutate(ltv_own = round(loan_amount / property_value * 100, digits = 4)) |> 
  mutate(compare_ltv = loan_to_value_ratio / ltv_own,
         identical = ifelse(loan_to_value_ratio > 0.95 & loan_to_value_ratio < 1.05, TRUE, FALSE)) |> 
  mutate(squared_ltv = (loan_to_value_ratio)^2)

ggplot(data, aes(loan_to_value_ratio)) +
  geom_density()

data |> 
  filter(loan_to_value_ratio < 103) |> 
  ggplot(aes(loan_to_value_ratio)) +
  geom_density()


############################### END ###########################################+