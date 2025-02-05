# TARGET: Expansing LP Function based on Adämmer's lpirfs package
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
# MAIN PART ####


# Test Data ----

library(lpirfs)
# library(TS)
library(urca)
library(panelvar)
library(CADFtest)
library(pcse)


# 1. Import Datasets ===========================================================

# Load: Home Purchase for large sample
df_hp_depository_large <- LOAD("29_thesis_mpt_us_samplecreation_main_hp_large")

# Load: Refinancing for large sample
df_ref_depository_large <- LOAD("29_thesis_mpt_us_samplecreation_main_ref_large")

# # Load: Home Purchase for small sample
# df_hp_depository_small <- LOAD("29_thesis_mpt_us_samplecreation_main_hp_small")
# 
# # Load: Refinancing for small sample
# df_ref_depository_small <- LOAD("29_thesis_mpt_us_samplecreation_main_ref_small")


# 2. Time Series ===============================================================

# 2.1 Home Purchase Large Sample -----------------------------------------------

df_hp_large <- df_hp_depository_large |> 
  # filter(cnty_pop.x > 65000) |>
  # Adjust Values of the function
  mutate( 
    log_median_household_income = log(median_household_income),
    poverty_percent_all_ages = poverty_percent_all_ages / 100,
    hpi_annual_change_perc = hpi_annual_change_perc / 100,
    inflation_us = inflation_us / 100,
    gdp_growth_us = gdp_growth_us / 100,
    ur = ur / 100
  ) |> 
  # Interaction Terms
  mutate(
    I_HHI_ZLB2_NS = d_ffr_mean_2perc * hhi * NS_target,
    I_HHI_ZLB1_NS = d_ffr_mean_1perc * hhi * NS_target,
    I_HHI_NS = hhi * NS_target,
    I_hhi_2perc = d_ffr_mean_2perc * hhi,
    I_hhi_NS = hhi * NS_target,
    I_2perc_NS = d_ffr_mean_2perc * NS_target
  )


df_hp_large_lp <- df_hp_large |> 
  # filter(d_hhi_indicator) |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")

# LP_LIN_PANEL Function -----

# DEBUGGING ?
DEBUG <- TRUE

LP_LIN_PANEL <- function(
    data_set          = NULL,  # Panel dataset
    data_sample       = "Full",  # Use full sample or subset
    endog_data        = NULL,  # Endogenous variable
    cumul_mult        = TRUE,  # Estimate cumulative multipliers?
    shock             = NULL,  # Shock variable
    diff_shock        = TRUE,  # First difference of shock variable
    panel_model       = "within",  # Panel model type
    panel_effect      = "individual",  # Panel effect type
    robust_cov        = NULL,  # Robust covariance estimation method
    c_exog_data       = NULL,  # Contemporaneous exogenous variables
    l_exog_data       = NULL,  # Lagged exogenous variables
    lags_exog_data    = NaN,  # Lag length for exogenous variables
    c_fd_exog_data    = NULL,  # First-difference contemporaneous exogenous variables
    l_fd_exog_data    = NULL,  # First-difference lagged exogenous variables
    lags_fd_exog_data = NaN,  # Lag length for first-difference exogenous variables
    confint           = NULL,  # Confidence interval width
    hor               = NULL){  # Number of horizons
  
  if (DEBUG) {
  data_set <- df_hp_large_lp
  data_sample <- "Full"
  endog_data <- "log_loan_amount"
  cumul_mult <-  TRUE
  shock <-  "I_HHI_NS"
  diff_shock <-  TRUE
  panel_model <-  "within"
  panel_effect <-  "twoways"
  robust_cov <-  "vcovSCC"
  c_exog_data <-  colnames(df_hp_large_lp)[c(4, 6:10)]
  c_fd_exog_data <- NULL
  l_exog_data <-  colnames(df_hp_large_lp)[c(4:10)] 
  l_fd_exog_data <- NULL
  lags_exog_data <-  1
  lags_fd_exog_data <- NULL
  confint <- 1.67
  hor <- 6
  }
  # Check if dataset is provided
  if(is.null(data_set)){
    stop("You have to provide the panel data set.")
  }
  
  # Check if endogenous variable is provided
  if(is.null(endog_data)){
    stop("You have to provide the name of the endogenous variable.")
  }
  
  # Check if shock variable is provided
  if(is.null(shock)){
    stop("You have to provide the name of the variable to shock with.")
  }
  
  # Check if panel model type is valid
  if(!panel_model %in% c("within", "random", "ht", "between", "pooling", "fd")){
    stop("Invalid panel model type.")
  }
  
  # Check if panel effect specification is valid
  if(!panel_effect %in% c("individual", "time", "twoways", "nested")){
    stop("Invalid panel effect specification.")
  }
  
  # Check if confidence interval width is valid
  if(is.null(confint) || !(confint >= 0)){
    stop("Specify a valid width for the confidence bands (>=0).")
  }
  
  # Check if the number of horizons is a positive integer
  if(!(hor > 0) | is.nan(hor) | !(hor %% 1 == 0)){
    stop("Number of horizons must be a positive integer.")
  }
  
  # Rename first two columns to standard identifiers
  colnames(data_set)[1] <- "cross_id"
  colnames(data_set)[2] <- "date_id"
  
  # Sort dataset by cross-section ID and time
  data_set <- dplyr::arrange(data_set, cross_id, date_id)
  
  # Store specifications in a list
  specs <- list(
    data_sample = data_sample,
    endog_data = endog_data,
    cumul_mult = cumul_mult,
    shock = shock,
    diff_shock = diff_shock,
    panel_model = panel_model,
    panel_effect = panel_effect,
    robust_cov = robust_cov,
    c_exog_data = c_exog_data,
    l_exog_data = l_exog_data,
    lags_exog_data = lags_exog_data,
    c_fd_exog_data = c_fd_exog_data,
    l_fd_exog_data = l_fd_exog_data,
    lags_fd_exog_data = lags_fd_exog_data,
    confint = confint,
    hor = hor,
    exog_data = colnames(data_set)[which(!colnames(data_set) %in% c("cross_id", "date_id"))],
    model_type = 2,
    is_nl = FALSE
  )
  
  # Create panel data structure
  lin_panel_data <- create_panel_data(specs, data_set)
  specs <- lin_panel_data$specs
  x_reg_data <- lin_panel_data$x_reg_data
  y_data <- lin_panel_data$y_data
  
  # Initialize matrices to store impulse response results
  irf_panel_mean <- matrix(NaN, 1, specs$hor)
  irf_panel_up <- matrix(NaN, 1, specs$hor)
  irf_panel_low <- matrix(NaN, 1, specs$hor)
  reg_outputs <- list(rep(NaN, specs$hor))
  reg_summaries <- list(rep(NaN, specs$hor))
  xy_data_sets <- list(rep(NaN, specs$hor))
  
  # Prepare regression formula
  y_reg_name <- specs$endog_data
  x_reg_names <- names(x_reg_data)
  ols_formula <- paste(y_reg_name, "~", paste(x_reg_names[!(x_reg_names %in% c("cross_id", "date_id"))], collapse = " + "))
  plm_formula <- stats::as.formula(ols_formula)
  
  # Loop over horizons to estimate impulse responses
  for(ii in 1:specs$hor){
    
    if (DEBUG) ii <- 1
    # Merge dependent and independent variables
    yx_data <- dplyr::left_join(y_data[[ii]], x_reg_data, by = c("cross_id", "date_id")) %>% stats::na.omit()
    
    # Filter dataset if subset specified
    if(!(specs$data_sample[1] == 'Full')){
      yx_data <- dplyr::filter(yx_data, date_id %in% specs$data_sample)
    }
    
    # Estimate panel regression
    panel_results <- plm::plm(formula = plm_formula, data = yx_data, index = c("cross_id", "date_id"), model = specs$panel_model, effect = specs$panel_effect)
    reg_output_tmp <- panel_results
    reg_summary_tmp <- summary(panel_results)
    
    # Extract shock coefficient
    shock_position <- which(stats::variable.names(t(reg_summary_tmp$coef)) == specs$shock)
    if(is.integer(shock_position) && length(shock_position) == 0){
      stop("Shock variable dropped during estimation. IRFs cannot be estimated.")
    }
    
    # Compute impulse responses and confidence bands
    irf_panel_mean[[1, ii]] <- reg_summary_tmp$coefficients[shock_position, 1]
    irf_panel_up[[1, ii]] <- reg_summary_tmp$coefficients[shock_position, 1] + specs$confint * reg_summary_tmp$coefficients[shock_position, 2]
    irf_panel_low[[1, ii]] <- reg_summary_tmp$coefficients[shock_position, 1] - specs$confint * reg_summary_tmp$coefficients[shock_position, 2]
    
    # Store results
    reg_outputs[[ii]] <- reg_output_tmp
    reg_summaries[[ii]] <- reg_summary_tmp
    xy_data_sets[[ii]] <- yx_data
  }
  
  # Return results as list
  result <- list(
    irf_panel_mean = irf_panel_mean,
    irf_panel_low = irf_panel_low,
    irf_panel_up = irf_panel_up,
    reg_outputs = reg_outputs,
    reg_summaries = reg_summaries,
    xy_data_sets = xy_data_sets,
    y_data = y_data,
    specs = specs
  )
  
  # Assign class to output
  class(result) <- "lpirfs_lin_panel_obj"
  return(result)
}