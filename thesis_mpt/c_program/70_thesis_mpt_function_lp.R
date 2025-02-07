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
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_ZLB2_NS", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")

# LP_LIN_PANEL Function -----

# DEBUGGING ?
DEBUG <- FALSE

LP_LIN_PANEL <- function(
    data_set          = NULL,  # Panel dataset
    data_sample       = "Full",  # Use full sample or subset
    endog_data        = NULL,  # Endogenous variable
    cumul_mult        = TRUE,  # Estimate cumulative multipliers?
    shock             = NULL,  # Shock variable
    diff_shock        = TRUE,  # First difference of shock variable
    panel_model       = "within",  # Panel model type
    panel_effect      = "twoway",  # Panel effect type
    robust_cov        = NULL,  # Robust covariance estimation method
    robust_cluster    = NULL,
    c_exog_data       = NULL,  # Contemporaneous exogenous variables
    l_exog_data       = NULL,  # Lagged exogenous variables
    lags_exog_data    = NaN,  # Lag length for exogenous variables
    c_fd_exog_data    = NULL,  # First-difference contemporaneous exogenous variables
    l_fd_exog_data    = NULL,  # First-difference lagged exogenous variables
    lags_fd_exog_data = NaN,  # Lag length for first-difference exogenous variables
    confint           = NULL,  # Confidence interval width
    hor               = NULL,
    biter             = NULL
    ){  # Number of horizons
  
  DEBUG <- FALSE
  if (DEBUG) {
  data_set <- df_hp_large_lp
  data_sample <- "Full"
  endog_data <- "log_loan_amount"
  cumul_mult <-  TRUE
  shock <-  "I_HHI_ZLB2_NS"
  diff_shock <-  TRUE
  panel_model <-  "within"
  panel_effect <-  "twoways"
  robust_cov <-  "wild.cluster.boot"
  robust_method <- NULL
  robust_type <- NULL
  robust_cluster <- "group"
  c_exog_data <-  NULL #colnames(df_hp_large_lp)[c(4, 6:10)]
  l_exog_data <-  NULL #colnames(df_hp_large_lp)[c(4:10)] 
  c_fd_exog_data <- colnames(df_hp_large_lp)[c(4, 6:10)]
  l_fd_exog_data <- colnames(df_hp_large_lp)[c(4:10)]
  lags_exog_data <-  NULL
  lags_fd_exog_data <- 1
  confint <- 1.65
  hor <- 6
  biter <- 10
  }
  
  
  ## Check Data on compatability ---
  
  # Check if dataset is provided
  if (is.null(data_set)) {
    stop("You have to provide the panel data set.")
  }
  
  # Check if endogenous variable is provided
  if (is.null(endog_data)) {
    stop("You have to provide the name of the endogenous variable.")
  }
  
  # Check if shock variable is provided
  if (is.null(shock)) {
    stop("You have to provide the name of the variable to shock with.")
  }
  
  # Check if panel model type is valid
  if (!panel_model %in% c("within", "random", "ht", "between", "pooling", "fd")) {
    stop("Invalid panel model type.")
  }
  
  # Check if panel effect specification is valid
  if (!panel_effect %in% c("individual", "time", "twoways", "nested")) {
    stop("Invalid panel effect specification.")
  }
  
  # Check if confidence interval width is valid
  if (is.null(confint) || !(confint >= 0)) {
    stop("Specify a valid width for the confidence bands (>=0).")
  }
  
  # Check if the number of horizons is a positive integer
  if (!(hor > 0) | is.nan(hor) | !(hor %% 1 == 0)) {
    stop("Number of horizons must be a positive integer.")
  }
  
  # Check if Wild.Cluster.Boost has a number of iteration
  if (robust_cov == "wild.cluster.boost" & is.null(biter)) {
    stop("Specify the number of iterations.")
  }
  
  # Check if Wild.Cluster.Boost has a defined cluster
  if (robust_cov == "wild.cluster.boost" & (is.null(robust_cluster) | !robust_cluster %in% c("group", "time"))) {
    stop("Specify the right cluster.")
  }
  
  
  # Rename first two columns to standard identifiers
  colnames(data_set)[1] <- "cross_id"
  colnames(data_set)[2] <- "date_id"
  
  # Sort dataset by cross-section ID and time
  data_set <- dplyr::arrange(data_set, cross_id, date_id)
  
  # Store specifications in a list
  specs <- list(
    
    # Basics
    data_sample = data_sample,
    endog_data = endog_data,
    cumul_mult = cumul_mult,
    shock = shock,
    diff_shock = diff_shock,
    
    # Panel Model
    panel_model = panel_model,
    panel_effect = panel_effect,
    
    # Robust
    robust_cov = robust_cov,
    robust_cluster = robust_cluster,
    confint = confint,
    biter = biter,
    model_type = 2,
    
    # Exogenous Vars
    c_exog_data = c_exog_data,
    l_exog_data = l_exog_data,
    lags_exog_data = lags_exog_data,
    c_fd_exog_data = c_fd_exog_data,
    l_fd_exog_data = l_fd_exog_data,
    lags_fd_exog_data = lags_fd_exog_data,
    hor = hor,
    exog_data = colnames(data_set)[which(!colnames(data_set) %in% c("cross_id", "date_id"))],
    
    # Not relevant argument
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
  
  # # Repare lfe regression formula 
  # fe <- "cross_id"
  # fe_formula <- paste(ols_formula, " | ", fe, " | ")
  # felm_formula <- stats::as.formula(fe_formula)
  
  # Loop over horizons to estimate impulse responses
  for(ii in 1:specs$hor){
    
    ## Create Panel Dataset for this Period ---
    
    if (DEBUG) ii <- 1
    # Merge dependent and independent variables
    yx_data <- dplyr::left_join(y_data[[ii]], x_reg_data, by = c("cross_id", "date_id")) %>% stats::na.omit()
    
    # Filter dataset if subset specified
    if(!(specs$data_sample[1] == 'Full')){
      yx_data <- dplyr::filter(yx_data, date_id %in% specs$data_sample)
    }
    
    ## Estimate Regression ---
    
    # # Estimate Panel Regression with lfe-package
    # panel_results_felm <- lfe::felm(formula = felm_formula, data = yx_data)    
    # reg_output_tmp <- panel_results_felm
    
    # Depreciated - Estimate panel regression
    panel_results <- plm::plm(formula = plm_formula, data = yx_data, index = c("cross_id", "date_id"), model = specs$panel_model, effect = specs$panel_effect)
    # reg_output_tmp <- panel_results
    # reg_summary_tmp <- summary(panel_results)
    
    
    ## Calculate Standard Errors ---
    
    # Estimate confidence bands with robust standard errors?
    if(is.character(specs$robust_cov)){
      
      # Estimate robust covariance matrices
      if(specs$robust_cov %in% c("vcovBK", "vcovDC", "vcovHC", "vcovNW", "vcovSCC", "wild.cluster.boot", "tLAHR")){
        
        reg_output_tmp  <- panel_results
        reg_summary_tmp <- GET_ROBUST_COV_PANEL(panel_results, specs)
        
      } else {
        
        reg_output_tmp  <- panel_results
        reg_summary_tmp <-  lmtest::coeftest(panel_results,  vcov = get_robust_vcxt_panel(specs$robust_cov))
        
      }
      
      # Extract the position of the parameters of the shock variable
      shock_position <- which(stats::variable.names(t(reg_summary_tmp)) == specs$shock)
      
      # If shock variable could not be found, stop estimation and give message
      if(is.integer(shock_position) && length(shock_position) == 0){
        stop("The shock variable has been dropped during the estimation. The
                 impulse responses can not be estimated.")
      }
      
      
      
      # Estimate irfs and confidence bands
      irf_panel_mean[[1, ii]]   <- reg_summary_tmp[shock_position, 1]
      irf_panel_up[[1,   ii]]   <- reg_summary_tmp[shock_position, 1] + specs$confint*reg_summary_tmp[shock_position, 2]
      irf_panel_low[[1,  ii]]   <- reg_summary_tmp[shock_position, 1] - specs$confint*reg_summary_tmp[shock_position, 2]
      
    }      else      { 
      
      reg_output_tmp  <- panel_results
      reg_summary_tmp <- summary(panel_results)
      
      # Extract the position of the parameters of the shock variable
      shock_position <- which(stats::variable.names(t(reg_summary_tmp$coef)) == specs$shock)
      
      # If shock variable could not be found, stop estimation and give message
      if(is.integer(shock_position) && length(shock_position) == 0){
        stop("The shock variable was dropped during the estimation, perhaps because of co-linearity or identification issues.
               As a consequence, the  impulse responses can not be estimated.")
      }
      
      # Estimate irfs and confidence bands
      irf_panel_mean[[1, ii]]   <- reg_summary_tmp$coefficients[shock_position, 1]
      irf_panel_up[[1,   ii]]   <- reg_summary_tmp$coefficients[shock_position, 1] + specs$confint*reg_summary_tmp$coefficients[shock_position, 2]
      irf_panel_low[[1,  ii]]   <- reg_summary_tmp$coefficients[shock_position, 1] - specs$confint*reg_summary_tmp$coefficients[shock_position, 2]
      
    }
    
    
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




# Extension for the original "get_robust_cov_panel" function by  ---------------
# Wild Clustered Bootstrap
GET_ROBUST_COV_PANEL <- function(panel_results, specs){
  
  if(specs$robust_cov         == "vcovBK"){
    
    # Panel Corrected Standard Errors after Beck and Katz (1995)
    reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovBK(panel_results,
                                                                       type    = specs$robust_type,
                                                                       cluster = specs$robust_cluster))
    
  } else if (specs$robust_cov == "vcovDC"){
    
    # High-level convenience wrapper for double-clustering robust covariance matrix estimators after Thompson (2011)
    reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovDC(panel_results,
                                                                       type    = specs$robust_type))
    
    
  } else if (specs$robust_cov == "vcovHC"){
    
    # Clustered-Robust Heteroskedastic Standard Errors
    reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovHC(panel_results,
                                                                       method   = specs$robust_method,
                                                                       type     = specs$robust_type,
                                                                       cluster  = specs$robust_cluster))
    
  } else if (specs$robust_cov == "vcovNW"){
    
    # Newey-White Robust Standard Errors
    reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovNW(panel_results,
                                                                       type    = specs$robust_type,
                                                                       maxlag  = specs$robust_maxlag))
  } else if (specs$robust_cov == "vcovSCC"){
    
    # Driscroll-Kraay SE (1998) - Accounts for cross-sectional and time dependence
    reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovSCC(panel_results,
                                                                        type    = specs$robust_type,
                                                                        maxlag  = specs$robust_maxlag))
    
  } else if (specs$robust_cov == "wild.cluster.boot") {
    
    # Determine confidence level
    confint_list <- list("1.96" = 0.95, "1.65" = 0.9, "1" = 0.68)  
    target_confint <- as.character(specs$confint)
    confint_level <- confint_list[[target_confint]]
    
    
    # Apply Wild Cluster Bootstrap to account for the case when N converges to infinity with fixed T
    # Problem: Asymptotic Distribution will be dominated by the cross-sectional dimension, which
    # causes concerns about distortions generated when there are roots near unity.
    # Wild Cluster Bootstrap corrects the heteroskedasticity by using a cluster-robust approach.
    wild_b <- cluster.wild.plm(panel_results,
                               dat = yx_data,
                               cluster = specs$robust_cluster,
                               boot.reps = specs$biter,
                               ci.level = confint_level,
                               report = FALSE
                               )
    
    # Get Coefficient of Shock
    index_shock <- which(stats::variable.names(t(wild_b$ci)) == specs$shock)
    estimate <- coef(panel_results)[which(stats::variable.names(t(panel_results$coefficients)) == specs$shock)]
    
    # Calculate SE
    wild_se <- (wild_b$ci[2, 2] - estimate) / 1.96
    
    # Save in line with coeftest class
    reg_results <- data.frame(estimate = estimate, se = wild_se)
    
  } else if (specs$robust_cov == "tLAHR") {
    
    # Clustered-Robust Heteroskedastic Standard Errors: Clustered for time
    reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovHC(panel_results,
                                                                       type     = "HC0",
                                                                       cluster  = "time")) 
    
  }
  
  return(reg_results)
  
}

## EXTENSION OF CREATE LIN PANEL #### ------------------------------------------

create_panel_data <- function(specs, data_set){
  
  if (DEBUG)
  
  
  # Function ---
  
  # Function to compute first differences
  diff_function <- function(data) {
    
    c(NA, diff(data))
  
  }
  
  # Function to compute cumulative endogenous variables
  cumul_function <- function(data, hor) {
    
    return(dplyr::lead(data, hor) - dplyr::lag(data, 1))
  
  }
  
  # Prepare list for endogenous variables
  y_data <- vector("list", specs$hor)
  
  # Cumulative Multiplier ---
  
  # Create horizons of dependent variables based on whether to use cumulative multipliers
  if(isTRUE(specs$cumul_mult)){
    
    # Loop to create endogenous variables
    for(ii in 0:(specs$hor-1)){
      
      # Create cumulative endogenous vector
      y_data[[ii + 1]]     <-  data_set %>%
        dplyr::select(cross_id, date_id, specs$endog_data) |> 
        dplyr::group_by(cross_id)                          |> 
        dplyr::mutate_at(vars(specs$endog_data),
                         list(~cumul_function(., ii)))     |> 
        dplyr::ungroup()
    }
    
  }    else     {
    
    
    # Loop to create endogenous variables
    for(ii in 0:(specs$hor-1)){
      
      # Create lead endogenous vectors
      y_data[[ii + 1]]     <-  data_set |> 
        dplyr::select(cross_id, date_id, specs$endog_data)  |> 
        dplyr::group_by(cross_id)                  |> 
        dplyr::mutate_at(vars(specs$endog_data ),
                         list(~dplyr::lead(., ii))) |> 
        dplyr::ungroup()
    }
    
  }
  
  # Shock Variable ---
  
  # Prepare shock variable
  x_reg_data <- data_set  |>  
    dplyr::select(cross_id, date_id, specs$shock)
  
  
  # Take first differences of shock variable?
  if(isTRUE(specs$diff_shock)){
    
    x_reg_data    <- x_reg_data                                 |> 
      dplyr::group_by(cross_id)                                 |> 
      dplyr::mutate_at(vars(specs$shock), diff_function)        |> 
      dplyr::rename_at(vars(specs$shock), list(~paste0("d",.))) |> 
      dplyr::ungroup()
    
    # Rename shock variable
    specs$shock   <- colnames(x_reg_data)[which(!(colnames(x_reg_data) %in% c("cross_id", "date_id")))]
    
  }
  
  # Exogenous Data ---
  
  # Prepare exogenous data
  x_data <- data_set |> 
    dplyr::select(cross_id, date_id, specs$exog_data)
  
  # Choose exogeonus data with contemporaneous impact
  if(!is.null(specs$c_exog_data)){
    
    c_x_data      <- x_data   %>%
      dplyr::select(cross_id, date_id, specs$c_exog_data)
    
    # Use lagged contemporaneous data as regressors
    x_reg_data    <- suppressMessages(x_reg_data %>%
                                        dplyr::left_join(c_x_data))
    
  }
  
  # Create lagged exogenous data
  if(!is.null(specs$l_exog_data)){
    
    # Make lag sequence
    lags_exog      <- seq(specs$lags_exog_data)
    
    # Lag function
    lag_functions  <- lapply(lags_exog, function(x) function(col) dplyr::lag(col, x))
    
    # Make labels for lagged variables
    col_lag_labels <- paste(unlist(lapply(specs$l_exog_data, rep, max(lags_exog))), "_lag_", lags_exog, sep = "")
    
    # Make and get lagged data
    l_x_data       <- x_data %>%
      dplyr::select(cross_id, date_id, specs$l_exog_data)     |> 
      dplyr::group_by(cross_id)                               |> 
      dplyr::mutate(across(specs$l_exog_data, lag_functions)) |> 
      dplyr::ungroup()                                        |> 
      dplyr::select(-specs$l_exog_data)
    
    # Rename columns
    colnames(l_x_data)[3:dim(l_x_data)[2]] <- col_lag_labels
    
    # Use lagged exogenous data as regressor
    x_reg_data        <- suppressMessages(x_reg_data |> 
                                            dplyr::left_join(l_x_data))
    
  }
  
  # Calculate first differences of exogenous data?
  if(!is.null(specs$c_fd_exog_data) | !is.null(specs$l_fd_exog_data)){
    
    d_x_data       <- x_data                                             |> 
      dplyr::group_by(cross_id)                                          |> 
      dplyr::mutate_at(vars(-cross_id, -date_id), diff_function)         |> 
      dplyr::ungroup()                                                   |> 
      dplyr::rename_at(vars(-cross_id, -date_id), list(~paste0("d",.)))
    
  }
  
  # Create data with contemporanous impact of first differences
  if(!is.null(specs$c_fd_exog_data)){
    
    # Specify column names to choose
    specs$c_fd_exog_data <- paste("d", specs$c_fd_exog_data, sep = "")
    
    # Create data
    cd_x_data            <- d_x_data |> 
      dplyr::select(cross_id, date_id, specs$c_fd_exog_data)
    
    
    # Use first differences as regressors
    x_reg_data           <- suppressMessages(x_reg_data |> 
                                               dplyr::left_join(cd_x_data))
    
    
  }
  
  
  # Create lagged exogenous data of first differences
  if(!is.null(specs$l_fd_exog_data)){
    
    # Specify column names to choose
    specs$l_fd_exog_data <- paste("d", specs$l_fd_exog_data, sep = "")
    
    # Make lag sequence
    dlags_exog      <- seq(specs$lags_fd_exog_data)
    
    # Lag function
    lag_functions  <- lapply(dlags_exog, function(x) function(col) dplyr::lag(col, x))
    
    # Make labels for lagged variables
    col_dlag_labels <- paste(unlist(lapply(specs$l_fd_exog_data, rep, max(dlags_exog))), "_lag_", dlags_exog, sep = "")
    
    # Make and get lagged data
    ld_x_data       <- d_x_data %>%
      dplyr::select(cross_id, date_id, specs$l_fd_exog_data)     |> 
      dplyr::group_by(cross_id)                                  |> 
      dplyr::mutate(across(specs$l_fd_exog_data, lag_functions)) |>  # , .names = "{col_lag_labels}")
      dplyr::ungroup()                                           |> 
      dplyr::select(-specs$l_fd_exog_data)
    
    
    # Rename columns
    colnames(ld_x_data)[3:dim(ld_x_data)[2]] <- col_dlag_labels
    
    # Use lags of first differences as regressors
    x_reg_data    <- suppressMessages(x_reg_data |> 
                                        dplyr::left_join(ld_x_data))
  }
  
  
  return(list(x_reg_data = x_reg_data, y_data = y_data, specs = specs))
}







### TEST GROUND #### -----------------------------------------------------------

df_hp_large_lp <- df_hp_large |> 
  # filter(d_hhi_indicator) |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_ZLB2_NS", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


test <- LP_LIN_PANEL(
  data_set          = df_hp_large_lp,  # Panel dataset
  data_sample       = "Full",  # Use full sample or subset
  endog_data        = "log_loan_amount",  # Endogenous variable
  cumul_mult        = TRUE,  # Estimate cumulative multipliers?
  shock             = "I_HHI_ZLB2_NS",  # Shock variable
  diff_shock        = TRUE,  # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = "twoways",  # Panel effect type
  robust_cov        = "wild.cluster.boot",  # Robust covariance estimation method
  robust_cluster    = "time",
  c_exog_data       = NULL,  # Contemporaneous exogenous variables
  l_exog_data       = NULL,  # Lagged exogenous variables
  lags_exog_data    = NULL,  # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_hp_large_lp)[c(4, 6:10)],  # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_hp_large_lp)[c(4:10)],  # First-difference lagged exogenous variables
  lags_fd_exog_data = 1,  # Lag length for first-difference exogenous variables
  confint           = 1.65,  # Confidence interval width
  hor               = 6,
  biter             = 50
)



plot(test)


#### COMPARISON ####

## 3.1 FULL SAMPLE - Log Loan Amount -------------------------------------------

df_hp_large_lp <- pdata.frame(df_hp_large_lp, index = c("fips", "year"))

results_lpirf <- lpirfs::lp_lin_panel(
  data_set = df_hp_large_lp,
  data_sample = "Full",
  endog_data = "log_loan_amount",
  cumul_mult = TRUE,
  shock = "I_HHI_ZLB2_NS",
  diff_shock = TRUE,
  panel_model = "within",
  panel_effect = "twoways",
  robust_cov = "vcovSCC",
  c_fd_exog_data = colnames(df_hp_large_lp)[c(4, 6:10)],
  l_fd_exog_data = colnames(df_hp_large_lp)[c(4:10)],
  lags_fd_exog_data = 1,
  confint = 1.65,
  hor = 6
)

plot(results)



library(ggplot2)
library(dplyr)

# Convert both datasets into a long format with a 'group' variable
df_irf <- bind_rows(
  data.frame(
    time = seq_along(test$irf_panel_mean),
    lp = t(unlist(test$irf_panel_mean)),
    lower = t(unlist(test$irf_panel_low)),
    upper = t(unlist(test$irf_panel_up)),
    group = "Wild Cluster Boots"
  ),
  data.frame(
    time = seq_along(results_lpirf$irf_panel_mean),
    lp = t(unlist(results_lpirf$irf_panel_mean)),
    lower = t(unlist(results_lpirf$irf_panel_low)),
    upper = t(unlist(results_lpirf$irf_panel_up)),
    group = "DK98"
  )
)

# Plot with ggplot
ggplot(df_irf, aes(x = time, y = lp, color = group)) +
  geom_line(size = 1) +  # Mean response for both groups
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha = 0.2, color = NA) +  # Confidence interval
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Zero line
  labs(title = "Local Projection Impulse Response Comparison",
       x = "Time Horizon",
       y = "Impulse Response",
       color = "Model", 
       fill = "Model") +
  theme_minimal()


################################### END #######################################+

