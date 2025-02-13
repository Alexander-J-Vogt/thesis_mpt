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

# DEBUGGING ?
DEBUG <- F

if (DEBUG) {
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
}
# 01. LP_LIN_PANEL Function ----------------------------------------------------

#' The LP_LIN_PANEL function is based on the lpirfs package  by but is extended
#' [Schreib hier etwas schlaues]


LP_LIN_PANEL <- function(
    data_set          = NULL,  # Panel dataset
    data_sample       = "Full",  # Use full sample or subset
    endog_data        = NULL,  # Endogenous variable
    lags_endog_data   = NULL,  # NEW: Determine lags for endogenous Variable
    cumul_mult        = TRUE,  # Estimate cumulative multipliers?
    shock             = NULL,  # Shock variable
    lags_shock        = NULL,  # NEW: Determine lags for shock variable (for level & FD)
    diff_shock        = TRUE,  # First difference of shock variable
    panel_model       = "within",  # Panel model type
    panel_effect      = NULL,  # Panel effect type
    robust_cov        = NULL,  # Robust covariance estimation method: NEW: tLAHR
    robust_cluster    = NULL,  # time vs group
    c_exog_data       = NULL,  # Contemporaneous exogenous variables
    l_exog_data       = NULL,  # Lagged exogenous variables
    lags_exog_data    = NaN,  # Lag length for exogenous variables
    c_fd_exog_data    = NULL,  # First-difference contemporaneous exogenous variables
    l_fd_exog_data    = NULL,  # First-difference lagged exogenous variables
    lags_fd_exog_data = NaN,  # Lag length for first-difference exogenous variables
    confint           = NULL,  # Confidence interval width
    hor               = NULL,
    biter             = NULL
    ) {  # Number of horizons
  

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
  robust_cluster <- "time"
  c_exog_data <-  NULL #colnames(df_hp_large_lp)[c(4, 6:10)]
  l_exog_data <-  NULL #colnames(df_hp_large_lp)[c(4:10)] 
  c_fd_exog_data <- colnames(df_hp_large_lp)[c(4, 6:10)]
  l_fd_exog_data <- colnames(df_hp_large_lp)[c(4:10)]
  lags_exog_data <-  NULL
  lags_fd_exog_data <- 1
  confint <- 1.97
  hor <- 2
  biter <- 50
  lags_shock <- NULL
  lags_endog_data <- NULL
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
  specs <- list()
    
  # Basics
  specs$data_sample <-  data_sample
  specs$endog_data <-  endog_data
  specs$cumul_mult <-  cumul_mult
  specs$shock <-  shock
  specs$diff_shock <-  diff_shock

  # Panel Model
  specs$panel_model <-  panel_model
  specs$panel_effect <-  panel_effect

  # Robust
  specs$robust_cov <-  robust_cov
  specs$robust_cluster <- robust_cluster
  specs$confint <- confint
  specs$biter <- biter
  specs$model_type <- 2

  # lags for endogenous & shock
  specs$lags_shock <- lags_shock
  specs$lags_endog_data <- lags_endog_data
  
  # Exogenous Vars
  specs$c_exog_data <- c_exog_data
  specs$l_exog_data <- l_exog_data
  specs$lags_exog_data <- lags_exog_data
  specs$c_fd_exog_data <- c_fd_exog_data
  specs$l_fd_exog_data <- l_fd_exog_data
  specs$lags_fd_exog_data <- lags_fd_exog_data
  specs$hor <- hor
  specs$exog_data <- colnames(data_set)[which(!colnames(data_set) %in% c("cross_id", "date_id"))]
    
  # Not relevant argument
  specs$is_nl <- FALSE
  
  
  # Create panel data structure
  lin_panel_data <- CREATE_PANEL_DATA(specs, data_set)
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
  
  # Extract from specs
  robust_cluster <- specs$robust_cluster
  biter <- specs$biter
  panel_model <<- specs$panel_model
  panel_effect <<- specs$panel_effect
  
  # Loop over horizons to estimate impulse responses
  for(ii in 1:specs$hor){
    
    ## Create Panel Dataset for this Period ---
    # ii <- 1
    # Count Iteration
    specs$iteration <- ii

    # Merge dependent and independent variables
    yx_data <- dplyr::left_join(y_data[[ii]], x_reg_data, by = c("cross_id", "date_id")) %>% stats::na.omit()
    
    # Filter dataset if subset specified
    if(!(specs$data_sample[1] == 'Full')){
      yx_data <- dplyr::filter(yx_data, date_id %in% specs$data_sample)
    }
    
    # # Save in specs
    # specs$yx_data <- yx_data
    
    
    ## Estimate Regression ---
    
    # Depreciated - Estimate panel regression
    panel_results <- plm::plm(formula = plm_formula, data = yx_data, index = c("cross_id", "date_id"), model = panel_model, effect = panel_effect)
    
    # Solve Issue by re-assigning the yx_data into plm-object
    panel_results$call$data <- yx_data
    
    ## Calculate Standard Errors ---
    
    if (specs$robust_cov == "vcovSCC"){
      
      # Driscroll-Kraay SE (1998) - Accounts for cross-sectional and time dependence
      reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovSCC(panel_results,
                                                                          type    = specs$robust_type,
                                                                          maxlag  = specs$robust_maxlag))
      
    } else if (specs$robust_cov == "wild.cluster.boot") {
      
      # Determine confidence level
      confint_list <- list("1.96" = 0.95, "1.65" = 0.9, "1" = 0.68)  
      target_confint <- as.character(specs$confint)
      confint_level <- confint_list[[target_confint]]
      
      # pyx_data <- pdata.frame(yx_data, index = c("cross_id", "date_id"))
      print(robust_cluster)
      print(biter)
      
      # Apply Wild Cluster Bootstrap to account for the case when N converges to infinity with fixed T
      # Problem: Asymptotic Distribution will be dominated by the cross-sectional dimension, which
      # causes concerns about distortions generated when there are roots near unity.
      # Wild Cluster Bootstrap corrects the heteroskedasticity by using a cluster-robust approach.
      wild_b <- clusterSEs::cluster.wild.plm(panel_results,
                                             dat = yx_data,
                                             cluster = robust_cluster,
                                             boot.reps = biter,
                                             ci.level = confint_level,
                                             report = FALSE,
                                             prog.bar = FALSE
      )
      
      # Get Coefficient of Shock
      index_shock <- which(stats::variable.names(t(wild_b$ci)) == specs$shock)
      estimate <- coef(panel_results)[which(stats::variable.names(t(panel_results$coefficients)) == specs$shock)]
      
      # Calculate SE
      wild_se <- (wild_b$ci[index_shock, 2] - estimate) / specs$confint
      
      # Update Message
      message(paste0("Wild Cluster Bootstrap: ", specs$iteration))
      
      # Save in line with coeftest class
      reg_results <- data.frame(estimate = estimate, se = wild_se)
      
    } else if (specs$robust_cov == "tLAHR") {
      
      # Update Message
      message(paste0("tLAHR SE ", specs$iteration))
      
      # Clustered-Robust Heteroskedastic Standard Errors: Clustered for time
      reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovHC(panel_results,
                                                                         type     = "HC0",
                                                                         cluster  = "time")) 
      
    }
    
    # Define key objects
    reg_output_tmp <- panel_results
    reg_summary_tmp <- reg_results
    
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
# GET_ROBUST_COV_PANEL <- function(panel_results, specs, yx_data){
#   
#   # Check for specs
#   if (is.null(specs)) {
#     stop("Error: specs is not initialized correctly.")
#   }
#   
#   # Check for specs
#   if (is.null(yx_data)) {
#     stop("Error: yx_data is not initialized correctly.")
#   }
#   
#   
#   if(specs$robust_cov         == "vcovBK"){
#     
#     # Panel Corrected Standard Errors after Beck and Katz (1995)
#     reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovBK(panel_results,
#                                                                        type    = specs$robust_type,
#                                                                        cluster = specs$robust_cluster))
#     
#   } else if (specs$robust_cov == "vcovDC"){
#     
#     # High-level convenience wrapper for double-clustering robust covariance matrix estimators after Thompson (2011)
#     reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovDC(panel_results,
#                                                                        type    = specs$robust_type))
#     
#     
#   } else if (specs$robust_cov == "vcovHC"){
#     
#     # Clustered-Robust Heteroskedastic Standard Errors
#     reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovHC(panel_results,
#                                                                        method   = specs$robust_method,
#                                                                        type     = specs$robust_type,
#                                                                        cluster  = specs$robust_cluster))
#     
#   } else if (specs$robust_cov == "vcovNW"){
#     
#     # Newey-White Robust Standard Errors
#     reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovNW(panel_results,
#                                                                        type    = specs$robust_type,
#                                                                        maxlag  = specs$robust_maxlag))
#   } else if (specs$robust_cov == "vcovSCC"){
#     
#     # Driscroll-Kraay SE (1998) - Accounts for cross-sectional and time dependence
#     reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovSCC(panel_results,
#                                                                         type    = specs$robust_type,
#                                                                         maxlag  = specs$robust_maxlag))
#     
#   } else if (specs$robust_cov == "wild.cluster.boot") {
#     
#     # Determine confidence level
#     confint_list <- list("1.96" = 0.95, "1.65" = 0.9, "1" = 0.68)  
#     target_confint <- as.character(specs$confint)
#     confint_level <- confint_list[[target_confint]]
#     
#     pyx_data <- pdata.frame(yx_data, index = c("cross_id", "date_id"))
#     
#     # Apply Wild Cluster Bootstrap to account for the case when N converges to infinity with fixed T
#     # Problem: Asymptotic Distribution will be dominated by the cross-sectional dimension, which
#     # causes concerns about distortions generated when there are roots near unity.
#     # Wild Cluster Bootstrap corrects the heteroskedasticity by using a cluster-robust approach.
#     wild_b <- clusterSEs::cluster.wild.plm(panel_results,
#                                dat = pyx_data,
#                                cluster = specs$robust_cluster,
#                                boot.reps = specs$biter,
#                                ci.level = confint_level,
#                                report = FALSE,
#                                prog.bar = FALSE
#     )
#     
#     # Get Coefficient of Shock
#     index_shock <- which(stats::variable.names(t(wild_b$ci)) == specs$shock)
#     estimate <- coef(panel_results)[which(stats::variable.names(t(panel_results$coefficients)) == specs$shock)]
#     
#     # Calculate SE
#     wild_se <- (wild_b$ci[index_shock, 2] - estimate) / specs$confint
#     
#     # Update Message
#     message(paste0("Wild Cluster Bootstrap: ", specs$iteration))
#     
#     # Save in line with coeftest class
#     reg_results <- data.frame(estimate = estimate, se = wild_se)
#     
#   } else if (specs$robust_cov == "tLAHR") {
#     
#     # Update Message
#     message(paste0("tLAHR SE ", specs$iteration))
#     
#     # Clustered-Robust Heteroskedastic Standard Errors: Clustered for time
#     reg_results <- lmtest::coeftest(panel_results, vcov. = plm::vcovHC(panel_results,
#                                                                        type     = "HC0",
#                                                                        cluster  = "time")) 
#     
#   }
#   
#   return(reg_results)
#   
# }

## EXTENSION OF CREATE LIN PANEL #### ------------------------------------------


CREATE_PANEL_DATA <- function(specs, data_set){

  
  DEBUG <- T
  if (DEBUG) {
    specs <- list()
    data_set <- df_hp_large_lp
    data_set <- data_set |> rename(cross_id = fips, date_id = year)
    specs$data_sample <- "Full"
    specs$endog_data <- "log_loan_amount"
    specs$lags_endog_data <- 2
    specs$cumul_mult <-  TRUE
    specs$shock <-  "I_HHI_ZLB2_NS"
    specs$diff_shock <-  TRUE
    specs$lags_shock <- 2
    specs$panel_model <-  "within"
    specs$panel_effect <-  "twoways"
    specs$robust_cov <-  "wild.cluster.boot"
    specs$robust_method <- NULL
    specs$robust_type <- NULL
    specs$robust_cluster <- "time"
    specs$c_exog_data <-  NULL #colnames(df_hp_large_lp)[c(4, 6:10)]
    specs$l_exog_data <-  NULL #colnames(df_hp_large_lp)[c(4:10)] 
    specs$c_fd_exog_data <- colnames(df_hp_large_lp)[c(4, 6:10)]
    specs$l_fd_exog_data <- colnames(df_hp_large_lp)[c(4:10)]
    specs$lags_exog_data <-  NULL
    specs$lags_fd_exog_data <- 1
    specs$confint <- 1.65
    specs$hor <- 6
    specs$biter <- 10
    specs$exog_data           <- colnames(data_set)[which(!colnames(data_set) %in%
                                                            c("cross_id", "date_id"))]
    # specs$tLAHR <- TRUE
    specs$l_endog_data <- 1
  }
  
  #############################################################################+
  # Function ---
  #############################################################################+
  
  # Function to compute first differences
  diff_function <- function(data) {
    
    c(NA, diff(data))
  
  }
  
  # Function to compute cumulative endogenous variables
  cumul_function <- function(data, hor) {
    
    return(dplyr::lead(data, hor) - dplyr::lag(data, 1))
  
  }
  

  #############################################################################+
  # Endogenous Variable ---
  #############################################################################+

  # Prepare list for endogenous variables
  y_data <- vector("list", specs$hor)
  
  
  # Create horizons of dependent variables based on whether to use cumulative multipliers
  if(isTRUE(specs$cumul_mult)){
    
    # Loop to create endogenous variables
    for(ii in 0:(specs$hor-1)){
      
      # Create cumulative endogenous vector
      y_data[[ii + 1]]     <-  data_set                    |> 
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
  
  # Implement t-LAHR | time-clustered heteroskedastic-robust Inference
  if (!is.null(specs$lags_endog_data) | specs$robust_cov == "tLAHR") {
    
      if (!is.null(specs$lags_endog_data)) {  
        lags_endog <- seq(specs$lags_endog_data)
      
      } else if (specs$robust_cov =="tLAHR") {
        # Use simple p-selection rule for determining the number of lags p
        periods <- length(unique(y_data[[ii]]$date_id))
        p <- min(specs$hor, floor((periods - specs$hor)^(1/3)))
        
        # Make lag sequence
        lags_endog      <- seq(p)
      }
    
      # Lag function
      lag_functions  <- lapply(lags_endog, function(x) function(col) dplyr::lag(col, x))
      
      # Make labels for lagged variables
      end_lag_labels <- paste(unlist(lapply(specs$endog_data, rep, max(lags_endog))), "_lag_", lags_endog, sep = "")
      
      # Lag Levels of Endogenous variable
      l_y_data <- data_set |> 
        dplyr::select(cross_id, date_id, specs$endog_data)      |> 
        dplyr::group_by(cross_id)                               |> 
        dplyr::mutate_at(vars(specs$endog_data), lag_functions) |> 
        ungroup()                                               
      
        # BUG FIX -----------
        # |> 
        # dplyr::select(-specs$endog_data)
         
      
      # Rename columns - BUG FIX 
      # If more than 1 lag, than the lags a created under fn*
      if (max(lags_endog) > 1) {
        
        l_y_data <- l_y_data |> dplyr::select(-specs$endog_data)
        colnames(l_y_data)[3:dim(l_y_data)[2]] <- end_lag_labels
      
        # If only one lag, than lag is under original name of endogenouse variable
      } else {
        colnames(l_y_data)[3:dim(l_y_data)[2]] <- end_lag_labels
      }
      
      
      # Loop to add level Lags to each element in list
      for(ii in 0:(specs$hor-1)) {
        
        # Merge to y_data by list
        y_data[[ii + 1]] <- y_data[[ii + 1]] |> 
          left_join(l_y_data, by = c("cross_id", "date_id"))
        
      }
    
  }
  
  #############################################################################+
  # Shock Variable 
  #############################################################################+
  
  # Prepare shock variable ----------------------------------------------------+
  
  # Prepare shock variable
  x_reg_data <- data_set  |>  
    dplyr::select(cross_id, date_id, specs$shock)
  
  # specs$l_shock <- 1
  # specs$robust_cov <- "vcov"
  # specs$diff_shock <- TRUE
  

  # Lag level Shock - tLAHR ### 
  
  if ((!is.null(specs$lags_shock) | specs$robust_cov == "tLAHR") & isFALSE(specs$diff_shock)) {
     
    # Implement t-LAHR as proposed by Almuzara & Sancibrian (2024)
    if (!is.null(specs$lags_shock)) {
    
      # Choose your own number of lags
      lags_shock <- specs$lags_shock
    
    } else if (specs$robust_cov == "tLAHR") {
      
      # Use simple p-selection rule for determining the number of lags p
      periods <- length(unique(x_reg_data$date_id))
      p <- min(specs$hor, floor((periods - specs$hor)^(1/3)))
    
      # Make lag sequence
      lags_shock <- seq(p)
    
    }
    
    # Lag function
    lag_functions  <- lapply(lags_shock, function(x) function(col) dplyr::lag(col, x))
    
    # Make labels for lagged variables
    end_lag_labels <- paste(unlist(lapply(specs$shock, rep, max(lags_shock))), "_lag_", lags_shock, sep = "")
    
    # Lag Levels of Endogenous variable
    x_reg_data <- x_reg_data |> 
      dplyr::select(cross_id, date_id, specs$shock)      |> 
      dplyr::group_by(cross_id)                          |> 
      dplyr::mutate_at(vars(specs$shock), lag_functions) |> 
      ungroup()
    
    # Rename columns
    colnames(x_reg_data)[4:dim(x_reg_data)[2]] <- end_lag_labels
  
  }
  
  
  # Take first differences of shock variable? ---------------------------------+
  
  # Overwrite x_reg_data in case of FD ###

  if(isTRUE(specs$diff_shock)){
    
    x_reg_data    <- x_reg_data                                 |> 
      dplyr::group_by(cross_id)                                 |> 
      dplyr::mutate_at(vars(specs$shock), diff_function)        |> 
      dplyr::rename_at(vars(specs$shock), list(~paste0("d",.))) |> 
      dplyr::ungroup()
    
    # Rename shock variable
    specs$shock   <- colnames(x_reg_data)[which(!(colnames(x_reg_data) %in% c("cross_id", "date_id")))]
    
  }
  
  # Lag level FD Shock ###
  if ((!is.null(specs$lags_shock) | specs$robust_cov == "tLAHR") & isTRUE(specs$diff_shock)) {
    
    # Implement t-LAHR as proposed by Almuzara & Sancibrian (2024)
    if (!is.null(specs$lags_shock)) {
      
      # Choose your own number of lags
      lags_shock <- specs$lags_shock
      
    } else if (specs$robust_cov == "tLAHR") {
      
      # Use simple p-selection rule for determining the number of lags p
      periods <- length(unique(x_reg_data$date_id))
      p <- min(specs$hor, floor((periods - specs$hor)^(1/3)))
      
      # Make lag sequence
      lags_shock <- seq(p)
      
    }
    
    # Lag function
    lag_functions  <- lapply(lags_shock, function(x) function(col) dplyr::lag(col, x))
    
    # Make labels for lagged variables
    end_lag_labels <- paste(unlist(lapply(specs$shock, rep, max(lags_shock))), "_lag_", lags_shock, sep = "")
    
    # Lag Levels of Endogenous variable
    x_reg_data <- x_reg_data |> 
      dplyr::select(cross_id, date_id, specs$shock)      |> 
      dplyr::group_by(cross_id)                               |> 
      dplyr::mutate_at(vars(specs$shock), lag_functions) |> 
      ungroup()
    
    # Rename columns
    colnames(x_reg_data)[4:dim(x_reg_data)[2]] <- end_lag_labels
    
  }
  

  #############################################################################+
  # Exogenous Data ---
  #############################################################################+
  
  # Prepare exogenous data ----------------------------------------------------+
  
  # Select first all var but later select exogenous vars
  # depending on the defined vars in specs.
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
  
  
  # Create lagged exogenous data ----------------------------------------------+ 
  
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
  
  
  # Calculate first differences of exogenous data? ----------------------------+
  
  if(!is.null(specs$c_fd_exog_data) | !is.null(specs$l_fd_exog_data)){
    
    d_x_data       <- x_data                                             |> 
      dplyr::group_by(cross_id)                                          |> 
      dplyr::mutate_at(vars(-cross_id, -date_id), diff_function)         |> 
      dplyr::ungroup()                                                   |> 
      dplyr::rename_at(vars(-cross_id, -date_id), list(~paste0("d",.)))
    
  }
  
  # Create data with contemporanous impact of first differences ---------------+
  
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
  
  
  # Create lagged exogenous data of first differences -------------------------+
  
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

if (DEBUG) {

df_hp_large_lp <- df_hp_large |> 
  # filter(d_hhi_indicator) |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_ZLB2_NS", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


WCD <- LP_LIN_PANEL(
  data_set          = df_hp_large_lp,  # Panel dataset
  data_sample       = "Full",  # Use full sample or subset
  lags_endog_data   = 2,
  endog_data        = "log_loan_amount",  # Endogenous variable
  cumul_mult        = TRUE,  # Estimate cumulative multipliers?
  shock             = "I_HHI_ZLB2_NS",  # Shock variable
  lags_shock        = 2,
  diff_shock        = TRUE,  # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = "twoways",  # Panel effect type
  robust_cov        = "wild.cluster.boot",  # Robust covariance estimation method
  robust_cluster    = "time",
  c_exog_data       = NULL,  # Contemporaneous exogenous variables
  l_exog_data       = NULL,  # Lagged exogenous variables
  lags_exog_data    = NULL,  # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_hp_large_lp)[c(4, 6:10)],  # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_hp_large_lp)[c(6:10)],  # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,  # Lag length for first-difference exogenous variables
  confint           = 1.96,  # Confidence interval width
  hor               = 1,
  biter             = 10
)




# plot(test)


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
  robust_cluster = "time",
  c_fd_exog_data = colnames(df_hp_large_lp)[c(4, 6:10)],
  l_fd_exog_data = colnames(df_hp_large_lp)[c(4:10)],
  lags_fd_exog_data = 1,
  confint = 1.96,
  hor = 6
)



plot(results)




# TRY tLAHR --------------------------------------------------------------------

tlahr <- LP_LIN_PANEL(
  data_set          = df_hp_large_lp,  # Panel dataset
  data_sample       = "Full",  # Use full sample or subset
  endog_data        = "log_loan_amount",  # Endogenous variable
  cumul_mult        = TRUE,  # Estimate cumulative multipliers?
  shock             = "I_HHI_ZLB2_NS",  # Shock variable
  diff_shock        = TRUE,  # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = "twoways",  # Panel effect type
  robust_cov        = "tLAHR",  # Robust covariance estimation method, automatically 
  robust_cluster    = "time",
  c_exog_data       = NULL,  # Contemporaneous exogenous variables
  l_exog_data       = NULL,  # Lagged exogenous variables
  lags_exog_data    = NULL,  # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_hp_large_lp)[c(4, 6:10)],  # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_hp_large_lp)[c(4, 6:10)],  # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,  # Lag length for first-difference exogenous variables
  confint           = 1.96,  # Confidence interval width
  hor               = 6
)

DK <- LP_LIN_PANEL(
  data_set          = df_hp_large_lp,  # Panel dataset
  data_sample       = "Full",  # Use full sample or subset
  endog_data        = "log_loan_amount",  # Endogenous variable
  cumul_mult        = TRUE,  # Estimate cumulative multipliers?
  shock             = "I_HHI_ZLB2_NS",  # Shock variable
  diff_shock        = TRUE,  # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = "twoways",  # Panel effect type
  robust_cov        = "vcovSCC",  # Robust covariance estimation method, automatically 
  robust_cluster    = "time",
  c_exog_data       = NULL,  # Contemporaneous exogenous variables
  l_exog_data       = NULL,  # Lagged exogenous variables
  lags_exog_data    = NULL,  # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_hp_large_lp)[c(4, 6:10)],  # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_hp_large_lp)[c(4, 6:10)],  # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,  # Lag length for first-difference exogenous variables
  confint           = 1.96,  # Confidence interval width
  hor               = 6
)


# WCD$reg_outputs

# PLOT -------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Convert both datasets into a long format with a 'group' variable
df_irf <- bind_rows(
  data.frame(
    time = seq_along(WCD$irf_panel_mean),
    lp = t(unlist(WCD$irf_panel_mean)),
    lower = t(unlist(WCD$irf_panel_low)),
    upper = t(unlist(WCD$irf_panel_up)),
    group = "Wild Cluster Boots"
  ),
  data.frame(
    time = seq_along(DK$irf_panel_mean),
    lp = t(unlist(DK$irf_panel_mean)),
    lower = t(unlist(DK$irf_panel_low)),
    upper = t(unlist(DK$irf_panel_up)),
    group = "DK98"
  ),
  data.frame(
    time = seq_along(tlahr$irf_panel_mean),
    lp = t(unlist(tlahr$irf_panel_mean)),
    lower = t(unlist(tlahr$irf_panel_low)),
    upper = t(unlist(tlahr$irf_panel_up)),
    group = "tLAHR"
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

}

# GGPLOT for One Estimate -----------------------------------------------------


GG_IRF_ONE <- function(data, 
                       hhi_coef = FALSE, 
                       y_lower = -1, 
                       y_upper = 1, 
                       title_name = "Impulse Response Function",
                       time_mame = "Time Horizon"
                       ) {
  
  # Initiate df
  df <- data.frame(
    time = integer(0),
    lp = integer(0),
    lp_lower = integer(0),
    lp_upper = integer(0)
  )
  
  # Add HHI Coefficient
  if (isTRUE(hhi_coef)) {
    
    for (i in 1:tlahr$specs$hor) {
      # Identify hhi coefficient
      coef_names <- names(coef(data$reg_summaries[[i]]))
      coef_hhi <- coef_names[ grepl("hhi", coef_names) & !grepl("lag", coef_names) ]
      estimate_hhi <- coef(data$reg_summaries[[i]])[coef_hhi]
      
      # Add HHI coef to shock
      df[i, "time"] <- i
      df[i, "lp"] <- unname(data$irf_panel_mean[[i]] + estimate_hhi)
      df[i, "lp_upper"] <- unname(data$irf_panel_up[[i]] + estimate_hhi)
      df[i, "lp_lower"] <- unname(data$irf_panel_low[[i]] + estimate_hhi)
    }
    
  } else {
    
    # Only Shock Coefficient
    df[i, "time"] <- i
    df[i, "lp"] <- data$irf_panel_mean[[i]] 
    df[i, "lp_upper"] <- data$irf_panel_up[[i]] 
    df[i, "lp_lower"] <- data$irf_panel_low[[i]]
  }
    
  # Plot with CI Bands
  ggplot(df, aes(x = time, y = lp)) +
    # LP
    geom_line(linewidth = 1, color = "red") +  # Plot the mean response lines
    # CI Band
    geom_ribbon(aes(ymin = lp_lower, ymax = lp_upper), 
                alpha = 0.1,
                fill = "red",
                color = "red",
                linetype = "dashed") +  # Add shaded confidence intervals
    # x-axis 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add a horizontal zero line
    # Scale y- and x-axos
    scale_x_continuous(
      breaks = seq(1:tlahr$specs$hor)
    ) +
    scale_y_continuous(
      breaks = seq(y_lower, y_upper, by = .2),
      limits = c(y_lower, y_upper)
    ) +
    #title
    labs(title = title_name,
         x = time_name,
         y = expression(Y[ t+h ] - Y[t]),
         color = "Model", 
         fill = "Model") +
    theme_minimal()
  
 }


# GG_IRF_ONE(data = tlahr, hhi_coef = TRUE, y_lower = -1, y_upper = 1, name = "IRF: tLAHR")

# GGPLOT for TWO Estimate aka Comparison ---------------------------------------
GG_IRF_TWO <- function(data1, # DF1
                       data2, # DF2
                       data_name, # Name in vector; Order in Data1, Data2
                       hhi_coef = FALSE, # Add hhi coef to IRF: Yes Or No 
                       y_lower = -1, # Lower Bound of y-axis
                       y_upper = 1, # Upper Boind of y_axis
                       name = "Impulse Response Function" # Name of Graph
                       ) {
  
  # Prepare datasets
  data_list <- list(data1, data2)
  
  ## Determine DF with and without HHI Estiamte -------------------------------+
  
  # Create DF for two datasets with HHI Coefficient added 
  
    # Loop over datasets
    df_main <- map2_dfr(data_list, data_name, function(data, label) {
      
      # Pre-allocate the output data frame
      df <- data.frame(
        time = 1:data$specs$hor,
        lp = NA_real_,
        lp_upper = NA_real_,
        lp_lower = NA_real_,
        cat = rep(label, data$specs$hor),
        stringsAsFactors = FALSE
      )
      
      # Populate df with shock coefficients (with or without HHI adjustment)
      for (i in 1:data$specs$hor) {
        
        # Add HHI Coef to the analysis
        if (hhi_coef) {
          
          coef_names <- names(coef(data$reg_summaries[[i]]))
          coef_hhi <- coef_names[grepl("hhi", coef_names) & !grepl("lag", coef_names)]
          estimate_hhi <- sum(coef(data$reg_summaries[[i]])[coef_hhi], na.rm = TRUE)  # Handle missing values safely
          
          df[i, "lp"] <- unname(data$irf_panel_mean[[i]] + estimate_hhi)
          df[i, "lp_upper"] <- unname(data$irf_panel_up[[i]] + estimate_hhi)
          df[i, "lp_lower"] <- unname(data$irf_panel_low[[i]] + estimate_hhi)
          
        # Do not add HHI coef to the analysis  
        } else {
          
          df[i, "lp"] <- data$irf_panel_mean[[i]]
          df[i, "lp_upper"] <- data$irf_panel_up[[i]]
          df[i, "lp_lower"] <- data$irf_panel_low[[i]]
        }
      }
      
      return(df)
    })
  
  ## Create Graph -------------------------------------------------------------+
  
  # Define color mapping dynamically
  color_palette <- c("red", "blue") # Extend if needed
  color_mapping <- setNames(color_palette[seq_along(data_name)], data_name)
  
  # Plot with CI Bands
  plot <- ggplot(df_main, aes(x = time, y = lp, color = cat )) +
    # LP
    geom_line(size = 1) +  # Plot the mean response lines
    # CI Band
    geom_ribbon(aes(ymin = lp_lower, ymax = lp_upper, fill = cat), 
                alpha = 0.1,
                linetype = "dashed") +  # Add shaded confidence intervals
    # x-axis 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add a horizontal zero line
    # Scale y- and x-axos
    scale_x_continuous(
      breaks = seq(1, max(tlahr$specs$hor))
    ) +
    scale_y_continuous(
      breaks = seq(y_lower, y_upper, by = .2),
      limits = c(y_lower, y_upper)
    ) +
    # title
    labs(title = name,
         x = "Time Horizon",
         y = expression(Y[ t+h ] - Y[t]),
         color = "Model", 
         fill = "Model") +
    # Manually define the colors for each group
    scale_color_manual(values = color_mapping) +
    scale_fill_manual(values = color_mapping) +
    theme_minimal()
  
  result <- list(plot = plot, df = df_main)

  
} # End of Function: GG_IRF_TWO



# two_graphs <- GG_IRF_TWO(data1 = tlahr,
#            data2 = DK,
#            data_name = c("tLAHR", "DK")
#            
#            )

## Dynamicall GGPLOT for more than two datasets --------------------------------

GG_IRF_DYNAMIC <- function(data_list,  # List of datasets
                           data_name,  # Vector of dataset names
                           hhi_coef = FALSE,  # Add HHI coefficient to IRF
                           y_lower = -1,  # Lower Bound of y-axis
                           y_upper = 1,  # Upper Bound of y-axis
                           name = "Impulse Response Function"  # Graph title
) {
  
  ## Check Inputs
  if (length(data_list) != length(data_name)) {
    stop("Error: The number of datasets in 'data_list' must match the number of names in 'data_name'.")
  }
  
  ## Process Each Dataset and Create a Combined DataFrame
  df_main <- map2_dfr(data_list, data_name, function(data, label) {
    
    # Pre-allocate DataFrame
    df <- data.frame(
      time = 1:data$specs$hor,
      lp = NA_real_,
      lp_upper = NA_real_,
      lp_lower = NA_real_,
      cat = rep(label, data$specs$hor),
      stringsAsFactors = FALSE
    )
    
    # Fill DataFrame with Shock Coefficients (with or without HHI adjustment)
    for (i in 1:data$specs$hor) {
      
      if (hhi_coef) {
        coef_names <- names(coef(data$reg_summaries[[i]]))
        coef_hhi <- coef_names[grepl("hhi", coef_names) & !grepl("lag", coef_names)]
        estimate_hhi <- sum(coef(data$reg_summaries[[i]])[coef_hhi], na.rm = TRUE)  # Handle missing values safely
        
        df[i, "lp"] <- unname(data$irf_panel_mean[[i]] + estimate_hhi)
        df[i, "lp_upper"] <- unname(data$irf_panel_up[[i]] + estimate_hhi)
        df[i, "lp_lower"] <- unname(data$irf_panel_low[[i]] + estimate_hhi)
      } else {
        df[i, "lp"] <- data$irf_panel_mean[[i]]
        df[i, "lp_upper"] <- data$irf_panel_up[[i]]
        df[i, "lp_lower"] <- data$irf_panel_low[[i]]
      }
    }
    
    return(df)
  })
  
  ## Create Plot -------------------------------------------------------------+
  
  # Generate a Color Palette for Any Number of Datasets
  color_palette <- scales::hue_pal()(length(data_name))  # Generates distinct colors dynamically
  color_mapping <- setNames(color_palette, data_name)    # Assign colors to dataset names
  
  plot <- ggplot(df_main, aes(x = time, y = lp, color = cat)) +
    geom_line(size = 1) +  # Plot mean response lines
    geom_ribbon(aes(ymin = lp_lower, ymax = lp_upper, fill = cat), 
                alpha = 0.1,
                linetype = "dashed") +  # Add shaded confidence intervals
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add a horizontal zero line
    scale_x_continuous(
      breaks = seq(1, max(df_main$time))
    ) +
    scale_y_continuous(
      breaks = seq(y_lower, y_upper, by = 0.2),
      limits = c(y_lower, y_upper)
    ) +
    labs(title = name,
         x = "Time Horizon",
         y = expression(Y[ t+h ] - Y[t]),
         color = "Model", 
         fill = "Model") +
    scale_color_manual(values = color_mapping) +
    scale_fill_manual(values = color_mapping) +
    theme_minimal()
  
  return(list(plot = plot, df = df_main))
}


if (DEBUG) {
results_list <- list(tlahr, WCD)

test2 <- dynamic_graphs <- GG_IRF_DYNAMIC(
           data_list =   results_list,
           data_name = c("tLAHR", "WCD")

           )
test2$plot

}
################################### END #######################################+


