# TARGET: Local Projection model for the EA
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

# 1. Load Dataset ==============================================================

# Full Sample
df_main <- LOAD("39_thesis_mpt_eu_samplecreation_main_m")

# NIRP Sample
df_main_nirp <- df_main |> 
  filter(d_dfr_nirp == 1) |> 
  mutate(month = as.numeric(month))

# Sample away from the NIRP
df_main_normal <- df_main |> 
  filter(d_dfr_nirp == 0) |> 
  mutate(month = as.numeric(month))

# Time Period within the NIRP period
nirp_period <- as.numeric(df_main_nirp$month)


###############################################################################+
# 2. Local Projection ##########################################################
###############################################################################+

## General Specification - Regression Details ---------------------------------+

# Panel Effect
PANEL_EFFECT <- "individual"

# Siginficance Level
CI <- 1.96

# Horizons 
HOR <- 12 

## General Specification - Controls -------------------------------------------+

endo_rate <- "lending_rate_total"


controls_lending_rate <- c("log_assets", "log_total_loan", "log_overnight_deposits", "deposit_rate", # Controls for bank sector interaction
                           "ur", "hicp_inflation", "reer", "commodity_index", "exr", "gdp_country_growth", # Controls for Macroeconomic Indicators on country-level
                           "hicp_ea_inflation", "gdp_ea_growth", # Controls for Macroeconomic Indicators on EA-level
                           "hpi_growth", "hosr", # Housing / Mortgage Market related controls
                           "d_month_02","d_month_03","d_month_04", "d_month_05", "d_month_06","d_month_06", "d_month_08","d_month_09", "d_month_10", "d_month_11","d_month_12","d_countries_arm" # Monthly dummies
                            )
                           
controls_loan_amount <-  c("log_assets", "log_total_loan", "log_overnight_deposits", "deposit_rate_deci", # Controls for bank sector interaction 
                           "ur_deci", "hicp_inflation_deci", "reer", "commodity_index", "exr", "gdp_country_growth_deci", # Controls for Macroeconomic Indicators on country-level
                           "hicp_ea_inflation_deci", "gdp_ea_growth_deci", # Controls for Macroeconomic Indicators on EA-level
                           "hpi_growth_deci", "hosr", # Housing / Mortgage Market related controls
                           "d_month_02","d_month_03","d_month_04", "d_month_05", "d_month_06","d_month_06", "d_month_08","d_month_09", "d_month_10", "d_month_11","d_month_12", "d_countries_arm" #  dummies
                           )

###############################################################################+
## 2.1 Baseline ################################################################
###############################################################################+

### 2.1.1 Regression for Baseline Specification --------------------------------

# Specification for Parallel Computing
specs_baseline_JK <- list(
  ## Lending Rate
  # HHI-Demeaned x MS
  lending_rate_hhi_ms_demeaned = list(
    shock_var   = "I_HHI_J_TOTAL_demeaned",
    select_cols = c("country", "month", endo_rate, "MP_median_total", "I_HHI_J_TOTAL_demeaned", controls_lending_rate),
    endo = endo_rate,
    l_var = c(4, 6:19),
    c_var = c(4, 6:30)
  ),
  # HHI x MS
  lending_rate_hhi_ms = list(
    shock_var   = "I_HHI_J_TOTAL",
    select_cols = c("country", "month", endo_rate, "MP_median_total", "I_HHI_J_TOTAL", controls_lending_rate),
    endo = endo_rate,
    l_var = c(4, 6:19),
    c_var = c(4, 6:30)
  )  ,
  # MS
  lending_rate_ms = list(
    shock_var   = "MP_median_total",
    select_cols = c("country", "month", endo_rate,  "MP_median_total", controls_lending_rate),
    endo = endo_rate,
    l_var = c(5:19),
    c_var = c(5:29)
  ),
  ## Log Mortgage Rate
  # HHI-Demeaned x MS
  loan_amount_hhi_ms_demeaned = list(
    shock_var   = "I_HHI_J_TOTAL_demeaned",
    select_cols = c("country", "month", "log_hp_total_amount", "MP_median_total", "I_HHI_J_TOTAL_demeaned",  controls_loan_amount),
    endo = "log_hp_total_amount",
    l_var = c(4, 6:19),
    c_var = c(4, 6:30)
  ),
  # HHI x MS
  loan_amount_hhi_ms = list(
    shock_var   = "I_HHI_J_TOTAL",
    select_cols = c("country", "month", "log_hp_total_amount", "MP_median_total", "I_HHI_J_TOTAL", controls_loan_amount),
    endo = "log_hp_total_amount",
    l_var = c(4, 6:19),
    c_var = c(4, 6:30)
  ),
  # MS
  loan_amount_ms = list(
    shock_var   = "MP_median_total",
    select_cols = c("country", "month", "log_hp_total_amount", "MP_median_total", controls_loan_amount),
    endo = "log_hp_total_amount",
    l_var = c(5:19),
    c_var = c(5:29)
  )
)

# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run the LP_LIN_PANEL function in parallel for each shock type
# Ensure that all required objects and functions (like df_hp_large, LP_LIN_PANEL, PANEL_EFFECT, CI, HOR, and BITER) 
# are available in the global environment or exported to each worker.
results_baseline <- foreach(spec = specs_baseline_JK, 
                            .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"), 
                            .export = c("df_main", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR")) %dopar% {
  
                            # Subset the data for the current shock specification
                            df_subset <- df_main |>  
                              select(all_of(spec$select_cols))
                          
                            # Run the LP_LIN_PANEL function
                            LP_LIN_PANEL(
                              data_set          = df_subset,
                              data_sample       = "Full",
                              endog_data        = spec$endo,
                              lags_endog_data   = 6,
                              cumul_mult        = T,
                              shock             = spec$shock_var,
                              lags_shock        = 6,
                              diff_shock        = F,
                              panel_model       = "within",
                              panel_effect      = PANEL_EFFECT,
                              robust_cov        = "tLAHR",
                              c_exog_data       = colnames(df_subset)[spec$c_var],
                              l_exog_data       = colnames(df_subset)[spec$l_var],
                              lags_exog_data    = 6,
                              # c_fd_exog_data    = colnames(df_subset)[spec$c_var],
                              # l_fd_exog_data    = colnames(df_subset)[spec$l_var],
                              # lags_fd_exog_data = 6,
                              confint           = CI,
                              hor               = HOR
                           )
                         }

# Shut down the parallel cluster
stopCluster(cl)

names(results_baseline) <- names(specs_baseline_JK)


### 2.1.2 Graphs on Regression Results for Baseline Specification --------------

# Graph 1: Lending Rate: HHI Demeaned x MS [INCLUDE] --------------------------+

plot_1_hhi_demeaned_ms <- GG_IRF_ONE(data = results_baseline$lending_rate_hhi_ms_demeaned,
                                     hhi_coef = FALSE,
                                     y_lower = -7,
                                     y_upper = 6,
                                     breaks = 1,
                                     title_name = expression("Mortgage Rate: " * delta[1]^h),
                                     time_name = "Months",
                                     y_axis_name = expression(r[t + h] - r[t])
                                     )

# Graph 1.1: Average Effect  HHI Demeaned x MS [INCLUDE] ----------------------+

plot_11_ms <- GG_IRF_VAR(data = results_baseline$lending_rate_hhi_ms_demeaned,
                         var  = "MP_median_total",
                         y_lower = -7, 
                         y_upper = 6,
                         breaks = 1,
                         title_name = expression("Mortgage Rate: " * delta[0]^h),
                         time_name = "Months",
                         y_axis_name = expression(r[t+h] - r[t])
                          )

# Graph 2: Lending Rate: HHI x MS ---------------------------------------------+

plot_2_hhi_ms <- GG_IRF_ONE(data = results_baseline$lending_rate_hhi_ms,
                            hhi_coef = FALSE,
                            y_lower = -6,
                            y_upper = 5,
                            breaks = 1,
                            title_name = "Mortgage Rate",
                            time_name = "Months",
                            y_axis_name = expression(r[t + h] - r[t])
                            )

# Graph 3: Log Outstanding Loan Amount: HHI Demeaned x MS [INCLUDE] -----------+

plot_3_hhi_demeaned_ms <- GG_IRF_ONE(data = results_baseline$loan_amount_hhi_ms_demeaned,
                                     hhi_coef = FALSE,
                                     y_lower = -2,
                                     y_upper = 1,
                                     breaks = .5,
                                     title_name = expression("Mortgage Amount: " * delta[1]^h),
                                     time_name = "Months",
                                     y_axis_name = expression(Y[t + h] - Y[t])
                                     )

# Graph 3.1: Average Effect HHI Demeaned x MS [INCLUDE] -----------------------+

plot_31_ms <- GG_IRF_VAR(data = results_baseline$loan_amount_hhi_ms_demeaned,
                         var  = "MP_median_total",
                         y_lower = -2, 
                         y_upper = 1,
                         breaks = .5,
                         title_name = expression("Mortgage Amount: " * delta[0]^h),
                         time_name = "Months",
                         y_axis_name = expression(Y[t+h] - Y[t])
                         )


# Graph 4: Log Outstanding Loan Amount: HHI x MS ------------------------------+

plot_4_hhi_ms <- GG_IRF_ONE(data = results_baseline$loan_amount_hhi_ms,
                            hhi_coef = FALSE,
                            y_lower = -2,
                            y_upper = 2,
                            breaks = 1,
                            title_name = "Mortgage Amount",
                            time_name = "Months",
                            y_axis_name = expression(Y[t + h] - Y[t])
                            )



# Final Graphs ... ------------------------------------------------------------+

# Graph for Demeaned Baseline
graph_baseline_demeaned <- (plot_31_ms$plot + plot_3_hhi_demeaned_ms$plot) /
                           (plot_11_ms$plot + plot_1_hhi_demeaned_ms$plot) +
                            plot_annotation(
                              tag_levels = c("I", "II", "III", "IV"),
                              theme = theme(
                                plot.title = element_text(size = 14, hjust = 0.5)
                                )
                              )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "04_EA_Panel_D/", "baseline_demeaned_ea.pdf"),
  plot = graph_baseline_demeaned,
  width = 10, height = 8, dpi = 300
)
}

### 2.1.3 Appendix for Baseline Regression -------------------------------------

# Appendix Graph 1: Lending Rate: MS ------------------------------------------+

plot_appendix_1_hhi_demeaned_ms <- GG_IRF_ONE(data = results_baseline$lending_rate_ms,
                                     hhi_coef = FALSE,
                                     y_lower = -2,
                                     y_upper = 1,
                                     breaks = .5,
                                     title_name = "Mortgage Rate",
                                     time_name = "Months",
                                     y_axis_name = expression(r[t + h] - r[t])
                                     )

# Appendix Graph 2: Loan Amount: MS -------------------------------------------+

plot_appendix_2_hhi_demeaned_ms <- GG_IRF_ONE(data = results_baseline$loan_amount_ms,
                                              hhi_coef = FALSE,
                                              y_lower = -2,
                                              y_upper = 1,
                                              breaks = .5,
                                              title_name = "Mortgage Amount",
                                              time_name = "Months",
                                              y_axis_name = expression(Y[t + h] - Y[t])
                                              )


# Appendix Graph 
graph_baseline_appendix <- (plot_appendix_1_hhi_demeaned_ms$plot + plot_appendix_2_hhi_demeaned_ms$plot) +
                            plot_annotation(
                              title = "Appendix D: IRF of a Monetary Shock on Endogenous Variables",
                              tag_levels = c("I", "II"),
                              theme = theme(
                                plot.title = element_text(size = 14, hjust = 0.5)
                              )
                            )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "04_EA_Panel_D/", "baseline_appendix_ea.pdf"),
  plot = graph_baseline_appendix,
  width = 12, height = 6, dpi = 300
)
}

###############################################################################+
## 2.2 Regression with NIRP Indicator ##########################################
###############################################################################+

### 2.2.1 Regression with NIRP Indicator ---------------------------------------

# Specification for Parallel Computing
specs_nirp_indicator_JK <- list(
  ## Lending Rate
  # HHI-Demeaned x MS
  lending_rate_hhi_ms_demeaned_nirp = list(
    shock_var   = "I_HHI_J_TOTAL_NIRP_demeaned",
    select_cols = c("country", "month", endo_rate, "I_J_TOTAL_NIRP", "I_HHI_J_TOTAL_NIRP_demeaned", controls_lending_rate),
    endo = endo_rate,
    l_var = c(4, 6:18),
    c_var = c(4, 6:29)
  ),
  # HHI x MS
  lending_rate_hhi_ms_nirp = list(
    shock_var   = "I_HHI_J_TOTAL_NIRP",
    select_cols = c("country", "month", endo_rate, "I_J_TOTAL_NIRP", "I_HHI_J_TOTAL_NIRP", controls_lending_rate),
    endo = endo_rate,
    l_var = c(4, 6:18),
    c_var = c(4, 6:29)
  )  ,
  # MS
  lending_rate_ms_nirp = list(
    shock_var   = "I_J_TOTAL_NIRP",
    select_cols = c("country", "month", endo_rate,  "I_J_TOTAL_NIRP", controls_lending_rate),
    endo = endo_rate,
    l_var = c(5:18),
    c_var = c(5:28)
  ),
  ## Log Mortgage Rate
  # HHI-Demeaned x MS
  loan_amount_hhi_ms_demeaned_nirp = list(
    shock_var   = "I_HHI_J_TOTAL_NIRP_demeaned",
    select_cols = c("country", "month", "log_hp_total_amount", "I_J_TOTAL_NIRP", "I_HHI_J_TOTAL_NIRP_demeaned",  controls_loan_amount),
    endo = "log_hp_total_amount",
    l_var = c(4, 6:18),
    c_var = c(4, 6:29)
  ),
  # HHI x MS
  loan_amount_hhi_ms_nirp = list(
    shock_var   = "I_HHI_J_TOTAL_NIRP",
    select_cols = c("country", "month", "log_hp_total_amount", "I_J_TOTAL_NIRP", "I_HHI_J_TOTAL_NIRP", controls_loan_amount),
    endo = "log_hp_total_amount",
    l_var = c(4, 6:18),
    c_var = c(4, 6:29)
  ),
  # MS
  loan_amount_ms_nirp = list(
    shock_var   = "I_J_TOTAL_NIRP",
    select_cols = c("country", "month", "log_hp_total_amount", "I_J_TOTAL_NIRP", controls_loan_amount),
    endo = "log_hp_total_amount",
    l_var = c(5:18),
    c_var = c(5:28)
  )
)


# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run the LP_LIN_PANEL function in parallel for each shock type
# Ensure that all required objects and functions (like df_hp_large, LP_LIN_PANEL, PANEL_EFFECT, CI, HOR, and BITER) 
# are available in the global environment or exported to each worker.
results_nirp_indicator <- foreach(spec = specs_nirp_indicator_JK, 
                            .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"), 
                            .export = c("df_main", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR")) %dopar% {
                              
                              # Subset the data for the current shock specification
                              df_subset <- df_main %>% 
                                select(all_of(spec$select_cols))
                              
                              # Run the LP_LIN_PANEL function
                              LP_LIN_PANEL(
                                data_set          = df_subset,
                                data_sample       = "Full",
                                endog_data        = spec$endo,
                                lags_endog_data   = 6,
                                cumul_mult        = TRUE,
                                shock             = spec$shock_var,
                                lags_shock        = 6,
                                diff_shock        = F,
                                panel_model       = "within",
                                panel_effect      = PANEL_EFFECT,
                                robust_cov        = "vcovSCC",
                                c_exog_data       = colnames(df_subset)[spec$c_var],
                                l_exog_data       = colnames(df_subset)[spec$l_var],
                                lags_exog_data    = 6,
                                c_fd_exog_data    = NULL,
                                l_fd_exog_data    = NULL,
                                lags_fd_exog_data = NULL,
                                confint           = CI,
                                hor               = HOR
                              )
                            }

# Shut down the parallel cluster
stopCluster(cl)

names(results_nirp_indicator) <- names(specs_nirp_indicator_JK)


### 2.2.2 Graphs on Regression Results with NIRP Indicator ---------------------

# Graph 5: Lending Rate: HHI Demeaned x MS x NIRP [INCLUDED] ------------------+

plot_5_hhi_demeaned_ms_nirp <- GG_IRF_ONE(data = results_nirp_indicator$lending_rate_hhi_ms_demeaned_nirp,
                                          hhi_coef = FALSE,
                                          y_lower = -3,
                                          y_upper = 12,
                                          breaks = 2,
                                          title_name = expression("Mortgage Rate: " * delta[1]^h),
                                          time_name = "Months",
                                          y_axis_name = expression(r[t + h] - r[t])
                                          )


# Graph 5.1: Average Effect  HHI Demeaned x MS [INCLUDE] ----------------------+

plot_51_hhi_demeaned_ms_nirp <- GG_IRF_VAR(data = results_nirp_indicator$lending_rate_hhi_ms_demeaned_nirp,
                         var  = "I_J_TOTAL_NIRP",
                         y_lower = -3, 
                         y_upper = 12,
                         breaks = 2,
                         title_name = expression("Mortgage Rate: " * delta[0]^h),
                         time_name = "Months",
                         y_axis_name = expression(r[t+h] - r[t])
                         )



# Graph 6: Lending Rate: MS x NIRP [NOT INCLUDED] -----------------------------+

plot_6_hhi_ms_nirp <- GG_IRF_ONE(data = results_nirp_indicator$lending_rate_ms_nirp,
                                 hhi_coef = FALSE,
                                 y_lower = -2,
                                 y_upper = 4,
                                 breaks = 1,
                                 title_name = "Mortgage Rate",
                                 time_name = "Months",
                                 y_axis_name = expression(r[t + h] - r[t])
                                )

# Graph 7: Log Outstanding Loan Amount: HHI Demeaned x MS x NIRP [INCLUDED] ---+

plot_7_hhi_demeaned_ms_nirp <- GG_IRF_ONE(data = results_nirp_indicator$loan_amount_hhi_ms_demeaned_nirp,
                                          hhi_coef = FALSE,
                                          y_lower = -2,
                                          y_upper = 2,
                                          breaks = 1,
                                          title_name = expression("Mortgage Amount: " * delta[1]^h),
                                          time_name = "Months",
                                          y_axis_name = expression(Y[t + h] - Y[t])
                                         )

# Graph 7.1: Average Effect  HHI Demeaned x MS x NIRP [INCLUDE] ----------------+

plot_71_hhi_demeaned_ms_nirp <- GG_IRF_VAR(data = results_nirp_indicator$loan_amount_hhi_ms_demeaned_nirp,
                                           var  = "I_J_TOTAL_NIRP",
                                           y_lower = -2, 
                                           y_upper = 2,
                                           breaks = 1,
                                           title_name = expression("Mortgage Amount: " * delta[0]^h),
                                           time_name = "Months",
                                           y_axis_name = expression(Y[t+h] - Y[t])
                                           )


# Graph 8: Log Outstanding Loan Amount: HHI x MS x NIRP -----------------------+

plot_8_hhi_ms_nirp <- GG_IRF_ONE(data = results_nirp_indicator$loan_amount_ms_nirp,
                                 hhi_coef = FALSE,
                                 y_lower = -2,
                                 y_upper = 4,
                                 breaks = 1,
                                 title_name = "Mortgage Amount",
                                 time_name = "Months",
                                 y_axis_name = expression(Y[t + h] - Y[t])
                                )


# Final Plot ... --------------------------------------------------------------+

# Graph for Demeaned Baseline
graph_nirp_indicator_demeaned <- (plot_71_hhi_demeaned_ms_nirp$plot + plot_7_hhi_demeaned_ms_nirp$plot) /
                                 (plot_51_hhi_demeaned_ms_nirp$plot + plot_5_hhi_demeaned_ms_nirp$plot) +
                                  plot_annotation(
                                    tag_levels = c("I", "II", "III", "IV"),
                                    theme = theme(
                                      plot.title = element_text(size = 14, hjust = 0.5)
                                    )
                                  )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "05_EA_Panel_E/", "graph_nirp_indicator_demeaned_ea.pdf"),
  plot = graph_nirp_indicator_demeaned,
  width = 12, height = 8, dpi = 300
)
}
### 2.2.3 Appendix for Baseline Regression -------------------------------------

# Appendix Graph 3: Lending Rate: MS  x NIRP ----------------------------------+

plot_appendix_3_hhi_demeaned_ms <- GG_IRF_ONE(data = results_nirp_indicator$lending_rate_ms_nirp,
                                              hhi_coef = FALSE,
                                              y_lower = -2,
                                              y_upper = 2,
                                              breaks = 1,
                                              title_name = "Mortgage Rate",
                                              time_name = "Months",
                                              y_axis_name = expression(r[t + h] - r[t])
                                              )
 
#  Appendix Graph 4: Loan Amount: MS x NIRP -----------------------------------+

plot_appendix_4_hhi_demeaned_ms <- GG_IRF_ONE(data = results_nirp_indicator$loan_amount_ms_nirp,
                                              hhi_coef = FALSE,
                                              y_lower = -2,
                                              y_upper = 2,
                                              breaks = 1,
                                              title_name = "Mortgage Amount",
                                              time_name = "Months",
                                              y_axis_name = expression(Y[t + h] - Y[t])
                                              )

# Appendix Graph 
graph_zlb_indicator_appendix <- (plot_appendix_3_hhi_demeaned_ms$plot + plot_appendix_4_hhi_demeaned_ms$plot) +
  plot_annotation(
    title = "Appendix E",
    tag_levels = c("I", "II"),
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0.5)
    )
  )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "05_EA_Panel_E/", "graph_nirp_indicator_appendix_ea.pdf"),
  plot = graph_zlb_indicator_appendix,
  width = 12, height = 6, dpi = 300
)
}

###############################################################################+
## 2.3 Regression on NIRP Sample ###############################################
###############################################################################+


# Specification for Parallel Computing
specs_nirp_sample_JK <- list(
  ## Lending Rate
  # HHI-Demeaned x MS
  lending_rate_hhi_ms_demeaned_nirp_sample = list(
    shock_var   = "I_HHI_J_TOTAL_demeaned",
    select_cols = c("country", "month", endo_rate, "MP_median_total", "I_HHI_J_TOTAL_demeaned", controls_lending_rate),
    endo = endo_rate,
    l_var = c(4, 6:19),
    c_var = c(4, 6:30),
    sample = nirp_period
  ),
  # HHI x MS
  lending_rate_hhi_ms_nirp_sample = list(
    shock_var   = "I_HHI_J_TOTAL",
    select_cols = c("country", "month", endo_rate, "MP_median_total", "I_HHI_J_TOTAL", controls_lending_rate),
    endo = endo_rate,
    l_var = c(4, 6:19),
    c_var = c(4, 6:30),
    sample = nirp_period
  )  ,
  # MS
  lending_rate_ms_nirp_sample = list(
    shock_var   = "MP_median_total",
    select_cols = c("country", "month", endo_rate,  "MP_median_total", controls_lending_rate),
    endo = endo_rate,
    l_var = c(5:19),
    c_var = c(5:29),
    sample = nirp_period
  ),
  ## Log Mortgage Amount
  # HHI-Demeaned x MS
  loan_amount_hhi_ms_demeaned_nirp_sample = list(
    shock_var   = "I_HHI_J_TOTAL_demeaned",
    select_cols = c("country", "month", "log_hp_total_amount", "MP_median_total", "I_HHI_J_TOTAL_demeaned",  controls_loan_amount),
    endo = "log_hp_total_amount",
    l_var = c(4, 6:19),
    c_var = c(4, 6:30),
    sample = nirp_period
  ),
  # HHI x MS
  loan_amount_hhi_ms_nirp_sample = list(
    shock_var   = "I_HHI_J_TOTAL",
    select_cols = c("country", "month", "log_hp_total_amount", "MP_median_total", "I_HHI_J_TOTAL", controls_loan_amount),
    endo = "log_hp_total_amount",
    l_var = c(4, 6:19),
    c_var = c(4, 6:30),
    sample = nirp_period
  ),
  # MS
  loan_amount_ms_nirp_sample = list(
    shock_var   = "MP_median_total",
    select_cols = c("country", "month", "log_hp_total_amount", "MP_median_total", controls_loan_amount),
    endo = "log_hp_total_amount",
    l_var = c(5:19),
    c_var = c(5:29),
    sample = nirp_period
  )
)


# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run the LP_LIN_PANEL function in parallel for each shock type
# Ensure that all required objects and functions (like df_hp_large, LP_LIN_PANEL, PANEL_EFFECT, CI, HOR, and BITER) 
# are available in the global environment or exported to each worker.
results_nirp_sample <- foreach(spec = specs_nirp_sample_JK, 
                            .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"), 
                            .export = c("df_main", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR")) %dopar% {

                              # Subset the data for the current shock specification
                              df_subset <- df_main  |>  
                                select(all_of(spec$select_cols)) |> 
                                mutate(month = as.numeric(month))
                                
                              
                              # Run the LP_LIN_PANEL function
                              LP_LIN_PANEL(
                                data_set          = df_subset,
                                data_sample       = spec$sample,
                                endog_data        = spec$endo,
                                lags_endog_data   = 6,
                                cumul_mult        = TRUE,
                                shock             = spec$shock_var,
                                lags_shock        = 6,
                                diff_shock        = F,
                                panel_model       = "within",
                                panel_effect      = PANEL_EFFECT,
                                robust_cov        = "tLAHR",
                                c_exog_data       = colnames(df_subset)[spec$c_var],
                                l_exog_data       = colnames(df_subset)[spec$l_var],
                                lags_exog_data    = 6,
                                c_fd_exog_data    = NULL,
                                l_fd_exog_data    = NULL,
                                lags_fd_exog_data = NULL,
                                confint           = CI,
                                hor               = HOR
                              )
                            }

# Shut down the parallel cluster
stopCluster(cl)

names(results_nirp_sample) <- names(specs_nirp_sample_JK)


### 2.3.2 Graphs on Regression Results for NIRP Sample -------------------------

# Graph 9: Lending Rate: HHI Demeaned x MS [INCLUDE] --------------------------+

plot_9_hhi_demeaned_ms_nirp_sampel <- GG_IRF_ONE(data = results_nirp_sample$lending_rate_hhi_ms_demeaned_nirp_sample,
                                                 hhi_coef = FALSE,
                                                 y_lower = -6,
                                                 y_upper = 7,
                                                 breaks = 1,
                                                 title_name = expression("Mortgage Rate: " * delta[1]^h),
                                                 time_name = "Months",
                                                 y_axis_name = expression(r[t + h] - r[t])
                                                 )

# Graph 9.1: Average Effect:  HHI Demeaned x MS [INCLUDE] ----------------------+

plot_91_hhi_demeaned_ms_nirp_sample <- GG_IRF_VAR(data = results_nirp_sample$lending_rate_hhi_ms_demeaned_nirp_sample,
                                           var  = "MP_median_total",
                                           y_lower = -6, 
                                           y_upper = 7,
                                           breaks = 1,
                                           title_name = expression("Mortgage Rate: " * delta[0]^h),
                                           time_name = "Months",
                                           y_axis_name = expression(r[t+h] - r[t])
                                           )


# Graph 10: Lending Rate: MS --------------------------------------------------+

plot_10_hhi_ms_nirp_sample <- GG_IRF_ONE(data = results_nirp_sample$lending_rate_ms_nirp_sample,
                                         hhi_coef = FALSE,
                                         y_lower = -2,
                                         y_upper = 2,
                                         breaks = .5,
                                         title_name = "Mortgage Rate",
                                         time_name = "Months",
                                         y_axis_name = expression(r[t + h] - r[t])
                                         )

# Graph 11: Log Outstanding Loan Amount: HHI Demeaned x MS [INCLUDE] ----------+

plot_11_hhi_demeaned_ms_nirp_sample <- GG_IRF_ONE(data = results_nirp_sample$loan_amount_hhi_ms_demeaned_nirp_sample,
                                                  hhi_coef = FALSE,
                                                  y_lower = -1.5,
                                                  y_upper = 1.5,
                                                  breaks = .5,
                                                  title_name = expression("Mortgage Amount: " * delta[1]^h),
                                                  time_name = "Months",
                                                  y_axis_name = expression(Y[t + h] - Y[t])
                                                  )

# Graph 11.1: Average Effect:  HHI Demeaned x MS [INCLUDE] ----------------------+

plot_111_hhi_demeaned_ms_nirp_sample <- GG_IRF_VAR(data = results_nirp_sample$loan_amount_hhi_ms_demeaned_nirp_sample,
                                                  var  = "MP_median_total",
                                                  y_lower = -1.5, 
                                                  y_upper = 1.5,
                                                  breaks = .5,
                                                  title_name = expression("Mortgage Amount: " * delta[0]^h),
                                                  time_name = "Months",
                                                  y_axis_name = expression(Y[t+h] - Y[t])
                                                  )

# Graph 12: Log Oustanding Loan Amount: MS ------------------------------------+

plot_12_hhi_ms_nirp_sample <- GG_IRF_ONE(data = results_nirp_sample$loan_amount_ms_nirp_sample,
                                         hhi_coef = FALSE,
                                         y_lower = -2,
                                         y_upper = 2,
                                         breaks = .5,
                                         title_name = "Mortgage Rate",
                                         time_name = "Months",
                                         y_axis_name = expression(Y[t + h] - Y[t])
                                         )


# Final Graphs ... ------------------------------------------------------------+

# Graph for Demeaned Baseline
graph_nirp_sample_demeaned <-  (plot_111_hhi_demeaned_ms_nirp_sample$plot + plot_11_hhi_demeaned_ms_nirp_sample$plot) /
                               (plot_91_hhi_demeaned_ms_nirp_sample$plot + plot_9_hhi_demeaned_ms_nirp_sampel$plot) +
                               plot_annotation(
                                 tag_levels = c("I", "II", "III", "IV"),
                                 theme = theme(
                                   plot.title = element_text(size = 14, hjust = 0.5)
                                 )
                               )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "06_EA_Panel_F/", "graph_nirp_sample_demeaned_ea.pdf"),
  plot = graph_nirp_sample_demeaned,
  width = 12, height = 8, dpi = 300
)
}

### 2.3.3 Appendix for Baseline Regression -------------------------------------

# Appendix Graph 5: Lending Rate: MS ------------------------------------------+

# Appendix Graph 
graph_zlb_sample_appendix <- (plot_10_hhi_ms_nirp_sample$plot + plot_12_hhi_ms_nirp_sample$plot) +
                              plot_annotation(
                                title = "Appendix F",
                                tag_levels = c("I", "II"),
                                theme = theme(
                                  plot.title = element_text(size = 14, hjust = 0.5)
                                )
                              )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "06_EA_Panel_F/", "graph_nirp_sample_appendix_ea.pdf"),
  plot = graph_zlb_sample_appendix,
  width = 12, height = 6, dpi = 300
)
}

###############################################################################+
#################################### END #######################################
###############################################################################+