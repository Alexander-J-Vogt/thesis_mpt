# TARGET: 
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

# No NIRLP Sample
df_main_no_nirp <- df_main |> 
  filter(d_dfr_nirp == 0) |> 
  mutate(month = as.numeric(month))


###############################################################################+
# 2. Local Projection ##########################################################
###############################################################################+

## General Specification - Regression Details ---------------------------------+

# Panel Effect
PANEL_EFFECT <- "individual"

# Siginficance Level
CI <- 1.96

# Time Horizon
HOR <- 12

## General Specification - Controls -------------------------------------------+

# 
controls_lending_rate <- c("log_cr", "log_dl", 
                           "ur", "hicp", "reer", "commodity_index", "exr", "gdp",
                           "hicp_ea", "gdp_ea", 
                           "d_countries_arm")

controls_loan_amount <-  c("log_cr", "log_dl", 
                           "ur_deci", "hicp", "reer", "commodity_index", "exr", "gdp",
                           "hicp_ea", "gdp_ea", 
                           "d_countries_arm")


###############################################################################+
## 2.1 Baseline ################################################################
###############################################################################+

### 2.1.1 Regression for Baseline Specification --------------------------------

# Define specifications for each shock type in a list
specs_baseline <- list(
  lending_rate_hhi_ms_demeaned = list(
    shock_var   = "I_HHI_A_TOTAL_demeaned",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_TOTAL_demeaned",  controls_lending_rate)
  ),
  lending_rate_hhi_ms = list(
    shock_var   = "I_HHI_A_TOTAL",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_TOTAL", controls_lending_rate)
  )  ,
  lending_rate_ms = list(
    shock_var   = "altavilla_total",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "altavilla_total", controls_lending_rate)
  )  ,
  loan_amount_hhi_ms_demeaned = list(
    shock_var   = "I_HHI_A_TOTAL_demeaned",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_TOTAL_demeaned",  controls_loan_amount)
  ),
  loan_amount_hhi_ms = list(
    shock_var   = "I_HHI_A_TOTAL",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_TOTAL", controls_loan_amount)
  ),
  loan_amount_ms = list(
    shock_var   = "altavilla_total",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "altavilla_total", controls_loan_amount)
  )
  )

# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run the LP_LIN_PANEL function in parallel for each shock type
# Ensure that all required objects and functions (like df_hp_large, LP_LIN_PANEL, PANEL_EFFECT, CI, HOR, and BITER) 
# are available in the global environment or exported to each worker.
results_baseline <- foreach(spec = shock_specs, 
                            .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"), 
                            .export = c("df_main", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR", "BITER")) %dopar% {
                          
                            # Subset the data for the current shock specification
                            df_subset <- df_main %>% 
                              select(all_of(spec$select_cols))
                          
                            # Run the LP_LIN_PANEL function
                            LP_LIN_PANEL(
                              data_set          = df_subset,
                              data_sample       = "Full",
                              endog_data        = "log_hp_total_amount",
                              lags_endog_data   = 6,
                              cumul_mult        = TRUE,
                              shock             = spec$shock_var,
                              lags_shock        = 6,
                              diff_shock        = FALSE,
                              panel_model       = "within",
                              panel_effect      = PANEL_EFFECT,
                              robust_cov        = "vcovSCC",
                              # robust_maxlag     = 4,
                              # robust_type       = "HC1",
                              c_exog_data       = colnames(df_subset)[c(4, 6:16)],
                              l_exog_data       = colnames(df_subset)[c(4, 6:15)],
                              lags_exog_data    = 6, #6,
                              c_fd_exog_data    = NULL, #colnames(df_subset)[c(4, 6:15)],
                              l_fd_exog_data    = NULL, #colnames(df_subset)[c(4, 6:15)],
                              lags_fd_exog_data = NULL,
                              confint           = CI,
                              hor               = HOR,
                              biter             = BITER
                           )
                         }

# Shut down the parallel cluster
stopCluster(cl)

names(results_baseline) <- names(specs_baseline)


### 2.1.2 Graphs on Regression Results for Baseline Specification --------------

# Graph 1: Lending Rate: HHI Demeaned x MS ------------------------------------+

plot_1_hhi_demeaned_ms <- GG_IRF_ONE(data = results_baseline$lending_rate_hhi_ms_demeaned,
                                     hhi_coef = FALSE,
                                     y_lower = Y_LOWER,
                                     y_upper = Y_UPPER,
                                     breaks = BREAKS_GRAPH,
                                     title_name = "Lending Rate",
                                     time_name = "Months",
                                     y_axis_name = expression(R[t + h] - R[t])
                                     )

# Graph 2: Lending Rate: HHI x MS ---------------------------------------------+

plot_2_hhi_ms <- GG_IRF_ONE(data = results_baseline$lending_rate_hhi_ms,
                            hhi_coef = FALSE,
                            y_lower = Y_LOWER,
                            y_upper = Y_UPPER,
                            breaks = BREAKS_GRAPH,
                            title_name = "Log Loan Amount",
                            time_name = "Months",
                            y_axis_name = expression(Y[t + h] - Y[t])
                            )

# Graph 3: Log Oustanding Loan Amount: HHI Demeane x MS -----------------------+

plot_3_hhi_demeaned_ms <- GG_IRF_ONE(data = results_baseline$loan_amount_hhi_ms_demeaned,
                                     hhi_coef = FALSE,
                                     y_lower = Y_LOWER,
                                     y_upper = Y_UPPER,
                                     breaks = BREAKS_GRAPH,
                                     title_name = "Lending Rate",
                                     time_name = "Months",
                                     y_axis_name = expression(R[t + h] - R[t])
                                     )

# Graph 4: Log Oustanding Loan Amount: HHI x MS -------------------------------+

plot_4_hhi_ms <- GG_IRF_ONE(data = results_baseline$lending_rate_hhi_ms,
                            hhi_coef = FALSE,
                            y_lower = Y_LOWER,
                            y_upper = Y_UPPER,
                            breaks = BREAKS_GRAPH,
                            title_name = "Lending Rate",
                            time_name = "Months",
                            y_axis_name = expression(Y[t + h] - Y[t])
                            )


# Final Graphs ... ------------------------------------------------------------+

graph_baseline_demeaned <- (plot_1_hhi_demeaned_ms + plot_2_hhi_ms) +
                            plot_annotation(
                              title = "Impulse Response Function on Demeaned HHI \u00D7 Monetary Shock",
                              theme = theme(
                                title = element_text(size = 10, hjust = .5))
                              )


graph_baseline_not_demeaned <- (plot_3_hhi_demeaned_ms + plot_4_hhi_ms) +
                            plot_annotation(
                              title = "Impulse Response Function on Demeaned HHI \u00D7 Monetary Shock",
                              theme = theme(
                                title = element_text(size = 10, hjust = .5))
                            )


### 2.1.3 Appendix for Baseline Regression -------------------------------------

# Appendix Graph 1: Lending Rate: MS ---------------------------------------------+

plot_appendix_1_hhi_demeaned_ms <- GG_IRF_ONE(data = results_baseline$lending_rate_ms,
                                     hhi_coef = FALSE,
                                     y_lower = Y_LOWER,
                                     y_upper = Y_UPPER,
                                     breaks = BREAKS_GRAPH,
                                     title_name = "Lending Rate",
                                     time_name = "Months",
                                     y_axis_name = expression(R[t + h] - R[t])
)

# Graph 2: Lending Rate: HHI x MS ---------------------------------------------+

plot_appendix_1_hhi_demeaned_ms <- GG_IRF_ONE(data = results_baseline$loan_amount_ms,
                                              hhi_coef = FALSE,
                                              y_lower = Y_LOWER,
                                              y_upper = Y_UPPER,
                                              breaks = BREAKS_GRAPH,
                                              title_name = "Log Loan Amount",
                                              time_name = "Months",
                                              y_axis_name = expression(Y[t + h] - Y[t])
                                              )





















## 2.1 Full Sample #############################################################

#' In the first section, the dependend variable is regressed on the three-fold 
#' interation term and than on a two fold interation term which excludes the 
#' NIRP period indicator.

### 2.1.1 Log Loan Amount ------------------------------------------------------

# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)


controls <- c("log_cr", "log_dl", 
              "ur", "hicp", "reer", "commodity_index", "exr", "gdp",
              "hicp_ea", "gdp_ea", 
              "d_countries_arm")

# Define specifications for each shock type in a list
shock_specs <- list(
  total_nirp = list(
    shock_var   = "I_HHI_A_TOTAL_NIRP",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_TOTAL_NIRP",  controls)
  ),
  total_original = list(
    shock_var   = "I_HHI_A_TOTAL",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_TOTAL", controls)
  )  ,
  total_above = list(
    shock_var   = "I_HHI_A_TOTAL_ABOVE",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_TOTAL_ABOVE", controls)
  ),
  positive_nirp = list(
    shock_var   = "I_HHI_A_POS_NIRP",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_POS_NIRP", controls)
  ),
  positive_original = list(
    shock_var   = "I_HHI_A_POS",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_POS", controls)
  ),
  positive_above = list(
    shock_var   = "I_HHI_A_POS_ABOVE",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_POS_ABOVE", controls)
  ),
  negative_nirp = list(
    shock_var   = "I_HHI_A_NEG_NIRP",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_NEG_NIRP", controls)
  ),
  negative_original = list(
    shock_var   = "I_HHI_A_NEG",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_NEG",  controls)
  ),
  negative_above = list(
    shock_var   = "I_HHI_A_NEG_ABOVE",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_NEG_ABOVE",  controls)
  )
  )



# Run the LP_LIN_PANEL function in parallel for each shock type
# Ensure that all required objects and functions (like df_hp_large, LP_LIN_PANEL, PANEL_EFFECT, CI, HOR, and BITER) 
# are available in the global environment or exported to each worker.
results_full <- foreach(spec = shock_specs, 
                   .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"), 
                   .export = c("df_main", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR", "BITER")) %dopar% {
                     
                     # Subset the data for the current shock specification
                     df_subset <- df_main %>% 
                       select(all_of(spec$select_cols))
                     
                     # Run the LP_LIN_PANEL function
                     LP_LIN_PANEL(
                       data_set          = df_subset,
                       data_sample       = "Full",
                       endog_data        = "log_hp_total_amount",
                       lags_endog_data   = 6,
                       cumul_mult        = TRUE,
                       shock             = spec$shock_var,
                       lags_shock        = 6,
                       diff_shock        = FALSE,
                       panel_model       = "within",
                       panel_effect      = PANEL_EFFECT,
                       robust_cov        = "vcovSCC",
                       # robust_maxlag     = 4,
                       # robust_type       = "HC1",
                       c_exog_data       = colnames(df_subset)[c(4, 6:16)],
                       l_exog_data       = colnames(df_subset)[c(4, 6:15)],
                       lags_exog_data    = 6, #6,
                       c_fd_exog_data    = NULL, #colnames(df_subset)[c(4, 6:15)],
                       l_fd_exog_data    = NULL, #colnames(df_subset)[c(4, 6:15)],
                       lags_fd_exog_data = NULL,
                       confint           = CI,
                       hor               = HOR,
                       biter             = BITER
                     )
                   }

# Shut down the parallel cluster
stopCluster(cl)

names(results_full) <- names(shock_specs)

Y_LOWER <- -.01
Y_UPPER <- .02
BREAKS_GRAPH <- .005

# Total MS Shock ---
plot_full_total_ms_nirp <- GG_IRF_ONE(data = results_full$total_nirp,
                                hhi_coef = FALSE,
                                y_lower = Y_LOWER,
                                y_upper = Y_UPPER,
                                breaks = BREAKS_GRAPH,
                                title_name = "IRF of Lending Amount: Total MS - NIRP"
                                )

plot_full_total_ms_original <- GG_IRF_ONE(data = results_full$total_original,
                                      hhi_coef = FALSE,
                                      y_lower = Y_LOWER,
                                      y_upper = Y_UPPER,
                                      breaks = BREAKS_GRAPH,
                                      title_name = "IRF of Lending Amount: Total MS - Reference"
)

plot_full_total_ms_comparison <- GG_IRF_TWO(data1 = results_full$total_nirp,
                                          data2 = results_full$total_original,
                                          data_name = c("NIRP", "Original"),
                                          hhi_coef = FALSE,
                                          y_lower = Y_LOWER,
                                          y_upper = Y_UPPER,
                                          breaks = BREAKS_GRAPH,
                                          name = "IRF of Lending Amount: Total MS"
)



#  Positive MS Shock ---
plot_full_pos_ms_nirp <- GG_IRF_ONE(data = results_full$positive_nirp,
                                    hhi_coef = FALSE,
                                    y_lower = Y_LOWER,
                                    y_upper = .03,
                                    breaks = BREAKS_GRAPH,
                                    title_name = "IRF of Lending Amount: Positive MS - NIRP"
)

plot_full_pos_ms_original <- GG_IRF_ONE(data = results_full$positive_original,
                                        hhi_coef = FALSE,
                                        y_lower = Y_LOWER,
                                        y_upper = Y_UPPER,
                                        breaks = BREAKS_GRAPH,
                                        title_name = "IRF of Lending Amount: Positive MS - Original"
)

plot_full_pos_ms_comparison <- GG_IRF_TWO(data1 = results_full$positive_nirp,
                                          data2 = results_full$positive_original,
                                          data_name = c("NIRP", "Original"),
                                          hhi_coef = FALSE,
                                          y_lower = Y_LOWER,
                                          y_upper = Y_UPPER,
                                          breaks = BREAKS_GRAPH,
                                          name = "IRF of Lending Amount: Positive MS"
)

# Negative MS Shock
plot_full_neg_ms_nirp <- GG_IRF_ONE(data = results_full$negative_nirp,
                                    hhi_coef = FALSE,
                                    y_lower = Y_LOWER,
                                    y_upper = Y_UPPER,
                                    breaks = BREAKS_GRAPH,
                                    title_name = "IRF of Lending Amount: Negative MS - NIRP"
)

plot_full_neg_ms_original <- GG_IRF_ONE(data = results_full$negative_original,
                                        hhi_coef = FALSE,
                                        y_lower = Y_LOWER,
                                        y_upper = Y_UPPER,
                                        breaks = BREAKS_GRAPH,
                                        title_name = "IRF of Lending Amount: Negative MS - Original"
)

plot_full_total_ms_comparison <- GG_IRF_TWO(data1 = results_full$negative_nirp,
                                            data2 = results_full$negative_original,
                                            data_name = c("NIRP", "Original"),
                                            hhi_coef = FALSE,
                                            y_lower = Y_LOWER,
                                            y_upper = Y_UPPER,
                                            breaks = BREAKS_GRAPH,
                                            name = "IRF of Lending Amount: Negative MS"
)


## Comparison between NIRP and Normal Period- ---------------------------------+

Y_LOWER <- -.03
Y_UPPER <- .03
BREAKS_GRAPH <- .01

plot_full_total_ms_comparison_amount_posneg <- GG_IRF_TWO(data1 = results_full$total_nirp,
                                                        data2 = results_full$total_above,
                                                        data_name = c("NIRP", "Normal"),
                                                        hhi_coef = F,
                                                        y_lower = Y_LOWER,
                                                        y_upper = Y_UPPER,
                                                        breaks = BREAKS_GRAPH,
                                                        name = "IRF: Total Monetary Shock"
)

plot_full_pos_ms_comparison_amount_posneg <- GG_IRF_TWO(data1 = results_full$positive_nirp,
                                                      data2 = results_full$positive_above,
                                                      data_name = c("NIRP", "Normal"),
                                                      hhi_coef = F,
                                                      y_lower = Y_LOWER,
                                                      y_upper = Y_UPPER,
                                                      breaks = BREAKS_GRAPH,
                                                      name = "IRF: Positive MS"
)

plot_full_neg_ms_comparison_amount_posneg <- GG_IRF_TWO(data1 = results_full$negative_nirp,
                                                      data2 = results_full$negative_above,
                                                      data_name = c("NIRP", "Normal"),
                                                      hhi_coef = F,
                                                      y_lower = Y_LOWER,
                                                      y_upper = Y_UPPER,
                                                      breaks = BREAKS_GRAPH,
                                                      name = "IRF: Negtive MS"
)

# Graph - Comparison NIRP vs Not NIRP Period

plot_rate_nirp_no_nirp <- plot_full_total_ms_comparison_amount_posneg$plot /  
                          plot_full_pos_ms_comparison_amount_posneg$plot   /
                          plot_full_neg_ms_comparison_amount_posneg$plot +
                          plot_annotation(title = "Log Loan Amount - NIRP vs No NIRP - Altavilla - SCC - without Constant - Country FE",
                                          theme = theme(plot.title = element_text(size = 8, hjust = .5)))

ggsave(
  filename = paste0(FIGURE, "Loan_Amount_SCC_Altavilla_Demeaned_withoutAVGCoef_NIRPvsNORMAL_CountryFE.pdf"),
  plot_rate_nirp_no_nirp, 
  width = 8, 
  height = 12, 
  dpi = 300
)




### 2.1.2 Interest Rate for Mortgages - Total ----------------------------------


# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

controls <- c("log_cr", "log_dl", 
              "ur", "hicp", "reer", "commodity_index", "exr", "gdp",
              "hicp_ea", "gdp_ea", 
              "d_countries_arm")

# Define specifications for each shock type in a list
shock_specs <- list(
  total_nirp = list(
    shock_var   = "I_HHI_A_TOTAL_NIRP",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_TOTAL_NIRP",  controls)
  ),
  total_original = list(
    shock_var   = "I_HHI_A_TOTAL",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A", controls)
  ),
  positive_nirp = list(
    shock_var   = "I_HHI_A_POS_NIRP",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_POS_NIRP", controls)
  ),
  positive_original = list(
    shock_var   = "I_HHI_A_POS",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_POS", controls)
  ),
  negative_nirp = list(
    shock_var   = "I_HHI_A_NEG_NIRP",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_NEG_NIRP", controls)
  ),
  negative_original = list(
    shock_var   = "I_HHI_A_NEG",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_NEG",  controls)
  ), 
  total_above0 = list(
    shock_var   = "I_HHI_A_TOTAL_ABOVE",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_TOTAL_ABOVE", controls)
  ),
  positive_above0 = list(
    shock_var   = "I_HHI_A_POS_ABOVE",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_POS_ABOVE", controls)
  ),
  negative_above0 = list(
    shock_var   = "I_HHI_A_NEG_ABOVE",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_NEG_ABOVE",  controls)
  )
  
)



# Run the LP_LIN_PANEL function in parallel for each shock type
# Ensure that all required objects and functions (like df_hp_large, LP_LIN_PANEL, PANEL_EFFECT, CI, HOR, and BITER) 
# are available in the global environment or exported to each worker.
results_full_rates <- foreach(spec = shock_specs, 
                        .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"), 
                        .export = c("df_main", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR", "BITER")) %dopar% {
                          
                          # Subset the data for the current shock specification
                          df_subset <- df_main %>% 
                            select(all_of(spec$select_cols))
                          
                          # Run the LP_LIN_PANEL function
                          LP_LIN_PANEL(
                            data_set          = df_subset,
                            data_sample       = "Full",
                            endog_data        = "lending_rate_total",
                            lags_endog_data   = 6,
                            cumul_mult        = TRUE,
                            shock             = spec$shock_var,
                            lags_shock        = 6,
                            diff_shock        = FALSE,
                            panel_model       = "within",
                            panel_effect      = PANEL_EFFECT,
                            robust_cov        = "vcovSCC",
                            # robust_maxlag     = 4,
                            # robust_type       = "HC1",
                            c_exog_data       = colnames(df_subset)[c(4, 6:16)],
                            l_exog_data       = colnames(df_subset)[c(4, 6:15)],
                            lags_exog_data    = 6, #6,
                            c_fd_exog_data    = NULL, #colnames(df_subset)[c(4, 6:15)],
                            l_fd_exog_data    = NULL, #colnames(df_subset)[c(4, 6:15)],
                            lags_fd_exog_data = NULL,
                            confint           = CI,
                            hor               = HOR,
                            biter             = BITER
                          )
                        }

# Shut down the parallel cluster
stopCluster(cl)

names(results_full_rates) <- names(shock_specs)

Y_LOWER <- -12
Y_UPPER <- 12
BREAKS_GRAPH <- 2

# Total MS Shock ---
plot_full_total_ms_nirp_rate_total <- GG_IRF_ONE(data = results_full_rates$total_nirp,
                                      hhi_coef = FALSE,
                                      y_lower = Y_LOWER,
                                      y_upper = Y_UPPER,
                                      breaks = BREAKS_GRAPH,
                                      title_name = "IRF of Lending Amount: Total MS - NIRP"
)

plot_full_total_ms_original_rate_total <- GG_IRF_ONE(data = results_full_rates$total_original,
                                          hhi_coef = FALSE,
                                          y_lower = Y_LOWER,
                                          y_upper = Y_UPPER,
                                          breaks = BREAKS_GRAPH,
                                          title_name = "IRF of Lending Amount: Total MS - Reference"
)

plot_full_total_ms_comparison_rate_total <- GG_IRF_TWO(data1 = results_full_rates$total_nirp,
                                            data2 = results_full_rates$total_original,
                                            data_name = c("NIRP", "Original"),
                                            hhi_coef = FALSE,
                                            y_lower = Y_LOWER,
                                            y_upper = Y_UPPER,
                                            breaks = BREAKS_GRAPH,
                                            name = "IRF of Lending Amount: Total MS"
)



#  Positive MS Shock ---
plot_full_pos_ms_nirp_rate_total <- GG_IRF_ONE(data = results_full_rates$positive_nirp,
                                    hhi_coef = FALSE,
                                    y_lower = Y_LOWER,
                                    y_upper = .03,
                                    breaks = BREAKS_GRAPH,
                                    title_name = "IRF of Lending Amount: Positive MS - NIRP"
)

plot_full_pos_ms_original_rate_total <- GG_IRF_ONE(data = results_full_rates$positive_original,
                                        hhi_coef = FALSE,
                                        y_lower = Y_LOWER,
                                        y_upper = Y_UPPER,
                                        breaks = BREAKS_GRAPH,
                                        title_name = "IRF of Lending Amount: Positive MS - Original"
)

plot_full_pos_ms_comparison_rate_total <- GG_IRF_TWO(data1 = results_full_rates$positive_nirp,
                                          data2 = results_full_rates$positive_original,
                                          data_name = c("NIRP", "Original"),
                                          hhi_coef = FALSE,
                                          y_lower = Y_LOWER,
                                          y_upper = Y_UPPER,
                                          breaks = BREAKS_GRAPH,
                                          name = "IRF of Lending Amount: Positive MS"
)

# Negative MS Shock
plot_full_neg_ms_nirp_rate_total <- GG_IRF_ONE(data = results_full_rates$negative_nirp,
                                    hhi_coef = FALSE,
                                    y_lower = Y_LOWER,
                                    y_upper = Y_UPPER,
                                    breaks = BREAKS_GRAPH,
                                    title_name = "IRF of Lending Amount: Negative MS - NIRP"
)

plot_full_neg_ms_original_rate_total <- GG_IRF_ONE(data = results_full_rates$negative_original,
                                        hhi_coef = FALSE,
                                        y_lower = Y_LOWER,
                                        y_upper = Y_UPPER,
                                        breaks = BREAKS_GRAPH,
                                        title_name = "IRF of Lending Amount: Negative MS - Original"
)

plot_full_neg_ms_comparison_rate_total <- GG_IRF_TWO(data1 = results_full_rates$negative_nirp,
                                            data2 = results_full_rates$negative_original,
                                            data_name = c("NIRP", "Original"),
                                            hhi_coef = FALSE,
                                            y_lower = Y_LOWER,
                                            y_upper = Y_UPPER,
                                            breaks = BREAKS_GRAPH,
                                            name = "IRF of Lending Amount: Negative MS"
)

## MAIN GRAPH - Comparison Nirp vs FUll Period

plot_rate <- plot_full_total_ms_comparison_rate_total$plot /
             plot_full_pos_ms_comparison_rate_total$plot   /
             plot_full_neg_ms_comparison_rate_total$plot   + 
             plot_annotation(title = "Lending Rate - SCC - Jarocinski - Demeaned",
                             theme = theme(plot.title = element_text(size = 10, hjust = .5)))

ggsave(
  filename = paste0(FIGURE, "Lending_Rate_SCC_Jarocinski_Demeaned.pdf"),
  plot_rate, 
  width = 8, 
  height = 12, 
  dpi = 300
)


## Compare --------------------------------------------------------------------+

Y_LOWER <- -1.5
Y_UPPER <- 1
BREAKS_GRAPH <- .5

plot_full_total_ms_comparison_rate_posneg <- GG_IRF_TWO(data1 = results_full_rates$total_nirp,
                                                       data2 = results_full_rates$total_above0,
                                                       data_name = c("NIRP", "Normal"),
                                                       hhi_coef = T,
                                                       y_lower = Y_LOWER,
                                                       y_upper = Y_UPPER,
                                                       breaks = BREAKS_GRAPH,
                                                       name = "IRF: Total Monetary Shock"
)

plot_full_pos_ms_comparison_rate_posneg <- GG_IRF_TWO(data1 = results_full_rates$positive_nirp,
                                                        data2 = results_full_rates$positive_above0,
                                                        data_name = c("NIRP", "Normal"),
                                                        hhi_coef = T,
                                                        y_lower = Y_LOWER,
                                                        y_upper = Y_UPPER,
                                                        breaks = BREAKS_GRAPH,
                                                        name = "IRF: Positive MS"
)

plot_full_neg_ms_comparison_rate_posneg <- GG_IRF_TWO(data1 = results_full_rates$negative_nirp,
                                                      data2 = results_full_rates$negative_above0,
                                                      data_name = c("NIRP", "Normal"),
                                                      hhi_coef = T,
                                                      y_lower = Y_LOWER,
                                                      y_upper = Y_UPPER,
                                                      breaks = BREAKS_GRAPH,
                                                      name = "IRF: Negtive MS"
)

# Graph - Comparison NIRP vs Not NIRP Period

plot_rate_nirp_no_nirp <- plot_full_total_ms_comparison_rate_posneg$plot /  
                          plot_full_pos_ms_comparison_rate_posneg$plot   /
                          plot_full_neg_ms_comparison_rate_posneg$plot +
                          plot_annotation(title = "Lending Rate - NIRP vs No NIRP - Altavilla - SCC - with Constant - Demeaned",
                                          theme = theme(plot.title = element_text(size = 8, hjust = .5)))

ggsave(
  filename = paste0(FIGURE, "Lending_Rate_SCC_Altavilla_Demeaned_withAVGCoef_NIRPvsNORMAL.pdf"),
  plot_rate_nirp_no_nirp, 
  width = 8, 
  height = 12, 
  dpi = 300
)

###############################################################################+
## 2.2 NIRP vs No-NIRP Period ##################################################
###############################################################################+

### 2.2.1 Log Loan Amount ------------------------------------------------------

# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)


controls <- c("log_cr", "log_dl", "log_tl", 
              "ur", "hicp", "reer", "commodity_index", "exr",
              "hicp_ea", "gdp_ea", 
              "d_fincrisis", "d_countries_arm")

# Define specifications for each shock type in a list
shock_specs <- list(
  total_nirp = list(
    shock_var   = "I_HHI_A_TOTAL_NIRP",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_TOTAL_NIRP",  controls),
    period = df_main_nirp$month
  ),
  total_original = list(
    shock_var   = "I_HHI_A_TOTAL",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_TOTAL", controls),
    period = df_main_no_nirp$month
  ),
  positive_nirp = list(
    shock_var   = "I_HHI_A_POS_NIRP",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_POS_NIRP", controls),
    period = df_main_nirp$month
  ),
  positive_original = list(
    shock_var   = "I_HHI_A_POS",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_POS", controls),
    period = df_main_no_nirp$month
  ),
  negative_nirp = list(
    shock_var   = "I_HHI_A_NEG_NIRP",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_NEG_NIRP", controls),
    period = df_main_nirp$month
  ),
  negative_nirp = list(
    shock_var   = "I_HHI_A_NEG",
    select_cols = c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_NEG",  controls),
    period = df_main_no_nirp$month
  )
)



# Run the LP_LIN_PANEL function in parallel for each shock type
# Ensure that all required objects and functions (like df_hp_large, LP_LIN_PANEL, PANEL_EFFECT, CI, HOR, and BITER) 
# are available in the global environment or exported to each worker.
results_sample_amonut <- foreach(spec = shock_specs, 
                        .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"), 
                        .export = c("df_main", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR", "BITER")) %dopar% {
                          
                          # Subset the data for the current shock specification
                          df_subset <- df_main %>% 
                            select(all_of(spec$select_cols))
                          
                          # Run the LP_LIN_PANEL function
                          LP_LIN_PANEL(
                            data_set          = df_subset,
                            data_sample       = spec$period,
                            endog_data        = "log_hp_total_amount",
                            lags_endog_data   = 1,
                            cumul_mult        = TRUE,
                            shock             = spec$shock_var,
                            lags_shock        = 1,
                            diff_shock        = TRUE,
                            panel_model       = "within",
                            panel_effect      = PANEL_EFFECT,
                            robust_cov        = "vcovSCC",
                            # robust_maxlag     = 4,
                            # robust_type       = "HC1",
                            c_exog_data       = colnames(df_subset)[c(4, 6:17)],
                            l_exog_data       = colnames(df_subset)[c(4, 6:15)],
                            lags_exog_data    = 6, #6,
                            c_fd_exog_data    = NULL, #colnames(df_subset)[c(4, 6:15)],
                            l_fd_exog_data    = NULL, #colnames(df_subset)[c(4, 6:15)],
                            lags_fd_exog_data = NULL,
                            confint           = CI,
                            hor               = HOR,
                            biter             = BITER
                          )
                        }

# Shut down the parallel cluster
stopCluster(cl)

names(results_sample_amonut) <- names(shock_specs)

Y_LOWER <- -.01
Y_UPPER <- .02
BREAKS_GRAPH <- .005

# Total MS Shock ---
plot_sample_total_ms_nirp_rate_total <- GG_IRF_ONE(data = results_sample_amonut$total_nirp,
                                                 hhi_coef = FALSE,
                                                 y_lower = Y_LOWER,
                                                 y_upper = Y_UPPER,
                                                 breaks = BREAKS_GRAPH,
                                                 title_name = "IRF of Lending Amount: Total MS - NIRP"
)

plot_sample_total_ms_original_rate_total <- GG_IRF_ONE(data = results_sample_amonut$total_original,
                                                     hhi_coef = FALSE,
                                                     y_lower = Y_LOWER,
                                                     y_upper = Y_UPPER,
                                                     breaks = BREAKS_GRAPH,
                                                     title_name = "IRF of Lending Amount: Total MS - Reference"
)

plot_sample_sample_ms_comparison_rate_total <- GG_IRF_TWO(data1 = results_sample_amonut$total_nirp,
                                                       data2 = results_sample_amonut$total_original,
                                                       data_name = c("NIRP", "Original"),
                                                       hhi_coef = FALSE,
                                                       y_lower = Y_LOWER,
                                                       y_upper = Y_UPPER,
                                                       breaks = BREAKS_GRAPH,
                                                       name = "IRF of Lending Amount: Total MS"
)



#  Positive MS Shock ---
plot_sample_pos_ms_nirp_rate_total <- GG_IRF_ONE(data = results_sample_amonut$positive_nirp,
                                               hhi_coef = FALSE,
                                               y_lower = Y_LOWER,
                                               y_upper = .03,
                                               breaks = BREAKS_GRAPH,
                                               title_name = "IRF of Lending Amount: Positive MS - NIRP"
)

plot_sample_pos_ms_original_rate_total <- GG_IRF_ONE(data = results_sample_amonut$positive_original,
                                                   hhi_coef = FALSE,
                                                   y_lower = Y_LOWER,
                                                   y_upper = Y_UPPER,
                                                   breaks = BREAKS_GRAPH,
                                                   title_name = "IRF of Lending Amount: Positive MS - Original"
)

plot_sample_pos_ms_comparison_rate_total <- GG_IRF_TWO(data1 = results_sample_amonut$positive_nirp,
                                                     data2 = results_sample_amonut$positive_original,
                                                     data_name = c("NIRP", "Original"),
                                                     hhi_coef = FALSE,
                                                     y_lower = Y_LOWER,
                                                     y_upper = Y_UPPER,
                                                     breaks = BREAKS_GRAPH,
                                                     name = "IRF of Lending Amount: Positive MS"
)

# Negative MS Shock
plot_sample_neg_ms_nirp_rate_total <- GG_IRF_ONE(data = results_sample_amonut$negative_nirp,
                                               hhi_coef = FALSE,
                                               y_lower = Y_LOWER,
                                               y_upper = Y_UPPER,
                                               breaks = BREAKS_GRAPH,
                                               title_name = "IRF of Lending Amount: Negative MS - NIRP"
)

plot_sample_neg_ms_original_rate_total <- GG_IRF_ONE(data = results_sample_amonut$negative_original,
                                                   hhi_coef = FALSE,
                                                   y_lower = Y_LOWER,
                                                   y_upper = Y_UPPER,
                                                   breaks = BREAKS_GRAPH,
                                                   title_name = "IRF of Lending Amount: Negative MS - Original"
)

plot_sample_total_ms_comparison_rate_total <- GG_IRF_TWO(data1 = results_sample_amonut$negative_nirp,
                                                       data2 = results_sample_amonut$negative_original,
                                                       data_name = c("NIRP", "Original"),
                                                       hhi_coef = FALSE,
                                                       y_lower = Y_LOWER,
                                                       y_upper = Y_UPPER,
                                                       breaks = BREAKS_GRAPH,
                                                       name = "IRF of Lending Amount: Negative MS"
)



### 2.2.2 Interest Rates for Mortgages - Total ---------------------------------

# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)


controls <- c("log_cr", "log_dl", "log_tl", 
              "ur", "hicp", "reer", "commodity_index", "exr",
              "hicp_ea", "gdp_ea", 
              "d_countries_arm")

# Define specifications for each shock type in a list
shock_specs <- list(
  total_nirp = list(
    shock_var   = "I_HHI_A_TOTAL_NIRP",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_TOTAL_NIRP",  controls),
    period = df_main_nirp$month
  ),
  total_original = list(
    shock_var   = "I_HHI_A_TOTAL",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_TOTAL", controls),
    period = df_main_no_nirp$month
  ),
  positive_nirp = list(
    shock_var   = "I_HHI_A_POS_NIRP",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_POS_NIRP", controls),
    period = df_main_nirp$month
  ),
  positive_original = list(
    shock_var   = "I_HHI_A_POS",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_POS", controls),
    period = df_main_no_nirp$month
  ),
  negative_nirp = list(
    shock_var   = "I_HHI_A_NEG_NIRP",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_NEG_NIRP", controls),
    period = df_main_nirp$month
  ),
  negative_nirp = list(
    shock_var   = "I_HHI_A_NEG",
    select_cols = c("country", "month", "lending_rate_total", "hhi_ci_total_assets", "I_HHI_A_NEG",  controls),
    period = df_main_no_nirp$month
  )
)
# 
shock_specs$total_nirp$select_cols

df_subset <- df_main %>%
  select(all_of(shock_specs$total_nirp$select_cols))
dlookr::describe(df_subset)
# Run the LP_LIN_PANEL function in parallel for each shock type
# Ensure that all required objects and functions (like df_hp_large, LP_LIN_PANEL, PANEL_EFFECT, CI, HOR, and BITER) 
# are available in the global environment or exported to each worker.
results_sample_rates <- foreach(spec = shock_specs, 
                                 .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"), 
                                 .export = c("df_main", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR", "BITER")) %dopar% {
                                   
                                   # Subset the data for the current shock specification
                                   df_subset <- df_main %>% 
                                     select(all_of(spec$select_cols))
                                   
                                   # Run the LP_LIN_PANEL function
                                   LP_LIN_PANEL(
                                     data_set          = df_subset,
                                     data_sample       = spec$period,
                                     endog_data        = "lending_rate_total",
                                     lags_endog_data   = 6,
                                     cumul_mult        = TRUE,
                                     shock             = spec$shock_var,
                                     lags_shock        = 6,
                                     diff_shock        = TRUE,
                                     panel_model       = "within",
                                     panel_effect      = PANEL_EFFECT,
                                     robust_cov        = "vcovSCC",
                                     # robust_maxlag     = 4,
                                     # robust_type       = "HC1",
                                     c_exog_data       = colnames(df_subset)[c(4, 6:16)],
                                     l_exog_data       = colnames(df_subset)[c(4, 6:15)],
                                     lags_exog_data    = 6, #6,
                                     c_fd_exog_data    = NULL, #colnames(df_subset)[c(4, 6:15)],
                                     l_fd_exog_data    = NULL, #colnames(df_subset)[c(4, 6:15)],
                                     lags_fd_exog_data = NaN,
                                     confint           = CI,
                                     hor               = HOR,
                                     biter             = BITER
                                   )
                                 }

# Shut down the parallel cluster
stopCluster(cl)

names(results_sample_rates) <- names(shock_specs)


Y_LOWER <- -.01
Y_UPPER <- .02
BREAKS_GRAPH <- .005

# Total MS Shock ---
plot_sample_total_ms_nirp_rate_total <- GG_IRF_ONE(data = results_sample_rates$total_nirp,
                                                   hhi_coef = FALSE,
                                                   y_lower = Y_LOWER,
                                                   y_upper = Y_UPPER,
                                                   breaks = BREAKS_GRAPH,
                                                   title_name = "IRF of Lending Amount: Total MS - NIRP"
)

plot_sample_total_ms_original_rate_total <- GG_IRF_ONE(data = results_sample_rates$total_original,
                                                       hhi_coef = FALSE,
                                                       y_lower = Y_LOWER,
                                                       y_upper = Y_UPPER,
                                                       breaks = BREAKS_GRAPH,
                                                       title_name = "IRF of Lending Amount: Total MS - Reference"
)

plot_sample_sample_ms_comparison_rate_total <- GG_IRF_TWO(data1 = results_sample_rates$total_nirp,
                                                          data2 = results_sample_rates$total_original,
                                                          data_name = c("NIRP", "Original"),
                                                          hhi_coef = FALSE,
                                                          y_lower = Y_LOWER,
                                                          y_upper = Y_UPPER,
                                                          breaks = BREAKS_GRAPH,
                                                          name = "IRF of Lending Amount: Total MS"
)



#  Positive MS Shock ---
plot_sample_pos_ms_nirp_rate_total <- GG_IRF_ONE(data = results_sample_rates$positive_nirp,
                                                 hhi_coef = FALSE,
                                                 y_lower = Y_LOWER,
                                                 y_upper = .03,
                                                 breaks = BREAKS_GRAPH,
                                                 title_name = "IRF of Lending Amount: Positive MS - NIRP"
)

plot_sample_pos_ms_original_rate_total <- GG_IRF_ONE(data = results_sample_rates$positive_original,
                                                     hhi_coef = FALSE,
                                                     y_lower = Y_LOWER,
                                                     y_upper = Y_UPPER,
                                                     breaks = BREAKS_GRAPH,
                                                     title_name = "IRF of Lending Amount: Positive MS - Original"
)

plot_sample_pos_ms_comparison_rate_total <- GG_IRF_TWO(data1 = results_sample_rates$positive_nirp,
                                                       data2 = results_sample_rates$positive_original,
                                                       data_name = c("NIRP", "Original"),
                                                       hhi_coef = FALSE,
                                                       y_lower = Y_LOWER,
                                                       y_upper = Y_UPPER,
                                                       breaks = BREAKS_GRAPH,
                                                       name = "IRF of Lending Amount: Positive MS"
)

# Negative MS Shock
plot_sample_neg_ms_nirp_rate_total <- GG_IRF_ONE(data = results_sample_rates$negative_nirp,
                                                 hhi_coef = FALSE,
                                                 y_lower = Y_LOWER,
                                                 y_upper = Y_UPPER,
                                                 breaks = BREAKS_GRAPH,
                                                 title_name = "IRF of Lending Amount: Negative MS - NIRP"
)

plot_sample_neg_ms_original_rate_total <- GG_IRF_ONE(data = results_sample_rates$negative_original,
                                                     hhi_coef = FALSE,
                                                     y_lower = Y_LOWER,
                                                     y_upper = Y_UPPER,
                                                     breaks = BREAKS_GRAPH,
                                                     title_name = "IRF of Lending Amount: Negative MS - Original"
)

plot_sample_total_ms_comparison_rate_total <- GG_IRF_TWO(data1 = results_sample_rates$negative_nirp,
                                                         data2 = results_sample_rates$negative_original,
                                                         data_name = c("NIRP", "Original"),
                                                         hhi_coef = FALSE,
                                                         y_lower = Y_LOWER,
                                                         y_upper = Y_UPPER,
                                                         breaks = BREAKS_GRAPH,
                                                         name = "IRF of Lending Amount: Negative MS"
)



################################### END #######################################

plot_full_total_neg$plot


select_cols <- c("country", "month", "log_hp_total_amount", "hhi_ci_total_assets", "I_HHI_A_NEG_NIRP", controls)
df_subset <- df_main %>% 
  mutate(month = as.numeric(month)) |> 
  select(all_of(select_cols))

# Run the LP_LIN_PANEL function
test <- LP_LIN_PANEL(
  data_set          = df_subset,
  data_sample       = df_main_nirp$month,
  endog_data        = "log_hp_total_amount",
  lags_endog_data   = 6,
  cumul_mult        = TRUE,
  shock             = "I_HHI_A_NEG_NIRP",
  lags_shock        = 6,
  diff_shock        = TRUE,
  panel_model       = "within",
  panel_effect      = PANEL_EFFECT,
  robust_cov        = "vcovSCC",
  robust_maxlag     = 4,
  robust_type       = "HC1",
  robust_cluster    = NULL,
  c_exog_data       = NULL, #colnames(df_subset)[c(16:17)],
  l_exog_data       = NULL,
  lags_exog_data    = NaN,
  c_fd_exog_data    = colnames(df_subset)[c(4, 6:15)],
  l_fd_exog_data    = colnames(df_subset)[c(4, 6:15)],
  lags_fd_exog_data = 6,
  confint           = CI,
  hor               = HOR,
  biter             = BITER
)

GG_IRF_ONE(data = test,
           hhi_coef = FALSE,
           y_lower = -.025,
           y_upper = .02,
           breaks = .005,
           title_name = "IRF: Total MS"
)



test <- lpirfs::lp_lin_panel(
  data_set = df_subset,
  data_sample = "Full",
  endog_data = "log_hp_total_amount",
  cumul_mult = TRUE,
  shock = "I_HHI_A_NEG_NIRP",
  diff_shock = TRUE,
  panel_model = "within",
  panel_effect = "twoways",
  robust_cov = "vcovSCC",
  c_fd_exog_data = colnames(df_subset)[c(4, 6:15)],
  l_fd_exog_data = colnames(df_subset)[c(4, 6:15)],
  lags_fd_exog_data = 6,
  confint = 1.96,
  hor = 12
)


plot(test)







## 2.1 Interaction Term: ZLB x HHI x MPS -

# log_hp_total_amount
df_lp_1 <- df_main |> 
  mutate(
    I_ZLB_MPS_HHI = d_dfr_nirp * MP_median * hhi_ci_total_assets,
  ) |> 
  select(country, month, log_hp_total_amount, I_ZLB_MPS_HHI, hhi_ci_total_assets,  log_cr, log_tl, log_dl, hicp, reer, ur, exr, commodity_index, hicp_ea, gdp_ea)
  

pdf_lp <- pdata.frame(df_lp_1, index = c("country", "month"))

test <- lpirfs::lp_lin_panel(
  data_set = pdf_lp,
  data_sample = "Full",
  endog_data = "log_hp_total_amount",
  cumul_mult = TRUE,
  shock = "I_ZLB_MPS_HHI",
  diff_shock = TRUE,
  panel_model = "within",
  panel_effect = "twoways",
  robust_cov = "vcovSCC",
  c_exog_data = colnames(pdf_lp)[c(5:12)],
  l_exog_data = colnames(pdf_lp)[c(4:12)],
  lags_exog_data = 6,
  confint = 1.96,
  hor = 12
)

png(file = paste0(FIGURE, "EU_IRF_log_hp_total_amount.png"))
plot(test)
dev.off()

# lending rate for 1 year 

df_lp_2 <- df_main |> 
  mutate(
    I_ZLB_MPS_HHI = d_dfr_nirp * Altavilla_target * hhi_ci_total_assets,
  ) |> 
  select(country, month, lending_rate_1year, I_ZLB_MPS_HHI, log_cr, log_tl, log_dl, hicp, reer, ur, commodity_index)


pdf_lp <- pdata.frame(df_lp_2, index = c("country", "month"))

test <- lpirfs::lp_lin_panel(
  data_set = pdf_lp,
  data_sample = "Full",
  endog_data = "lending_rate_1year",
  cumul_mult = TRUE,
  shock = "I_ZLB_MPS_HHI",
  diff_shock = TRUE,
  panel_model = "within",
  panel_effect = "twoways",
  robust_cov = "vcovSCC",
  c_exog_data = colnames(pdf_lp)[c(5:11)],
  l_exog_data = colnames(pdf_lp)[c(4:11)],
  lags_exog_data = 6,
  confint = 1.96,
  hor = 12
)

png(file = paste0(FIGURE, "EU_IRF_lending_rate_1year.png"))
plot(test)
dev.off()

# lending rate for 5 year 

df_lp_3 <- df_main |> 
  mutate(
    I_ZLB_MPS_HHI = d_dfr_nirp * Altavilla_target * hhi_ci_total_assets,
  ) |> 
  select(country, month, lending_rate_5year, I_ZLB_MPS_HHI, log_cr, log_tl, log_dl, hicp, reer, ur, commodity_index)


pdf_lp <- pdata.frame(df_lp_3, index = c("country", "month"))

test <- lpirfs::lp_lin_panel(
  data_set = pdf_lp,
  data_sample = "Full",
  endog_data = "lending_rate_5year",
  cumul_mult = TRUE,
  shock = "I_ZLB_MPS_HHI",
  diff_shock = TRUE,
  panel_model = "within",
  panel_effect = "twoways",
  robust_cov = "vcovSCC",
  c_exog_data = colnames(pdf_lp)[c(5:11)],
  l_exog_data = colnames(pdf_lp)[c(4:11)],
  lags_exog_data = 6,
  confint = 1.96,
  hor = 12
)

png(file = paste0(FIGURE, "EU_IRF_lending_rate_5year.png"))
plot(test)
dev.off()


 ## 2.2 Run LP for ZLB & Non-ZLB Period Separately -

# HP Total Amount
df_lp_4 <- df_main |> 
  mutate(
    I_ZLB_MPS_HHI = Altavilla_target * hhi_ci_total_assets,
  ) |> 
  select(country, month, log_hp_total_amount, I_ZLB_MPS_HHI, log_cr, log_tl, log_dl, hicp, reer, ur, commodity_index) |> 
  mutate(month = as.numeric(month))

df_subset <- df_main |> 
  select(month, d_dfr_nirp) |> 
  distinct(month, d_dfr_nirp) |> 
  mutate(month = as.numeric(month)) |> 
  filter(d_dfr_nirp == 1)

sample_data <- unique(df_subset$month)

pdf_lp <- pdata.frame(df_lp_4, index = c("country", "month"))

test <- lpirfs::lp_lin_panel(
  data_set = pdf_lp,
  data_sample = sample_data,
  endog_data = "log_hp_total_amount",
  cumul_mult = TRUE,
  shock = "I_ZLB_MPS_HHI",
  diff_shock = TRUE,
  panel_model = "within",
  panel_effect = "twoways",
  robust_cov = "vcovSCC",
  c_exog_data = colnames(pdf_lp)[c(5:11)],
  l_exog_data = colnames(pdf_lp)[c(4:11)],
  lags_exog_data = 6,
  confint = 1.96,
  hor = 12
)

png(file = paste0(FIGURE, "EU_IRF_NIRP_hp_total_amount.png"))
plot(test)
dev.off()


# HP Total Amount | Non-NIRP
df_lp_4 <- df_main |> 
  mutate(
    I_ZLB_MPS_HHI = Altavilla_target * hhi_ci_total_assets,
  ) |> 
  select(country, month, log_hp_total_amount, I_ZLB_MPS_HHI, log_cr, log_tl, log_dl, hicp, reer, ur, commodity_index) |> 
  mutate(month = as.numeric(month))

df_subset <- df_main |> 
  select(month, d_dfr_nirp) |> 
  distinct(month, d_dfr_nirp) |> 
  mutate(month = as.numeric(month)) |> 
  filter(d_dfr_nirp == 0)

sample_data <- unique(df_subset$month)

pdf_lp <- pdata.frame(df_lp_4, index = c("country", "month"))

test <- lpirfs::lp_lin_panel(
  data_set = pdf_lp,
  data_sample = sample_data,
  endog_data = "log_hp_total_amount",
  cumul_mult = TRUE,
  shock = "I_ZLB_MPS_HHI",
  diff_shock = TRUE,
  panel_model = "within",
  panel_effect = "twoways",
  robust_cov = "vcovSCC",
  c_exog_data = colnames(pdf_lp)[c(5:11)],
  l_exog_data = colnames(pdf_lp)[c(4:11)],
  lags_exog_data = 6,
  confint = 1.96,
  hor = 12
)

png(file = paste0(FIGURE, "EU_IRF_NON_NIRP_hp_total_amount.png"))
plot(test)
dev.off()


# Lending Rate | NIRP
df_lp_4 <- df_main |> 
  mutate(
    I_ZLB_MPS_HHI = Altavilla_target * hhi_ci_total_assets,
  ) |> 
  select(country, month, lending_rate_1year, I_ZLB_MPS_HHI, log_cr, log_tl, log_dl, hicp, reer, ur, commodity_index) |> 
  mutate(month = as.numeric(month))

df_subset <- df_main |> 
  select(month, d_dfr_nirp) |> 
  distinct(month, d_dfr_nirp) |> 
  mutate(month = as.numeric(month)) |> 
  filter(d_dfr_nirp == 1)

sample_data <- unique(df_subset$month)

pdf_lp <- pdata.frame(df_lp_4, index = c("country", "month"))

test <- lpirfs::lp_lin_panel(
  data_set = pdf_lp,
  data_sample = sample_data,
  endog_data = "lending_rate_1year",
  cumul_mult = TRUE,
  shock = "I_ZLB_MPS_HHI",
  diff_shock = TRUE,
  panel_model = "within",
  panel_effect = "twoways",
  robust_cov = "vcovSCC",
  c_exog_data = colnames(pdf_lp)[c(5:11)],
  l_exog_data = colnames(pdf_lp)[c(4:11)],
  lags_exog_data = 6,
  confint = 1.96,
  hor = 12
)

png(file = paste0(FIGURE, "EU_IRF_NIRP_lending_rate_1year.png"))
plot(test)
dev.off()


# HP Total Amount | NIRP
df_lp_4 <- df_main |> 
  mutate(
    I_ZLB_MPS_HHI = Altavilla_target * hhi_ci_total_assets,
  ) |> 
  select(country, month, lending_rate_1year, I_ZLB_MPS_HHI, log_cr, log_tl, log_dl, hicp, reer, ur, commodity_index) |> 
  mutate(month = as.numeric(month))

df_subset <- df_main |> 
  select(month, d_dfr_nirp) |> 
  distinct(month, d_dfr_nirp) |> 
  mutate(month = as.numeric(month)) |> 
  filter(d_dfr_nirp == 0)

sample_data <- unique(df_subset$month)

pdf_lp <- pdata.frame(df_lp_4, index = c("country", "month"))

test <- lpirfs::lp_lin_panel(
  data_set = pdf_lp,
  data_sample = sample_data,
  endog_data = "lending_rate_1year",
  cumul_mult = TRUE,
  shock = "I_ZLB_MPS_HHI",
  diff_shock = TRUE,
  panel_model = "within",
  panel_effect = "twoways",
  robust_cov = "vcovSCC",
  c_exog_data = colnames(pdf_lp)[c(5:11)],
  l_exog_data = colnames(pdf_lp)[c(4:11)],
  lags_exog_data = 6,
  confint = 1.96,
  hor = 12
)

png(file = paste0(FIGURE, "EU_IRF_NON_NIRP_lending_rate_1year.png"))
plot(test)
dev.off()



################################# END #########################################+