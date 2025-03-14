# TARGET: Creating a Main dataset for all control variables from different sources
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

# 1. Import Datasets ===========================================================

# Load: Home Purchase for large sample
df_hp_depository_large <- LOAD("29_thesis_mpt_us_samplecreation_main_hp_large")

# 2. Time Series ===============================================================

# 2.1 Home Purchase Large Sample -----------------------------------------------

# Main Dataset ----------------------------------------------------------------+
df_hp_large <- df_hp_depository_large |> 
  mutate( 
    log_median_household_income = log(median_household_income),
    poverty_percent_all_ages = poverty_percent_all_ages / 100,
    hpi_annual_change_perc = hpi_annual_change_perc / 100,
    inflation_us = inflation_us / 100,
    gdp_growth_us = gdp_growth_us / 100,
    ur_county = ur_county / 100,
    ur_national = ur_national /100
    ) |> 
  mutate(
    loan_amount_pc = loan_amount  / cnty_pop,
    log_loan_amount_pc = log(loan_amount  / cnty_pop)
  )


# Determine High- and Low Market Concentration Subsamples ---------------------+

# 1. hhi_mean: Take the mean of HHI for each county over time
# 2. SD: Take the SD over the hhi_mean
# 3. HHI_mean: Take the mean over hhi_mean
# 4. High Market Concentration: Counties above HHI_mean + SD
# 5. Low Market Concentration: Counties below HHI_mean - SD 

df_subsample <- df_hp_large |> 
  mutate(
    # Step 2:
    HHI_mean = mean(hhi_mean),
    # Step 3:
    SD = sd(hhi_mean)) |> 
  mutate(
    # Step 4:
    high_conc = if_else(hhi_mean > HHI_mean + SD, 1, 0),
    # Step 5:
    low_conc = if_else(hhi_mean < HHI_mean - SD, 1, 0)
  )

# Create Low- and High-Concentration Counties
df_hp_large_highconc <- df_subsample |>
  filter(high_conc == 1)

df_hp_large_lowconc <- df_subsample |>
  filter(low_conc == 1)

# Create ZLB Time Period Sample -----------------------------------------------+

# Create Year Sample for HHI
df_hp_large_zlb1 <- df_hp_large |> 
  filter(d_ffr_mean_1perc == 1)

# ZLB Period
zlb_year <- unique(df_hp_large_zlb1$year)

# Create Year Sample Away from the ZLB
df_hp_large_normal <- df_hp_large |> 
  filter(d_ffr_mean_1perc == 0)

# Non- ZLB Period
normal_year <- unique(df_hp_large_normal$year)


# 3. Local Projection for Panel Data ===========================================

##  General Specification the LP analysis -------------------------------------+

# Panel Effect: "individual", "time", "twoways"
PANEL_EFFECT <- "individual"

# Significance Level
CI <- 1.96

# Time Horizon
HOR <- 6

###############################################################################+
## 3.1 Baseline ################################################################
###############################################################################+

endo <- "log_loan_amount_pc"
controls <- c("ur_county", "log_median_household_income", "hpi_annual_change_perc", "dti", 
              "inflation_us", "gdp_growth_us", "ur_national")


### 3.1.1 Regressions for Baseline ---------------------------------------------


# Define Specficiation for Parallel Computing ---------------------------------+
shock_specs_baseline_JK <- list(
  # Full Sample + JK SUM
  full_sample_hhi_ms_SUM = list(
    shock_var   = "I_HHI_JK_MEDIAN_SUM",
    select_cols = c("fips", "year", endo, "hhi", "I_HHI_JK_MEDIAN_SUM",  controls),
    sample = df_hp_large
  ),
  full_sample_ms_SUM = list(
    shock_var   = "MP_median_sum",
    select_cols = c("fips", "year", endo, "hhi", "MP_median_sum", controls),
    sample = df_hp_large
  ),
  # High-Concentration Sample + JK SUM
  separate_sample_hhi_ms_high_SUM = list(
    shock_var   = "I_HHI_JK_MEDIAN_SUM",
    select_cols = c("fips", "year", endo, "hhi", "I_HHI_JK_MEDIAN_SUM",controls),
    sample = df_hp_large_highconc
    
  ),
  separate_sample_ms_high_SUM = list(
    shock_var   = "MP_median_sum",
    select_cols = c("fips", "year", endo, "hhi", "MP_median_sum", controls),
    sample = df_hp_large_highconc
  ),
  # Low-Concentration Sample + JK SUM
  separate_sample_hhi_ms_low_SUM = list(
    shock_var   = "I_HHI_JK_MEDIAN_SUM",
    select_cols = c("fips", "year", endo, "hhi", "I_HHI_JK_MEDIAN_SUM",controls),
    sample = df_hp_large_lowconc
    
  ),
  separate_sample_ms_low_SUM = list(
    shock_var   = "MP_median_sum",
    select_cols = c("fips", "year", endo, "hhi", "MP_median_sum", controls),
    sample = df_hp_large_lowconc
  )
)

# Parallel Computing ----------------------------------------------------------+

# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run the LP_LIN_PANEL function in parallel for each specification
results_baseline <- foreach(spec = shock_specs_baseline_JK,
                            .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"),
                            .export = c("df_hp_large", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR", "BITER", "endo")) %dopar% {

                            # Subset the data according to the current specification
                            df_subset <- spec$sample  |> 
                              dplyr::select(all_of(spec$select_cols))
                                 
                            # Run the model estimation
                            LP_LIN_PANEL(
                             data_set          = df_subset,
                             data_sample       = "Full",
                             endog_data        = endo,
                             lags_endog_data   = 1,
                             cumul_mult        = TRUE,
                             shock             = spec$shock_var,
                             lags_shock        = 1,
                             diff_shock        = T,
                             panel_model       = "within",
                             panel_effect      = PANEL_EFFECT,
                             robust_cov        = "tLAHR",
                             c_exog_data       = NULL,
                             l_exog_data       = NULL,
                             lags_exog_data    = NaN,
                             c_fd_exog_data    = colnames(df_subset)[c(4, 6:12)],
                             l_fd_exog_data    = colnames(df_subset)[c(4, 6:12)],
                             lags_fd_exog_data = 1,
                             confint           = CI,
                             hor               = HOR,
                             biter             = BITER,
                             p_selection_rule  = FALSE
                            )
                          }

# Optionally, assign names to the results list based on your shock specifications
names(results_baseline) <- names(shock_specs_baseline_JK)

# Shut down the parallel cluster
stopCluster(cl)

# Clear garbage
gc()

### 3.1.2 Graphs for Baseline --------------------------------------------------

# Graph 1: Full Sample - Shock: MS x HHI [INCLUDED] ---------------------------+

plot1_full_ms_hhi <- GG_IRF_ONE(data = results_baseline$full_sample_hhi_ms_SUM,
                                hhi_coef = FALSE, 
                                y_lower = -2, 
                                y_upper = 2, 
                                breaks = 1,
                                title_name = "Full Sample",
                                time_name = "Years"
                                )

# Graph 2: Full Sample - Shock: MS [APPENDIX] ---------------------------------+

plot2_full_ms <- GG_IRF_ONE(data     = results_baseline$full_sample_ms_SUM,
                            hhi_coef = FALSE, 
                            y_lower  = -2, 
                            y_upper  = 2, 
                            breaks   = 1,
                            title_name = "Full Sample: Monetary Shock",
                            time_name  = "Years"
                            )

# Graph 3: High vs Low Sample - Shock: MS x HHI -------------------------------+

# Graph 3.1: Combined ---------------------------------------------------------+
plot31_separate_sample_ms_hhi <- GG_IRF_TWO(data1 = results_baseline$separate_sample_hhi_ms_high_SUM,
                                           data2 = results_baseline$separate_sample_hhi_ms_low_SUM,
                                           data_name = c("High HHI", "Low HHI"),
                                           hhi_coef = FALSE, 
                                           y_lower = -24,
                                           y_upper = 8,
                                           breaks = 2,
                                           title_name = "Subsample",
                                           time_name = "Years"
                                           )

# Graph 3.2: Large Market Concentration  [INCLUDED] ---------------------------+
  
plot32_large_ms_hhi <- GG_IRF_ONE(data       = results_baseline$separate_sample_hhi_ms_high_SUM,
                                 hhi_coef   = FALSE, 
                                 y_lower    = -2, 
                                 y_upper    = 2, 
                                 breaks     = 1,
                                 title_name = "Sample: Large Market Concentration",
                                 time_name  = "Years"
                                 )  

# Graph 3.3: Low Market Concentration  [INCLUDED] -----------------------------+

plot33_low_ms_hhi <- GG_IRF_ONE(data     = results_baseline$separate_sample_hhi_ms_low_SUM,
                                 hhi_coef = FALSE, 
                                 y_lower  = -6, 
                                 y_upper  = 10, 
                                 breaks   = 2,
                                 title_name = " Sample: Low Market Concentration",
                                 time_name  = "Years"
                                )  

# Graph 4: High vs Low Sample - Shock: MS [APPENDIX ] -------------------------+

plot4_separate_sample_ms <- GG_IRF_TWO(data1 = results_baseline$separate_sample_ms_high_SUM,
                                       data2 = results_baseline$separate_sample_ms_low_SUM,
                                       data_name = c("High HHI", "Low HHI"),
                                       hhi_coef = FALSE, 
                                       y_lower = -2,
                                       y_upper = 2,
                                       breaks = 1,
                                       title_name = "Subsample: Low and High Market concentration",
                                       time_name = "Years"
                                       )

# Final Graph: Combine 1 to 4 -------------------------------------------------+

# PANEL A GRAPH ---------------------------------------------------------------+

graph_baseline <- (plot1_full_ms_hhi$plot + plot32_large_ms_hhi$plot + plot33_low_ms_hhi$plot) +
  plot_annotation(
    title = "Panel A",
    tag_levels = c("I", "II", "III"),
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0),
      plot.caption = element_text(size = 10, hjust = 0),
      plot.tag.position = c(0.5, 1),
      plot.tag = element_text(hjust = 0.5)
      )
  )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "01_US_Panel_A/", "baseline_results_us.pdf"),
  plot = graph_baseline,
  width = 12, height = 8, dpi = 300
  )
}

# Appendix 1 GRAPH ------------------------------------------------------------+

graph_baseline_appendix <- (plot2_full_ms$plot + plot4_separate_sample_ms$plot) +
  plot_annotation(
    title = "Appendix A",
    tag_levels = c("I", "II"),
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "01_US_Panel_A/", "baseline_results_us_appendix.pdf"),
  plot = graph_baseline_appendix,
  width = 12, height = 8, dpi = 300
)
}


###############################################################################+
## 3.2 Interaction Term with ZLB Indicator #####################################
###############################################################################+

### 3.2.1 Regressions with Interaction Term ------------------------------------

# Specification for Parallel Computing
shock_specs_zlb_indicator_JK <- list(
  full_sample_hhi_ms = list(
    shock_var   = "I_HHI_JK_MEDIAN_SUM_1",
    select_cols = c("fips", "year", endo, "hhi", "I_HHI_JK_MEDIAN_SUM_1",  controls),
    sample = df_hp_large
  ),
  full_sample_ms = list(
    shock_var   = "I_JK_MEDIAN_SUM_1",
    select_cols = c("fips", "year", endo, "hhi", "I_JK_MEDIAN_SUM_1", controls),
    sample = df_hp_large
  ),
  separate_sample_hhi_ms_high = list(
    shock_var   = "I_HHI_JK_MEDIAN_SUM_1",
    select_cols = c("fips", "year", endo, "hhi", "I_HHI_JK_MEDIAN_SUM_1",controls),
    sample = df_hp_large_highconc
    
  ),
  separate_sample_ms_high = list(
    shock_var   = "I_JK_MEDIAN_SUM_1",
    select_cols = c("fips", "year", endo, "hhi", "I_JK_MEDIAN_SUM_1", controls),
    sample = df_hp_large_highconc
  ),
  separate_sample_hhi_ms_low = list(
    shock_var   = "I_HHI_JK_MEDIAN_SUM_1",
    select_cols = c("fips", "year", endo, "hhi", "I_HHI_JK_MEDIAN_SUM_1",controls),
    sample = df_hp_large_lowconc
    
  ),
  separate_sample_ms_low = list(
    shock_var   = "I_JK_MEDIAN_SUM_1",
    select_cols = c("fips", "year", endo, "hhi", "I_JK_MEDIAN_SUM_1", controls),
    sample = df_hp_large_lowconc
  )
)

# Parallel Computing ----------------------------------------------------------+

# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run the LP_LIN_PANEL function in parallel for each specification
results_zlb_indicator <- foreach(spec = shock_specs_zlb_indicator_JK,
                            .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"),
                            .export = c("df_hp_large", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR", "BITER", "endo")) %dopar% {
                              
                              # Subset the data according to the current specification
                              df_subset <- spec$sample  |> 
                                select(all_of(spec$select_cols))
                              
                              # Run the model estimation
                              LP_LIN_PANEL(
                                data_set          = df_subset,
                                data_sample       = "Full",
                                endog_data        = endo,
                                lags_endog_data   = 1,
                                cumul_mult        = TRUE,
                                shock             = spec$shock_var,
                                lags_shock        = 1,
                                diff_shock        = TRUE,
                                panel_model       = "within",
                                panel_effect      = PANEL_EFFECT,
                                robust_cov        = "tLAHR",
                                c_exog_data       = NULL,
                                l_exog_data       = NULL,
                                lags_exog_data    = NaN,
                                c_fd_exog_data    = colnames(df_subset)[c(4, 6:12)],
                                l_fd_exog_data    = colnames(df_subset)[c(4, 6:12)],
                                lags_fd_exog_data = 1,
                                confint           = CI,
                                hor               = HOR,
                                biter             = BITER
                              )
                            }

# Optionally, assign names to the results list based on your shock specifications
names(results_zlb_indicator) <- names(shock_specs_zlb_indicator_JK)

# Shut down the parallel cluster
stopCluster(cl)

# Clear garbage
gc()

### 3.2.2 Graphs for Regressions with Interaction Term -------------------------

# Graph 5: Full Sample - Shock: MS x HHI x ZLB [INCLUDED] ---------------------+

plot5_full_ms_hhi <- GG_IRF_ONE(data = results_zlb_indicator$full_sample_hhi_ms,
                                hhi_coef = FALSE, 
                                y_lower = -2, 
                                y_upper = 5, 
                                breaks = 1,
                                title_name = "Full Sample: Monetary Shock \u00D7 HHI \u00D7 ZLB",
                                time_name = "Years"
                                )

# Graph 6: Full Sample - Shock: MS x ZLB [APPENDIX] ---------------------------+

plot6_full_ms <- GG_IRF_ONE(data = results_zlb_indicator$full_sample_ms,
                            hhi_coef = FALSE, 
                            y_lower = -3, 
                            y_upper = 5, 
                            breaks = 1,
                            title_name = "Full Sample: Monetary Shock \u00D7 ZLB",
                            time_name = "Years"
                            )

# Graph 7: High vs Low Sample - Shock: MS x HHI x ZLB -------------------------+

# Graph 7.1: Large and Low  Market Concentration [INCLUDED] -------------------+

plot71_separate_sample_ms_hhi <- GG_IRF_TWO(data1 = results_zlb_indicator$separate_sample_hhi_ms_high,
                                           data2 = results_zlb_indicator$separate_sample_hhi_ms_low,
                                           data_name = c("High HHI", "Low HHI"),
                                           hhi_coef = FALSE, 
                                           y_lower = -14,
                                           y_upper = 28,
                                           breaks = 2,
                                           title_name = "Subsample: Monetary Shock \u00D7 HHI \u00D7 ZLB",
                                           time_name = "Years"
)

# Graph 7.2: Large Market Concentration [INCLUDED] ----------------------------+

plot72_separate_sample_ms_hhi <- GG_IRF_ONE(data = results_zlb_indicator$separate_sample_hhi_ms_high,
                                 hhi_coef   = FALSE, 
                                 y_lower    = -2, 
                                 y_upper    = 5, 
                                 breaks     = 1,
                                 title_name = "Sample: Large Market Concentration",
                                 time_name  = "Years"
)  

# Graph 7.3: Low Market Concentration [INCLUDE] -------------------------------+

plot73_separate_sample_ms_hhi <- GG_IRF_ONE(data = results_zlb_indicator$separate_sample_hhi_ms_low,
                               hhi_coef = FALSE, 
                               y_lower  = -4, 
                               y_upper  = 24, 
                               breaks   = 2,
                               title_name = " Sample: Low Market Concentration",
                               time_name  = "Years"
)  

# Graph 8: High vs Low Sample - Shock: MS x ZLB [APPENIDX] --------------------+

plot8_separate_sample_ms <- GG_IRF_TWO(data1 =  results_zlb_indicator$separate_sample_ms_high,
                                       data2 = results_zlb_indicator$separate_sample_ms_low,
                                       data_name = c("High HHI", "Low HHI"),
                                       hhi_coef = FALSE, 
                                       y_lower = -3,
                                       y_upper = 5,
                                       breaks = 1,
                                       title_name = "Subsample: Monetary Shock \u00D7 ZLB",
                                       time_name = "Years"
                                       )

# Final Graph: Combine 5 to 8 -------------------------------------------------+

# PANEL B GRAPH ---------------------------------------------------------------+

graph_ZLB_indicator <- (plot5_full_ms_hhi$plot + plot72_separate_sample_ms_hhi$plot + plot73_separate_sample_ms_hhi$plot) +
  plot_annotation(
    title = "Panel B",
    tag_levels = c("I", "II", "III"),
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "02_US_Panel_B/", "results_zlb_indicator_us.pdf"),
  plot = graph_ZLB_indicator,
  width = 12, height = 8, dpi = 300
)
}

# APPENDIX 2 ------------------------------------------------------------------+

graph_ZLB_indicator_withhoutHHI <- (plot6_full_ms$plot + plot8_separate_sample_ms$plot) +
  plot_annotation(
    title = "Appendix B",
    tag_levels = c("I", "II"),
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "02_US_Panel_B/", "results_zlb_indicator_us_appendix.pdf"),
  plot = graph_ZLB_indicator_withhoutHHI,
  width = 12, height = 8, dpi = 300
)
}


###############################################################################+
## 3.3 Subsample for the Identification of the ZLB Effect on HHI ###############
###############################################################################+

### 3.3.1 Regressions on Subsample ---------------------------------------------


# Specification for Parallel Computing ----------------------------------------+
shock_specs_zlb_sample_JK <- list(
  full_sample_hhi_ms = list(
    shock_var   = "I_HHI_JK_MEDIAN_SUM",
    select_cols = c("fips", "year", endo, "hhi", "I_HHI_JK_MEDIAN_SUM",  controls),
    sample = df_hp_large,
    sample_period = zlb_year
  ),
  full_sample_ms = list(
    shock_var   = "MP_median_sum",
    select_cols = c("fips", "year", endo, "hhi", "MP_median_sum", controls),
    sample = df_hp_large,
    sample_period = zlb_year
  ),
  separate_sample_hhi_ms_high = list(
    shock_var   = "I_HHI_JK_MEDIAN_SUM",
    select_cols = c("fips", "year", endo, "hhi", "I_HHI_JK_MEDIAN_SUM",controls),
    sample = df_hp_large_highconc,
    sample_period = zlb_year
    
  ),
  separate_sample_ms_high = list(
    shock_var   = "MP_median_sum",
    select_cols = c("fips", "year", endo, "hhi", "MP_median_sum", controls),
    sample = df_hp_large_highconc,
    sample_period = zlb_year
  ),
  separate_sample_hhi_ms_low = list(
    shock_var   = "I_HHI_JK_MEDIAN_SUM",
    select_cols = c("fips", "year", endo, "hhi", "I_HHI_JK_MEDIAN_SUM",controls),
    sample = df_hp_large_lowconc,
    sample_period = zlb_year
    
  ),
  separate_sample_ms_low = list(
    shock_var   = "MP_median_sum",
    select_cols = c("fips", "year", endo, "hhi", "MP_median_sum", controls),
    sample = df_hp_large_lowconc,
    sample_period = zlb_year
  )
)


# Parallel Computing ----------------------------------------------------------+

# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run the LP_LIN_PANEL function in parallel for each specification
results_zlb_sample <- foreach(spec = shock_specs_zlb_sample_JK,
                              .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"),
                              .export = c("df_hp_large", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR", "endo")) %dopar% {
                                   
                              # Subset the data according to the current specification
                              df_subset <- spec$sample  |> 
                                select(all_of(spec$select_cols))
                              
                              # Run the model estimation
                              LP_LIN_PANEL(
                                data_set          = df_subset,
                                data_sample       = spec$sample_period,
                                endog_data        = endo,
                                lags_endog_data   = 1,
                                cumul_mult        = TRUE,
                                shock             = spec$shock_var,
                                lags_shock        = 1,
                                diff_shock        = TRUE,
                                panel_model       = "within",
                                panel_effect      = PANEL_EFFECT,
                                robust_cov        = "tLAHR",
                                robust_cluster    = "time",
                                c_exog_data       = NULL,
                                l_exog_data       = NULL,
                                lags_exog_data    = NaN,
                                c_fd_exog_data    = colnames(df_subset)[c(4, 6:12)],
                                l_fd_exog_data    = colnames(df_subset)[c(4, 6:12)],
                                lags_fd_exog_data = 1,
                                confint           = CI,
                                hor               = HOR,
                                biter             = BITER
                              )
                            }

# Optionally, assign names to the results list based on your shock specifications
names(results_zlb_sample) <- names(shock_specs_zlb_indicator_JK)

# Shut down the parallel cluster
stopCluster(cl)

# Clear garbage
gc()

### 3.3.2 Graphs for Regressions on Subsample ----------------------------------

# Graph 9: Subsample - Shock: MS x HHI [INCLUDED] -----------------------------+

plot9_full_ms_hhi <- GG_IRF_ONE(data = results_zlb_sample$full_sample_hhi_ms,
                                hhi_coef = FALSE, 
                                y_lower = -1.5, 
                                y_upper = 2.5, 
                                breaks = .5,
                                title_name = "Full Sample",
                                time_name = "Years"
                                )

# Graph 10: Subample - Shock: MS [APPENDIX] -----------------------------------+

plot10_full_ms <- GG_IRF_ONE(data = results_zlb_sample$full_sample_ms,
                             hhi_coef = FALSE, 
                             y_lower = -2, 
                             y_upper = 3, 
                             breaks = 1,
                             title_name = "Full Sample",
                             time_name = "Year"
                             )

# Graph 11: High vs Low Sample - Shock: MS x HHI ------------------------------+

# Graph 11.1: Large Market Concentration --------------------------------------+

plot111_separate_sample_ms_hhi <- GG_IRF_TWO(data1 = results_zlb_sample$separate_sample_hhi_ms_high,
                                           data2 = results_zlb_sample$separate_sample_hhi_ms_low,
                                           data_name = c("High HHI", "Low HHI"),
                                           hhi_coef = FALSE, 
                                           y_lower = -16,
                                           y_upper = 8,
                                           breaks = 2,
                                           title_name = "Subsample: Monetary Shock \u00D7 HHI \u00D7 ZLB",
                                           time_name = "Years"
                                           )


# Graph 11.2: Large Market Concentration [INCLDUED] ---------------------------+

plot112_separate_sample_ms_hhi <- GG_IRF_ONE(data = results_zlb_sample$separate_sample_hhi_ms_high,
                                            hhi_coef   = F, 
                                            y_lower    = -1.5, 
                                            y_upper    = 2.5, 
                                            breaks     = .5,
                                            title_name = "Sample: Large Market Concentration",
                                            time_name  = "Years"
                                            )  

# Graph 11.3: Low Market Concentration [INCLUDED] -----------------------------+

plot113_separate_sample_ms_hhi <- GG_IRF_ONE(data = results_zlb_sample$separate_sample_hhi_ms_low,
                                            hhi_coef = F, 
                                            y_lower  = -8, 
                                            y_upper  = 8, 
                                            breaks   = 1,
                                            title_name = "Sample: Low Market Concentration",
                                            time_name  = "Years"
                                            )  

# Graph 12: High vs Low Sample - Shock: MS [APPENDIX] -------------------------+

plot12_separate_sample_ms <- GG_IRF_TWO(data1 =  results_zlb_sample$separate_sample_ms_high,
                                       data2 = results_zlb_sample$separate_sample_ms_low,
                                       data_name = c("High HHI", "Low HHI"),
                                       hhi_coef = FALSE, 
                                       y_lower = -2,
                                       y_upper = 3,
                                       breaks = 1,
                                       title_name = "Subsample: Monetary Shock \u00D7 ZLB",
                                       time_name = "Years"
                                       )


# Final Graph: Combine 9 to 12 ------------------------------------------------+

# PANEL C GRAPH ---------------------------------------------------------------+

graph_ZLB_sample <- (plot9_full_ms_hhi$plot + plot112_separate_sample_ms_hhi$plot + plot113_separate_sample_ms_hhi$plot) +
  plot_annotation(
    title = "Panel C",
    tag_levels = c("I", "II", "III"),
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0),
      plot.caption = element_text(size = 10, hjust = 0)
    ) 
  )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "03_US_Panel_C/", "results_zlb_sample_us.pdf"),
  plot = graph_ZLB_sample,
  width = 12, height = 8, dpi = 300
)
}


# APPENDIX - Exlcude HHI from Specification -----------------------------------+

graph_ZLB_sample_withoutHHI <- (plot10_full_ms$plot + plot12_separate_sample_ms$plot) +
  plot_annotation(
    title = "Appendix C",
    tag_levels = c("I", "II"),
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0),
      plot.caption = element_text(size = 10, hjust = 0)
    ) 
  )

if (PRINT) {
# Save
ggsave(
  filename = paste0(FIGURE, "03_US_Panel_C/", "results_zlb_sample_us_appendix.pdf"),
  plot = graph_ZLB_sample_withoutHHI,
  width = 8, height = 4, dpi = 300
)
}


###############################################################################+
###################################### END #####################################
###############################################################################+

