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

# install.packages("lpirfs")
# install.packages("urca")
# install.packages("TS")
# install.packages("panelvar")
# install.packages("CADFtest")
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
    ) 
  #|> 
  # Interaction Terms
  # mutate(
  #   I_HHI_ZLB2_NS = d_ffr_mean_2perc * hhi * NS_target,
  #   I_HHI_ZLB1_NS = d_ffr_mean_1perc * hhi * NS_target,
  #   I_HHI_NS = hhi * NS_target,
  #   I_hhi_2perc = d_ffr_mean_2perc * hhi,
  #   I_hhi_NS = hhi * NS_target,
  #   I_2perc_NS = d_ffr_mean_2perc * NS_target
  # )

# Local Projection Model
model_lp <- formula(log_loan_amount ~ hhi_scale + d_ffr_mean_2perc + NS_target + I_treatment_NS_2 + ur + poverty_percent_all_ages + log_median_household_income + hpi_annual_change_perc + inflation_us + gdp_growth_us)

# Variables of Interest
model_var_interest <- formula(log_loan_amount ~ hhi + d_ffr_mean_2perc + NS_target + ur + poverty_percent_all_ages + log_median_household_income + hpi_annual_change_perc + inflation_us + gdp_growth_us)

# Create Panel Dataset with pdata.frame
df_hp_large_panel <- pdata.frame(df_hp_large, index = c("fips", "year"))

model <- plm(model_var_interest, data = df_hp_large_panel)


## Testing for Cross-Sectional Dependence of the Data --------------------------

# Pesaran’s CD Test: H0: No cross-sectional dependence ---
pcd <- pcdtest(model)
print(pcd)

# Correlation Matrix ---
# Define the variables
vars <- c("log_loan_amount", "hhi", "NS_target", "ur", 
          "poverty_percent_all_ages", "median_household_income", 
          "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")

# Compute within-period correlation (for each time period separately)

df_hp_large_panel <- df_hp_large_panel %>%
  mutate(year = as.factor(year)) # Convert year to factor if necessary

# Compute time-demeaned (within-transformed) variables
df_demeaned <- df_hp_large_panel %>%
  group_by(year) %>%
  mutate(
    log_loan_amount = log_loan_amount - mean(log_loan_amount, na.rm = TRUE),
    hhi = hhi - mean(hhi, na.rm = TRUE),
    NS_target = NS_target - mean(NS_target, na.rm = TRUE),
    ur = ur - mean(ur, na.rm = TRUE),
    poverty_percent_all_ages = poverty_percent_all_ages - mean(poverty_percent_all_ages, na.rm = TRUE),
    median_household_income = median_household_income - mean(median_household_income, na.rm = TRUE),
    hpi_annual_change_perc = hpi_annual_change_perc - mean(hpi_annual_change_perc, na.rm = TRUE),
    inflation_us = inflation_us - mean(inflation_us, na.rm = TRUE),
    gdp_growth_us = gdp_growth_us - mean(gdp_growth_us, na.rm = TRUE)
  ) %>%
  dplyr::select(!!vars) |> 
  ungroup()

# Compute correlation matrix on time-demeaned variables
cor_matrix_demeaned <- cor(df_demeaned %>% select(
  log_loan_amount, hhi, ur, 
  poverty_percent_all_ages, median_household_income, 
  hpi_annual_change_perc
), use = "pairwise.complete.obs")

## Heatmap

# Melt the correlation matrix for visualization
cor_melted <- reshape2::melt(cor_matrix_demeaned)

# Create heatmap using ggplot2
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ggtitle("Heatmap of Time-Demeaned Variable Correlations") +
  xlab("Variables") +
  ylab("Variables")


# Panel Unit Root Test ----------------------------------------------------------
if (DEBUG) {
# install.packages("fUnitRoots")
# install.packages("punitroots", repos="http://R-Forge.R-project.org")
library(fUnitRoots)
library(punitroots)

vars_suspected_nonstationary <- c("log_loan_amount", "hhi", "ur", "poverty_percent_all_ages",
                                  "median_household_income", "hpi_annual_change_perc")




library(tseries)
adf_test <- adf.test(test$log_loan_amount[test$fips %in% unique(test$fips)[1]], alternative = "stationary")
print(adf_test)

test <- acf(test$log_loan_amount, na.action = na.pass)

purtest(test$log_loan_amount, method = "madwu", pmax = 0)

hist(test$log_loan_amount)

zero_variance_check <- df_hp_large_panel %>%
  group_by(fips) %>%
  summarise(var_check = var(log_loan_amount, na.rm = TRUE)) %>%
  filter(var_check < 1e-6)

# Combine results into a data frame
cips_results_df <- do.call(rbind, cips_results)


# Function to compute ACF for a single county and extract the first lag
compute_acf <- function(series) {
  
  series <- test$log_loan_amount
  acf_values <- acf(series, plot = TRUE, na.action = na.pass)  # Compute ACF without plotting
  return(acf_values$acf[2])  # Extract lag-1 autocorrelation
}
}


# 3. Local Projection for Panel Data ===========================================

##  General Specification the LP analysis -------------------------------------+

# Number of Bootstrap Iteration
BITER <- 10

# Panel Effect: "individual", "time", "twoways"
PANEL_EFFECT <- "twoways"

# Signigikance Level
CI <- 1.96

# Time Horizon
HOR <- 6

## 3.1 General Approach --------------------------------------------------------

#' The first approach consists of implementing a linear projection model on the 
#' all counties and not separating the the sample into high- vs low- market concentration
#' counties.

### 3.1.1 Full Sample ----------------------------------------------------------

#' In the Full Sample Approach, the sample is not split up between ZLB period vs 
#' no ZLB period. The LP is run for three specification:
#' 
#' 1. Total Monetary Shock
#' 2. Positive Monetary Shock
#' 3. Negative Monetary Shock

###############################################################################+
## Interaction Tem with ZLB Indicator #########################################+
###############################################################################+

## Total Shock ----------------------------------------------------------------+

df_general_full_ns_total <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_TOTAL_2", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_total <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_total,      # Panel dataset
  data_sample       = "Full",                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_TOTAL_2",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns_total)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns_total)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER       # Number of Iteration for Wild Cluster Bootsrap
)


plot_general_full_ns_total <- GG_IRF_ONE(data = results_general_full_ns_total,
                                   hhi_coef = FALSE, 
                                   y_lower = -1.5, 
                                   y_upper = 1.5, 
                                   title_name = "IRF: Full Sample - Total MS",
                                   time_name = "Time Horizon"
                                   )


## Positive Shock -------------------------------------------------------------+

df_general_full_ns_pos <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_POS_2", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_pos <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_pos,      # Panel dataset
  data_sample       = "Full",                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_POS_2",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns_pos)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns_pos)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER      # Number of Iteration for Wild Cluster Bootsrap
)


plot_general_full_ns_pos <- GG_IRF_ONE(data = results_general_full_ns_pos,
                                   hhi_coef = FALSE, 
                                   y_lower = -2.1, 
                                   y_upper = 2, 
                                   title_name = "IRF: Full Sample - Positive MS",
                                   time_name = "Time Horizon"
)


## Negative Shock -------------------------------------------------------------+

df_general_full_ns_neg <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_NEG_2", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_neg <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_neg,      # Panel dataset
  data_sample       = "Full",                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_NEG_2",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns_neg)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns_neg)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER       # Number of Iteration for Wild Cluster Bootsrap
)


plot_general_full_ns_neg <- GG_IRF_ONE(data = results_general_full_ns_neg,
                                       hhi_coef = FALSE, 
                                       y_lower = -1.5, 
                                       y_upper = 1.5, 
                                       title_name = "IRF: Full Sample - Negative MS - Reference",
                                       time_name = "Time Horizon"
)


###############################################################################+
## Interaction Tem without ZLB Indicator ######################################+
###############################################################################+

## Total Shock ----------------------------------------------------------------+

df_general_full_ns_total_ref <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_TOTAL", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_total_ref <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_total_ref,      # Panel dataset
  data_sample       = "Full",                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_TOTAL",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns_total)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns_total)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER       # Number of Iteration for Wild Cluster Bootsrap
)


plot_general_full_ns_total_ref <- GG_IRF_ONE(data = results_general_full_ns_total_ref,
                                         hhi_coef = FALSE, 
                                         y_lower = -1.5, 
                                         y_upper = 1.5, 
                                         title_name = "IRF: Full Sample - Total MS - Reference",
                                         time_name = "Time Horizon"
)


## Positive Shock -------------------------------------------------------------+

df_general_full_ns_pos_ref <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_POS", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_pos_ref <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_pos_ref,      # Panel dataset
  data_sample       = "Full",                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_POS",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns_pos)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns_pos)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER      # Number of Iteration for Wild Cluster Bootsrap
)


plot_general_full_ns_pos_ref <- GG_IRF_ONE(data = results_general_full_ns_pos_ref,
                                       hhi_coef = FALSE, 
                                       y_lower = -2.1, 
                                       y_upper = 2, 
                                       title_name = "IRF: Full Sample - Positive MS - Reference",
                                       time_name = "Time Horizon"
)


## Negative Shock -------------------------------------------------------------+

df_general_full_ns_neg_ref <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_NEG", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_neg_ref <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_neg_ref,      # Panel dataset
  data_sample       = "Full",                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_NEG",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns_neg)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns_neg)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER       # Number of Iteration for Wild Cluster Bootsrap
)


plot_general_full_ns_neg_ref <- GG_IRF_ONE(data = results_general_full_ns_neg_ref,
                                       hhi_coef = FALSE, 
                                       y_lower = -1.5, 
                                       y_upper = 1.5, 
                                       title_name = "IRF: Full Sample - Negative MS - Reference",
                                       time_name = "Time Horizon"
)


###############################################################################+ 
# Graphs - Full Sample - Comparison between Interaction term with & w/o Indicator 
###############################################################################+

# Full Sample Graph for Interaction Term with ZLB Indicator 
graph_full_withInd <- plot_general_full_ns_total$plot / plot_general_full_ns_pos$plot / plot_general_full_ns_neg$plot
ggsave(filename = paste0(FIGURE, "graph_full_withInd.pdf"), final_plot, width = 8, height = 12, dpi = 300)

# Full Sample Graph for Interaction Term without ZLB Indicator 
graph_full_withoutInd <- plot_general_full_ns_total_ref$plot / plot_general_full_ns_pos_ref$plot / plot_general_full_ns_neg_ref$plot
ggsave(filename = paste0(FIGURE, "graph_full_withoutInd.pdf"), final_plot, width = 8, height = 12, dpi = 300)

# Full Sample Graph for Interaction Term with ZLB Indicator 
# Create header grobs for the two columns
header_left <- wrap_elements(full = grid::textGrob("Graphs with Interaction Term",
                                                   gp = grid::gpar(fontsize = 16, fontface = "bold")))
header_right <- wrap_elements(full = grid::textGrob("Reference",
                                                    gp = grid::gpar(fontsize = 16, fontface = "bold")))


graph_full_final <- (header_left + header_right) /
                    (plot_general_full_ns_total$plot + plot_general_full_ns_total_ref$plot)  / 
                    (plot_general_full_ns_pos$plot + plot_general_full_ns_pos_ref$plot) / 
                    (plot_general_full_ns_neg$plot + plot_general_full_ns_neg_ref$plot) +
                     plot_layout(heights = c(0.2, 1, 1, 1))

ggsave(filename = paste0(FIGURE, "graph_full_final.pdf"), final_plot, width = 8, height = 12, dpi = 300)




### 3.1.2 ZLB vs No-ZLB period -------------------------------------------------

#' In this section, the sample period is split up into two parts. The period ZLB
#' period defined if the Federal Funds Rate is below 1 Percent, and vice versa 
#' for the No-ZLB period.

## Determine the ZLB period ---------------------------------------------------+

# Get ZLB Sample Years
zlb_sample <- df_hp_large |> 
  dplyr::distinct(year, d_ffr_mean_1perc) |> 
  filter(d_ffr_mean_1perc == "1")

# Get No-ZLB Sample Years
no_zlb_sample <- df_hp_large |> 
  dplyr::distinct(year, d_ffr_mean_1perc) |> 
  filter(d_ffr_mean_1perc == "0")

## Total Shock - ZLB Period ---------------------------------------------------+


df_general_full_ns_total_zlb <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_TOTAL", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_total_zlb <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_total_zlb,      # Panel dataset
  data_sample       = zlb_sample$year,                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_TOTAL",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER       # Number of Iteration for Wild Cluster Bootstrap
)


plot_general_full_ns_total_zlb <- GG_IRF_ONE(data = results_general_full_ns_total_zlb,
                                         hhi_coef = FALSE, 
                                         y_lower = -1.5, 
                                         y_upper = 1.5, 
                                         title_name = "IRF: Full Sample - Total MS - ZLB",
                                         time_name = "Time Horizon"
)



## Total Shock - No-ZLB Period ------------------------------------------------+

df_general_full_ns_total_no_zlb <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_TOTAL", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_total_no_zlb <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_total_no_zlb,      # Panel dataset
  data_sample       = no_zlb_sample$year,                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_TOTAL",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER       # Number of Iteration for Wild Cluster Bootstrap
)


plot_general_full_ns_total_no_zlb <- GG_IRF_ONE(data = results_general_full_ns_total_no_zlb,
                                             hhi_coef = FALSE, 
                                             y_lower = -1.5, 
                                             y_upper = 1.5, 
                                             title_name = "IRF: Full Sample - Total MS - No ZLB",
                                             time_name = "Time Horizon"
)



## Positive Shock - ZLB Period ------------------------------------------------+

df_general_full_ns_pos_zlb <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_POS", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_pos_zlb <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_pos_zlb,      # Panel dataset
  data_sample       = zlb_sample$year,                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_POS",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER       # Number of Iteration for Wild Cluster Bootstrap
)


plot_general_full_ns_pos_zlb <- GG_IRF_ONE(data = results_general_full_ns_pos_zlb,
                                           hhi_coef = FALSE, 
                                           y_lower = -1.5, 
                                           y_upper = 1.5, 
                                           title_name = "IRF: Full Sample - Positive MS - ZLB",
                                           time_name = "Time Horizon"
)



## Positive Shock - No-ZLB Period ---------------------------------------------+

df_general_full_ns_pos_zlb <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_POS", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_pos_no_zlb <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_pos_zlb,      # Panel dataset
  data_sample       = no_zlb_sample$year,                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_POS",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER       # Number of Iteration for Wild Cluster Bootstrap
)


plot_general_full_ns_pos_no_zlb <- GG_IRF_ONE(data = results_general_full_ns_pos_no_zlb,
                                           hhi_coef = FALSE, 
                                           y_lower = -1.5, 
                                           y_upper = 1.5, 
                                           title_name = "IRF: Full Sample - Positive MS - No ZLB",
                                           time_name = "Time Horizon"
)


## Negative Shock - ZLB Period ------------------------------------------------+

df_general_full_ns_neg_zlb <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_NEG", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_neg_zlb <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_neg_zlb,      # Panel dataset
  data_sample       = zlb_sample$year,                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_NEG",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER       # Number of Iteration for Wild Cluster Bootstrap
)


plot_general_full_ns_neg_zlb <- GG_IRF_ONE(data = results_general_full_ns_neg_zlb,
                                           hhi_coef = FALSE, 
                                           y_lower = -1.5, 
                                           y_upper = 1.5, 
                                           title_name = "IRF: Full Sample - Negative MS - ZLB",
                                           time_name = "Time Horizon"
)

## Negative Shock - No-ZLB Period ---------------------------------------------+

df_general_full_ns_neg_no_zlb <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_NEG", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_neg_no_zlb <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_neg_no_zlb,      # Panel dataset
  data_sample       = zlb_sample$year,                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_NEG",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER       # Number of Iteration for Wild Cluster Bootstrap
)


plot_general_full_ns_neg_no_zlb <- GG_IRF_ONE(data = results_general_full_ns_neg_no_zlb,
                                           hhi_coef = FALSE, 
                                           y_lower = -1.5, 
                                           y_upper = 1.5, 
                                           title_name = "IRF: Full Sample - Negative MS - No ZLB",
                                           time_name = "Time Horizon"
)


### 3.1.3 Final Graph ----------------------------------------------------------

library(patchwork)


# Full Sample Graph by ZLB vs No-ZLB



## 3.2 Low- vs High Market Concentration --------------------------------------+



df_general_full_ns_neg_no_zlb <- df_hp_large |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_NEG", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")


results_general_full_ns_neg_no_zlb <- LP_LIN_PANEL(
  data_set          = df_general_full_ns_neg_no_zlb,      # Panel dataset
  data_sample       = no_zlb_sample$year,                  # Use full sample or subset
  endog_data        = "log_loan_amount",       # Endogenous variable
  lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
  cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
  shock             = "I_HHI_NS_NEG",      # Shock variable
  lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
  diff_shock        = TRUE,      # First difference of shock variable
  panel_model       = "within",  # Panel model type
  panel_effect      = PANEL_EFFECT,      # Panel effect type
  robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
  robust_cluster    = "time",      # time vs group
  c_exog_data       = NULL,      # Contemporaneous exogenous variables
  l_exog_data       = NULL,      # Lagged exogenous variables
  lags_exog_data    = NaN,       # Lag length for exogenous variables
  c_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
  l_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference lagged exogenous variables
  lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
  confint           = CI,      # Confidence interval width
  hor               = HOR,      # Time Horizon
  biter             = BITER       # Number of Iteration for Wild Cluster Bootstrap
)


plot_general_full_ns_neg_no_zlb <- GG_IRF_ONE(data = results_general_full_ns_neg_no_zlb,
                                           hhi_coef = FALSE, 
                                           y_lower = -1.5, 
                                           y_upper = 1.5, 
                                           title_name = "IRF: Full Sample & Total MS & ZLB",
                                           time_name = "Time Horizon"
)

















## 3.1 FULL SAMPLE - Log Loan Amount -------------------------------------------
df_hp_large_lp <- df_hp_large |> 
  # filter(d_hhi_indicator) |> 
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_ZLB2_NS", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")

df_hp_large_lp <- pdata.frame(df_hp_large_lp, index = c("fips", "year"))

results <- lpirfs::lp_lin_panel(
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
  confint = 1.67,
  hor = 6
)

plot(results)

## 3.2 ZLB Sample - Log Loan Amount --------------------------------------------

# Get ZLB Sample
df_zlb_sample <- df_hp_large |> 
  distinct(year, d_ffr_mean_1perc) |> 
  filter(d_ffr_mean_1perc == "1")


# Select ZLB years and Relevant Variables
df_hp_large_lp <- df_hp_large |> 
  # filter(year %in% df_zlb_sample$year) |>
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")

df_hp_large_lp <- pdata.frame(df_hp_large_lp, index = c("fips", "year"))


results <- lpirfs::lp_lin_panel(
  data_set = df_hp_large_lp,
  data_sample = df_zlb_sample$year,
  endog_data = "log_loan_amount",
  cumul_mult = TRUE,
  shock = "I_HHI_NS",
  diff_shock = TRUE,
  panel_model = "within",
  panel_effect = "twoways",
  robust_cov = "vcovSCC",
  c_fd_exog_data = colnames(df_hp_large_lp)[c(4, 6:10)],
  l_fd_exog_data = colnames(df_hp_large_lp)[c(4:10)],
  lags_fd_exog_data = 1,
  confint = 1.67,
  hor = 6
)

plot(results)

## 3.3 Non-ZLB Sample - Log Loan Amount ----------------------------------------

# Get Non-ZLB Sample
df_zlb_sample <- df_hp_large |> 
  distinct(year, d_ffr_mean_1perc) |> 
  filter(d_ffr_mean_1perc == "0")

# Select ZLB years and Relevant Variables
df_hp_large_lp <- df_hp_large |> 
  # filter(year %in% df_zlb_sample$year) |>
  dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS", "ur", 
                "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")

df_hp_large_lp <- pdata.frame(df_hp_large_lp, index = c("fips", "year"))

# LP
results <- lpirfs::lp_lin_panel(
  data_set = df_hp_large_lp,
  data_sample = df_zlb_sample$year,
  endog_data = "log_loan_amount",
  cumul_mult = TRUE,
  shock = "I_HHI_NS",
  diff_shock = TRUE,
  panel_model = "within",
  panel_effect = "twoways",
  robust_cov = "vcovSCC",
  c_exog_data = colnames(df_hp_large_lp)[c(4, 6:10)],
  l_exog_data = colnames(df_hp_large_lp)[c(4:10)],
  lags_exog_data = 1,
  confint = 1.67,
  hor = 6
)

plot(results)

## DEVELOPMENT SECTION ### -----------------------------------------------------




############################### END ###########################################+