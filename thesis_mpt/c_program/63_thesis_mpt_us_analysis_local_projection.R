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
start <- Sys.time()
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
    ) |> 
  mutate(
    log_loan_amount_pc = log(loan_amount / cnty_pop.x)
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
if (DEBUG) {
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

###############################################################################+
## 3.1 Full Time Period ########################################################
###############################################################################+

### 3.1.1 Full Sample ----------------------------------------------------------

# Define the six specifications in a list
shock_specs <- list(
  total = list(
    shock_var   = "I_HHI_NS_TOTAL_2",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_TOTAL_2",  
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us")
  ),
  positive = list(
    shock_var   = "I_HHI_NS_POS_2",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_POS_2",  
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us")
  ),
  negative = list(
    shock_var   = "I_HHI_NS_NEG_2",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_NEG_2",  
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us")
  ),
  total_ref = list(
    shock_var   = "I_HHI_NS_TOTAL",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_TOTAL",  
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us")
  ),
  positive_ref = list(
    shock_var   = "I_HHI_NS_POS",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_POS",  
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us")
  ),
  negative_ref = list(
    shock_var   = "I_HHI_NS_NEG",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_NEG",  
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us")
  )
)

# Set up the parallel backend (use available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run the LP_LIN_PANEL function in parallel for each specification
results_full_sample <- foreach(spec = shock_specs,
                   .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"),
                   .export = c("df_hp_large", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR", "BITER")
) %dopar% {
  
  # Subset the data according to the current specification
  df_subset <- df_hp_large %>% select(all_of(spec$select_cols))
  
  # Run the model estimation
  LP_LIN_PANEL(
    data_set          = df_subset,
    data_sample       = "Full",
    endog_data        = "log_loan_amount",
    lags_endog_data   = 2,
    cumul_mult        = TRUE,
    shock             = spec$shock_var,
    lags_shock        = 2,
    diff_shock        = TRUE,
    panel_model       = "within",
    panel_effect      = PANEL_EFFECT,
    robust_cov        = "wild.cluster.boot",
    robust_cluster    = "time",
    c_exog_data       = NULL,
    l_exog_data       = NULL,
    lags_exog_data    = NaN,
    c_fd_exog_data    = colnames(df_subset)[c(4, 6:10)],
    l_fd_exog_data    = colnames(df_subset)[c(4, 6:10)],
    lags_fd_exog_data = 2,
    confint           = CI,
    hor               = HOR,
    biter             = BITER
  )
}

# Optionally, assign names to the results list based on your shock specifications
names(results) <- names(shock_specs)

# Shut down the parallel cluster
stopCluster(cl)

# Clear garbage
gc()

###############################################################################+
### 3.1.2 Graphs - Full Sample ------------------------------------------------- 
###############################################################################+

# Retrieve all Graphs
p1_full <- results$total
p2_full <- results$positive
p3_full <- results$negative
p4_full <- results$total_ref
p5_full <- results$positive_ref
p6_full <- results$negative_ref


## Graphs with Interaction Terms ----------------------------------------------+

# Interaction Term - Total Shock
plot_general_full_ns_total <- GG_IRF_ONE(data = p1_full,
                                   hhi_coef = FALSE, 
                                   y_lower = -1.5, 
                                   y_upper = 1.5, 
                                   breaks = .5,
                                   title_name = "IRF: Full Sample - Total MS",
                                   time_name = "Time Horizon"
                                   )

# Interaction Term - Positive Shock
plot_general_full_ns_pos <- GG_IRF_ONE(data = p2_full,
                                       hhi_coef = FALSE, 
                                       y_lower = -2.5, 
                                       y_upper = 2,
                                       breaks = .5,
                                       title_name = "IRF: Full Sample - Positive MS",
                                       time_name = "Time Horizon"
)

# Interaction Term - Negative Shock
plot_general_full_ns_neg <- GG_IRF_ONE(data = p3_full,
                                       hhi_coef = FALSE, 
                                       y_lower = -1.5, 
                                       y_upper = 1.5,
                                       breaks = .5,
                                       title_name = "IRF: Full Sample - Negative MS",
                                       time_name = "Time Horizon"
)

## Graphs - Reference Period --------------------------------------------------+

# Reference - Total Shock
plot_general_full_ns_total_ref <- GG_IRF_ONE(data = p4_full,
                                             hhi_coef = FALSE, 
                                             y_lower = -1.5, 
                                             y_upper = 1.5, 
                                             breaks = .5,
                                             title_name = "IRF: Full Sample - Total MS - Reference",
                                             time_name = "Time Horizon"
)

# Reference - Positive Shock
plot_general_full_ns_pos_ref <- GG_IRF_ONE(data = p5_full,
                                           hhi_coef = FALSE, 
                                           y_lower = -3, 
                                           y_upper = 7,
                                           breaks = 1,
                                           title_name = "IRF: Full Sample - Positive MS - Reference",
                                           time_name = "Time Horizon"
)

# Reference - Negative Sock
plot_general_full_ns_neg_ref <- GG_IRF_ONE(data = p6_full,
                                           hhi_coef = FALSE, 
                                           y_lower = -1.5, 
                                           y_upper = 1.5,
                                           breaks = .5,
                                           title_name = "IRF: Full Sample - Negative MS - Reference",
                                           time_name = "Time Horizon"
)

## Create Final Graphs --------------------------------------------------------+

# Full Sample Graph for Interaction Term with ZLB Indicator 
graph_full_withInd <- plot_general_full_ns_total$plot / plot_general_full_ns_pos$plot / plot_general_full_ns_neg$plot
ggsave(filename = paste0(FIGURE, "graph_full_withInd.pdf"), graph_full_withInd, width = 8, height = 12, dpi = 300)

# Full Sample Graph for Interaction Term without ZLB Indicator 
graph_full_withoutInd <- plot_general_full_ns_total_ref$plot / plot_general_full_ns_pos_ref$plot / plot_general_full_ns_neg_ref$plot
ggsave(filename = paste0(FIGURE, "graph_full_withoutInd.pdf"), graph_full_withoutInd, width = 8, height = 12, dpi = 300)

# Full Sample Graph for Interaction Term with ZLB Indicator 
# Create header grobs for the two columns
header_left <- wrap_elements(full = grid::textGrob("Graphs with Interaction Term",
                                                   gp = grid::gpar(fontsize = 8, fontface = "bold")))
header_right <- wrap_elements(full = grid::textGrob("Reference",
                                                    gp = grid::gpar(fontsize = 8, fontface = "bold")))


graph_full_final <- (header_left + header_right) /
                    (plot_general_full_ns_total$plot + plot_general_full_ns_total_ref$plot)  / 
                    (plot_general_full_ns_pos$plot + plot_general_full_ns_pos_ref$plot) / 
                    (plot_general_full_ns_neg$plot + plot_general_full_ns_neg_ref$plot) +
                     plot_layout(heights = c(0.2, 1, 1, 1)) + 
                     plot_annotation(title = "Impulse Response Function",
                                     theme = theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)))

ggsave(filename = paste0(FIGURE, "graph_full_final.pdf"), final_plot, width = 8, height = 12, dpi = 300)



###############################################################################+
### 3.2.3 ZLB vs No-ZLB period #################################################
###############################################################################+

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


# Create a list of specifications for the six models
# For each specification, we define:
# - the shock variable,
# - the columns to select from df_hp_large, and 
# - the sample years to use (ZLB or No-ZLB)
spec_list <- list(
  total_zlb = list(
    shock_var   = "I_HHI_NS_TOTAL",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_TOTAL", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us"),
    sample_year = zlb_sample$year
  ),
  total_no_zlb = list(
    shock_var   = "I_HHI_NS_TOTAL",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_TOTAL", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us"),
    sample_year = no_zlb_sample$year
  ),
  pos_zlb = list(
    shock_var   = "I_HHI_NS_POS",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_POS", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us"),
    sample_year = zlb_sample$year
  ),
  pos_no_zlb = list(
    shock_var   = "I_HHI_NS_POS",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_POS", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us"),
    sample_year = no_zlb_sample$year
  ),
  neg_zlb = list(
    shock_var   = "I_HHI_NS_NEG",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_NEG", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us"),
    sample_year = zlb_sample$year
  ),
  neg_no_zlb = list(
    shock_var   = "I_HHI_NS_NEG",
    select_cols = c("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_NEG", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us"),
    sample_year = no_zlb_sample$year
  )
)

# Set up the parallel backend (using available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run LP_LIN_PANEL in parallel for each specification
results_subsample <- foreach(spec = spec_list,
                   .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"),
                   .export = c("df_hp_large", "LP_LIN_PANEL", "CREATE_PANEL_DATA", "PANEL_EFFECT", "CI", "HOR", "BITER")) %dopar% {
  
  # Subset the data using the specified columns
  df_subset <- df_hp_large %>% select(all_of(spec$select_cols))
  
  # Run the LP_LIN_PANEL model
  LP_LIN_PANEL(
    data_set          = df_subset,
    data_sample       = spec$sample_year,
    endog_data        = "log_loan_amount",
    lags_endog_data   = 2,
    cumul_mult        = TRUE,
    shock             = spec$shock_var,
    lags_shock        = 2,
    diff_shock        = TRUE,
    panel_model       = "within",
    panel_effect      = PANEL_EFFECT,
    robust_cov        = "wild.cluster.boot",
    robust_cluster    = "time",
    c_exog_data       = NULL,
    l_exog_data       = NULL,
    lags_exog_data    = NaN,
    c_fd_exog_data    = colnames(df_subset)[c(4, 6:10)],
    l_fd_exog_data    = colnames(df_subset)[c(4, 6:10)],
    lags_fd_exog_data = 2,
    confint           = CI,
    hor               = HOR,
    biter             = BITER
  )
}

# Assign names to the results list based on the specification names
names(results_subsample) <- names(spec_list)

# Shut down the parallel cluster
stopCluster(cl)



###############################################################################+
# 3.1.4 Graph - Comparison between ZLB and No-ZLB Period #######################
###############################################################################+

# Retrieve results
p7_subsample <- results_subsample$total_zlb
p8_subsample <- results_subsample$total_no_zlb
p9_subsample <- results_subsample$pos_zlb
p10_subsample <- results_subsample$pos_no_zlb
p11_subsample <- results_subsample$neg_zlb
p12_subsample <- results_subsample$neg_no_zlb




## Total Shock ----------------------------------------------------------------+

# Total Shock - ZLB 
plot_general_full_ns_total_zlb <- GG_IRF_ONE(data = p7_subsample,
                                         hhi_coef = FALSE, 
                                         y_lower = -2.5, 
                                         y_upper = 1.5,
                                         breaks = .5,
                                         title_name = "ZLB Sample - Total MS",
                                         time_name = "Time Horizon"
)

# Total Shock - No ZLB
plot_general_full_ns_total_no_zlb <- GG_IRF_ONE(data = p8_subsample,
                                             hhi_coef = FALSE, 
                                             y_lower = -2.5, 
                                             y_upper = 3.5,
                                             breaks = .5,
                                             title_name = "No ZLB Sample - Total MS",
                                             time_name = "Time Horizon"
)

## Positive Shock -------------------------------------------------------------+ 

# Positive Shock - ZLB
plot_general_full_ns_pos_zlb <- GG_IRF_ONE(data = p9_subsample,
                                           hhi_coef = FALSE, 
                                           y_lower = -3, 
                                           y_upper = 2,
                                           breaks = .5,
                                           title_name = "ZLB Sample - Positive MS",
                                           time_name = "Time Horizon"
)

# Positive Shock - No ZLB
plot_general_full_ns_pos_no_zlb <- GG_IRF_ONE(data = p10_subsample,
                                           hhi_coef = FALSE, 
                                           y_lower = -3, 
                                           y_upper = 4, 
                                           breaks = .5,
                                           title_name = "No ZLB Sample - Positive MS",
                                           time_name = "Time Horizon"
)

## Negative Shock -------------------------------------------------------------+

# Negative Shock - ZLB
plot_general_full_ns_neg_zlb <- GG_IRF_ONE(data = p11_subsample,
                                           hhi_coef = FALSE, 
                                           y_lower = -3, 
                                           y_upper = 4,
                                           breaks = .5,
                                           title_name = "ZLB Sample - Negative MS",
                                           time_name = "Time Horizon"
)

# Negative Shock - No ZLB
plot_general_full_ns_neg_no_zlb <- GG_IRF_ONE(data = p12_subsample,
                                           hhi_coef = FALSE, 
                                           y_lower = -4, 
                                           y_upper = 6, 
                                           breaks = .5,
                                           title_name = "No ZLB Sample - Negative MS",
                                           time_name = "Time Horizon"
)


# Easier notation
p7_plot_subsample  <- plot_general_full_ns_total_zlb$plot
p8_plot_subsample  <- plot_general_full_ns_total_no_zlb$plot
p9_plot_subsample  <- plot_general_full_ns_pos_zlb$plot
p10_plot_subsample <- plot_general_full_ns_pos_no_zlb$plot
p11_plot_subsample <- plot_general_full_ns_neg_zlb$plot
p12_plot_subsample <- plot_general_full_ns_neg_no_zlb$plot

library(grid)
# Create column header elements using wrap_elements()
header1 <- wrap_elements(full = textGrob("Period: ZLB", gp = gpar(fontsize = 8, fontface = "bold")))
header2 <- wrap_elements(full = textGrob("Period: No ZLB", gp = gpar(fontsize = 8, fontface = "bold")))

# Combine headers and plots into one layout
zlb_vs_nozlb_layout <- (header1 + header2) /  # Header row
  (p7_plot_subsample    + p8_plot_subsample) /          # First row of plots
  (p9_plot_subsample    + p10_plot_subsample) /          # Second row of plots
  (p11_plot_subsample   + p12_plot_subsample) +
  plot_layout(heights = c(0.2, 1, 1, 1))  # Adjust relative heights

# Save
ggsave(filename = paste0(FIGURE, "zlb_vs_nozlb_layout.pdf"), zlb_vs_nozlb_layout, width = 8, height = 12, dpi = 300)



###############################################################################+
# 3.2 Low- vs High Market Concentration ########################################
###############################################################################+

# Create Low- and High-Concentration Counties
df_hp_large_highconc <- df_hp_large |> 
  filter(d_hhi_indicator == 1)
  
df_hp_large_lowconc <- df_hp_large |> 
  filter(d_hhi_indicator == 0)


# Define a list of specifications for the six models
# Each element includes:
# - data_set: the appropriate subset (high or low concentration)
# - shock_var: the shock variable name for that model
# - select_cols: the columns to select (including the appropriate shock variable)
spec_list <- list(
  high_total = list(
    data_set    = df_hp_large_highconc,
    shock_var   = "I_HHI_NS_TOTAL_2",
    select_cols = c("fips", "year", "log_loan_amount_pc", "hhi", "I_HHI_NS_TOTAL_2", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us", )
  ),
  high_positive = list(
    data_set    = df_hp_large_highconc,
    shock_var   = "I_HHI_NS_POS_2",
    select_cols = c("fips", "year", "log_loan_amount_pc", "hhi", "I_HHI_NS_POS_2", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us")
  ),
  high_negative = list(
    data_set    = df_hp_large_highconc,
    shock_var   = "I_HHI_NS_NEG_2",
    select_cols = c("fips", "year", "log_loan_amount_pc", "hhi", "I_HHI_NS_NEG_2", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us")
  ),
  low_total = list(
    data_set    = df_hp_large_lowconc,
    shock_var   = "I_HHI_NS_TOTAL_2",
    select_cols = c("fips", "year", "log_loan_amount_pc", "hhi", "I_HHI_NS_TOTAL_2", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us")
  ),
  low_positive = list(
    data_set    = df_hp_large_lowconc,
    shock_var   = "I_HHI_NS_POS_2",
    select_cols = c("fips", "year", "log_loan_amount_pc", "hhi", "I_HHI_NS_POS_2", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us")
  ),
  low_negative = list(
    data_set    = df_hp_large_lowconc,
    shock_var   = "I_HHI_NS_NEG_2",
    select_cols = c("fips", "year", "log_loan_amount_pc", "hhi", "I_HHI_NS_NEG_2", 
                    "ur", "log_median_household_income", "hpi_annual_change_perc", 
                    "inflation_us", "gdp_growth_us")
  )
)

# Set up the parallel backend (using available cores minus one)
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run the LP_LIN_PANEL function in parallel for each specification
results_high_low <- foreach(spec = spec_list,
                   .packages = c("dplyr", "lpirfs", "plm", "clusterSEs", "lmtest"),
                   .export = c("LP_LIN_PANEL", "CREATE_PANEL_DATA" ,"PANEL_EFFECT", "CI", "HOR", "BITER")) %dopar% {
  
  # Subset the data using the specified columns
  df_subset <- spec$data_set %>% select(all_of(spec$select_cols))
  
  # Run the LP_LIN_PANEL model
  LP_LIN_PANEL(
    data_set          = df_subset,
    data_sample       = "Full",
    endog_data        = "log_loan_amount_pc",
    lags_endog_data   = 3,
    cumul_mult        = TRUE,
    shock             = spec$shock_var,
    lags_shock        = 3,
    diff_shock        = TRUE,
    panel_model       = "within",
    panel_effect      = PANEL_EFFECT,
    robust_cov        = "wild.cluster.boot",
    # robust_maxlag     = 2,
    # robust_type       = "HC1",
    robust_cluster    = "time",
    c_exog_data       = NULL,
    l_exog_data       = NULL,
    lags_exog_data    = NaN,
    c_fd_exog_data    = colnames(df_subset)[c(4, 6:10)],
    l_fd_exog_data    = colnames(df_subset)[c(4, 6:10)],
    lags_fd_exog_data = 3,
    confint           = CI,
    hor               = HOR,
    biter             = BITER
  )
}

# Assign names to the results list based on the specification names
names(results_high_low) <- names(spec_list)

# Shut down the parallel cluster
stopCluster(cl)


###############################################################################+
## 3.2.3 Graph - Comparison High vs Low Market Concentration Market ############
###############################################################################+

# Retrieve Data from list
p13_high_low <- results_high_low$high_total
p14_high_low <- results_high_low$high_positive
p15_high_low <- results_high_low$high_negativ
p16_high_low <- results_high_low$low_total
p17_high_low <- results_high_low$low_positive
p18_high_low <- results_high_low$low_negative

# Create Graph for comparison between Low vs High Market Concentration
graph_high_low_total <- GG_IRF_TWO(data1 = p13_high_low,
                                   data2 = p16_high_low,
                                   data_name = c("High HHI", "Low HHI"),
                                   hhi_coef = FALSE, 
                                   y_lower = -5,
                                   y_upper = 6,
                                   breaks = 1,
                                   name = "Total MS")

graph_high_low_pos <- GG_IRF_TWO(data1 =  p14_high_low,
                                 data2 = p17_high_low,
                                 data_name = c("High HHI", "Low HHI"),
                                 hhi_coef = FALSE, 
                                 y_lower = -1.5,
                                 y_upper = 1.5,
                                 name = "Positive MS"
                                 )

graph_high_low_negativ <- GG_IRF_TWO(data1 =  p15_high_low,
                                     data2 = p18_high_low,
                                     data_name = c("High HHI", "Low HHI"),
                                     hhi_coef = FALSE, 
                                     y_lower = -1.5,
                                     y_upper = 1.5,
                                     name = "Negative MS"
                                     )



graph_combined_high_low <- graph_high_low_total$plot /
                           graph_high_low_pos$plot   /
                           graph_high_low_negativ$plot +
                           plot_annotation(
                             title = "IRF of Mortgage Amount on Shock",
                             subtitle =  "High vs Low Market Concentration Counties",
                             theme =  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                                            plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5))
                           )


ggsave(filename = paste0(FIGURE, "graph_combined_high_low_pc.pdf"), graph_combined_high_low, width = 8, height = 12, dpi = 300)



############################### END ###########################################+



# 
# df_general_full_ns_neg_no_zlb <- df_hp_large |> 
#   dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS_NEG", "ur", 
#                 "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")
# 
# 
# results_general_full_ns_neg_no_zlb <- LP_LIN_PANEL(
#   data_set          = df_general_full_ns_neg_no_zlb,      # Panel dataset
#   data_sample       = no_zlb_sample$year,                  # Use full sample or subset
#   endog_data        = "log_loan_amount",       # Endogenous variable
#   lags_endog_data   = 2,                       # NEW: Determine lags for endogenous Variable
#   cumul_mult        = TRUE,                    # Estimate cumulative multipliers?
#   shock             = "I_HHI_NS_NEG",      # Shock variable
#   lags_shock        = 2,                      # NEW: Determine lags for shock variable (for level & FD)
#   diff_shock        = TRUE,      # First difference of shock variable
#   panel_model       = "within",  # Panel model type
#   panel_effect      = PANEL_EFFECT,      # Panel effect type
#   robust_cov        = "wild.cluster.boot",      # Robust covariance estimation method: NEW: tLAHR & WCD
#   robust_cluster    = "time",      # time vs group
#   c_exog_data       = NULL,      # Contemporaneous exogenous variables
#   l_exog_data       = NULL,      # Lagged exogenous variables
#   lags_exog_data    = NaN,       # Lag length for exogenous variables
#   c_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference contemporaneous exogenous variables
#   l_fd_exog_data    = colnames(df_general_full_ns)[c(4, 6:10)],      # First-difference lagged exogenous variables
#   lags_fd_exog_data = 2,       # Lag length for first-difference exogenous variables
#   confint           = CI,      # Confidence interval width
#   hor               = HOR,      # Time Horizon
#   biter             = BITER       # Number of Iteration for Wild Cluster Bootstrap
# )
# 
# 
# plot_general_full_ns_neg_no_zlb <- GG_IRF_ONE(data = results_general_full_ns_neg_no_zlb,
#                                            hhi_coef = FALSE, 
#                                            y_lower = -1.5, 
#                                            y_upper = 1.5, 
#                                            title_name = "IRF: Full Sample & Total MS & ZLB",
#                                            time_name = "Time Horizon"
# )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## 3.1 FULL SAMPLE - Log Loan Amount -------------------------------------------
# df_hp_large_lp <- df_hp_large |> 
#   # filter(d_hhi_indicator) |> 
#   dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_ZLB2_NS", "ur", 
#                 "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")
# 
# df_hp_large_lp <- pdata.frame(df_hp_large_lp, index = c("fips", "year"))
# 
# results <- lpirfs::lp_lin_panel(
#   data_set = df_hp_large_lp,
#   data_sample = "Full",
#   endog_data = "log_loan_amount",
#   cumul_mult = TRUE,
#   shock = "I_HHI_ZLB2_NS",
#   diff_shock = TRUE,
#   panel_model = "within",
#   panel_effect = "twoways",
#   robust_cov = "vcovSCC",
#   c_fd_exog_data = colnames(df_hp_large_lp)[c(4, 6:10)],
#   l_fd_exog_data = colnames(df_hp_large_lp)[c(4:10)],
#   lags_fd_exog_data = 1,
#   confint = 1.67,
#   hor = 6
# )
# 
# plot(results)
# 
# ## 3.2 ZLB Sample - Log Loan Amount --------------------------------------------
# 
# # Get ZLB Sample
# df_zlb_sample <- df_hp_large |> 
#   distinct(year, d_ffr_mean_1perc) |> 
#   filter(d_ffr_mean_1perc == "1")
# 
# 
# # Select ZLB years and Relevant Variables
# df_hp_large_lp <- df_hp_large |> 
#   # filter(year %in% df_zlb_sample$year) |>
#   dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS", "ur", 
#                 "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")
# 
# df_hp_large_lp <- pdata.frame(df_hp_large_lp, index = c("fips", "year"))
# 
# 
# results <- lpirfs::lp_lin_panel(
#   data_set = df_hp_large_lp,
#   data_sample = df_zlb_sample$year,
#   endog_data = "log_loan_amount",
#   cumul_mult = TRUE,
#   shock = "I_HHI_NS",
#   diff_shock = TRUE,
#   panel_model = "within",
#   panel_effect = "twoways",
#   robust_cov = "vcovSCC",
#   c_fd_exog_data = colnames(df_hp_large_lp)[c(4, 6:10)],
#   l_fd_exog_data = colnames(df_hp_large_lp)[c(4:10)],
#   lags_fd_exog_data = 1,
#   confint = 1.67,
#   hor = 6
# )
# 
# plot(results)
# 
# ## 3.3 Non-ZLB Sample - Log Loan Amount ----------------------------------------
# 
# # Get Non-ZLB Sample
# df_zlb_sample <- df_hp_large |> 
#   distinct(year, d_ffr_mean_1perc) |> 
#   filter(d_ffr_mean_1perc == "0")
# 
# # Select ZLB years and Relevant Variables
# df_hp_large_lp <- df_hp_large |> 
#   # filter(year %in% df_zlb_sample$year) |>
#   dplyr::select("fips", "year", "log_loan_amount", "hhi", "I_HHI_NS", "ur", 
#                 "log_median_household_income", "hpi_annual_change_perc", "inflation_us", "gdp_growth_us")
# 
# df_hp_large_lp <- pdata.frame(df_hp_large_lp, index = c("fips", "year"))
# 
# # LP
# results <- lpirfs::lp_lin_panel(
#   data_set = df_hp_large_lp,
#   data_sample = df_zlb_sample$year,
#   endog_data = "log_loan_amount",
#   cumul_mult = TRUE,
#   shock = "I_HHI_NS",
#   diff_shock = TRUE,
#   panel_model = "within",
#   panel_effect = "twoways",
#   robust_cov = "vcovSCC",
#   c_exog_data = colnames(df_hp_large_lp)[c(4, 6:10)],
#   l_exog_data = colnames(df_hp_large_lp)[c(4:10)],
#   lags_exog_data = 1,
#   confint = 1.67,
#   hor = 6
# )
# 
# plot(results)

## DEVELOPMENT SECTION ### -----------------------------------------------------



