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


# 01. Market Concentration =====================================================

## Year 2019 -------------------------------------------------------------------
# Get Data
df_hp <- LOAD("26_thesis_mpt_us_varcreation_treatment")

df_hhi_2019 <- df_hp |> 
  filter(year == 2019) |> 
  dplyr::select(fips, hhi) 

# Set tigris option to cache files locally (optional but speeds up repeat use)
options(tigris_use_cache = TRUE)

# Download county shapefiles (using the generalized boundaries for faster plotting)
# This shapefile includes all US counties (including Alaska and Hawaii)
counties_sf <- counties(cb = TRUE, resolution = "5m", year = 2020)

# Merge your data with the spatial counties data.
# The county shapefile uses 'GEOID' to store the county FIPS codes.
merged_data <- left_join(counties_sf, df_hhi_2019, by = c("GEOID" = "fips"))

# Create the map plot using a yellow-to-red color gradient for market concentration
us_map_sod_2019 <- ggplot(merged_data) +
  # Set the border color to black and adjust the size if desired (e.g., size = 0.1)
  geom_sf(aes(fill = hhi), color = "black", size = 0.1) +
  scale_fill_gradient(low = "yellow", high = "red", 
                      name = "Market\nConcentration (HHI)",
                      na.value = "grey90",
                      guide = guide_colorbar(
                        barwidth = unit(5, "cm"),
                        barheight = unit(0.5, "cm"),
                        title.position = "top",
                        title.hjust = 0.5
                      )) +
  labs(title = "Market Concentration by County (2019)",
       caption = "Source: Summary of Deposity - FFIEC") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    # Remove background grid lines and panel background
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    # Adjust legend appearance
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    # Remove axis titles, texts, and ticks
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    # Adjust title and caption styling
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = 0.8),
    plot.caption = element_text(size = 8)
  )


ggsave(filename = paste0(FIGURE, "us_market_concentration_map_2019.pdf"), 
       plot = us_map_sod_2019, 
       device = "pdf", 
       width = 10,   # adjust width as needed
       height = 7)   # adjust height as needed


## Year 2004 -------------------------------------------------------------------


df_hhi_2004 <- df_hp |> 
  filter(year == 2004) |> 
  dplyr::select(fips, hhi) 

# Set tigris option to cache files locally (optional but speeds up repeat use)
options(tigris_use_cache = TRUE)

# Download county shapefiles (using the generalized boundaries for faster plotting)
# This shapefile includes all US counties (including Alaska and Hawaii)
counties_sf <- counties(cb = TRUE, resolution = "5m", year = 2000)


# Merge your data with the spatial counties data.
# The county shapefile uses 'GEOID' to store the county FIPS codes.
merged_data <- counties_sf |> 
  mutate(fips = paste0(STATE, COUNTY)) |> 
  left_join(df_hhi_2004, by = c("fips"))

# Create the map plot using a yellow-to-red color gradient for market concentration
us_map_sod_2004 <- ggplot(merged_data) +
  # Set the border color to black and adjust the size if desired (e.g., size = 0.1)
  geom_sf(aes(fill = hhi), color = "black", size = 0.1) +
  scale_fill_gradient(low = "yellow", high = "red", 
                      name = "Market\nConcentration (HHI)",
                      na.value = "grey90",
                      guide = guide_colorbar(
                        barwidth = unit(5, "cm"),
                        barheight = unit(0.5, "cm"),
                        title.position = "top",
                        title.hjust = 0.5
                      )) +
  labs(title = "Market Concentration by County (2004)",
       caption = "Source: Summary of Deposity - FFIEC") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    # Remove background grid lines and panel background
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    # Adjust legend appearance
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    # Remove axis titles, texts, and ticks
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    # Adjust title and caption styling
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = 0.8),
    plot.caption = element_text(size = 8)
  )


ggsave(filename = paste0(FIGURE, "us_market_concentration_map_2004.pdf"), 
       plot = us_map_sod, 
       device = "pdf", 
       width = 10,   # adjust width as needed
       height = 7)   # adjust height as needed

# 02. Plot Monetary Shocks =====================================================

# Annual Monetary Shock 
df_monetary_shock <- LOAD(dfinput = "26_thesis_mpt_us_varcreation_treatment")

# Plot yearly NS
df_NS <- df_monetary_shock |> 
  distinct(year, NS_total)

annual_ms <- ggplot(df_NS, aes(x = year, y = NS_total)) +
  geom_point(size = 2) +
  labs(
    title = "Annual Monetary Shock \nby Nakamura & Steinsson (2018)",
    x = "Year",
    y = "Size of Monetary Shock"
  ) +
  scale_x_continuous(
    breaks = seq(2004, 2023, by = 2)
  ) +
  scale_y_continuous(
    # breaks = seq(-0.3, .2, by = .1),
    limit = c(-0.3, .2)
  ) +
  theme(
    title = element_text(size = 10, hjust = .5),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme_minimal()

# Monethly Monetary Shock
df_monthly_ms <- read_excel(
  path = paste0(A, "t_policy_shocks_us/NS_GSS_monetary_shock/ABJ-2024-monetary-policy-surprises.xlsx"),
  sheet = "Data"
)

df_monthly_ms <- df_monthly_ms |> 
  mutate(
    month = date(date),
  ) |> 
  dplyr::select(month, NS) |> 
  filter(month > as.Date("2003-12-01"))

month_ms <- ggplot(df_monthly_ms, aes(x = month, y = NS)) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Monetary Shock \nby Nakamura & Steinsson (2018)",
    x = "Month",
    y = "Size of Monetary Shock"
  ) +
  # scale_x_continuous(
  #   breaks = seq(2004, 2023, by = 2)
  # ) +
  scale_y_continuous(
    # breaks = seq(-0.3, .2, by = .1),
    limit = c(-0.3, .2)
  ) +
  theme(
    title = element_text(size = 10, hjust = .5),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme_minimal()


compare_ms <- annual_ms + month_ms


month <- seq(as.Date("1995-01-01"), as.Date("2024-12-01"), by = "month")
month <- data.frame(month = month)

df_monthly_ms <- month |> 
  left_join(df_monthly_ms, by = "month") |> 
  mutate()


# 03. Plot Total Mortgage Amount for New House Purchases and Refinancing =======

# Load data
df_hp_dep <- LOAD("25_thesis_mpt_us_varceation_outcome_hp_depsitory")
df_hp_ref <- LOAD("25_thesis_mpt_us_varceation_outcome_ref_depository")

# Collapse to Annual data + Merge both Datasets
# House Purchasing Variable
df_hp_annual <- df_hp_dep |> 
  select(year, loan_amount) |> 
  group_by(year) |> 
  summarize(
    `House Purchase` = sum(loan_amount)
  )

# Refinanced Mortgages
df_ref_annual <- df_hp_ref |> 
  select(year, loan_amount) |> 
  group_by(year) |> 
  summarize(
    Refinancing = sum(loan_amount)
  )

# Merge to one DF
mortgages <- df_hp_annual |> 
  left_join(df_ref_annual, by = c("year")) |> 
  pivot_longer(
    cols = c(`House Purchase`, Refinancing),
    names_to = "loan_type",
    values_to = "loan_amount"
  ) |> 
  mutate(
    loan_amount = loan_amount / 1000000 # Billion
  )

# Plot these
plot_mortgages_amount <- ggplot(mortgages, aes(x = factor(year), y = loan_amount, fill = loan_type)) +
  geom_bar(stat = "identity",  position = position_dodge2(preserve = "single", padding = 0), width = 0.77) +
  labs(title = "House Purchase and Refinancing Loans by Year",
       x = "Year",
       y = "Loan Amount in USD$",
       fill = "Loan Type") +
  scale_fill_manual(
    values = c(`House Purchase` = "#1f78b4", "Refinancing" = "#33a02c"),
    label = c(`House Purchase` = "House \nPurchase",
              "Refinancing" = "Refinancing")
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, " bn."),
    breaks = seq(0, 900, by = 100)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = .5),
    axis.title.y.left  = element_text(size = 10, margin = margin(r = 10)),
    axis.title.y.right = element_text(size = 10, margin = margin(l = 10)),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    # axis.title.x.bottom = element_text(size = 10, margin = margin(b = 10)),
    axis.title.x = element_text(size = 10, margin = margin(t = 10))
  )

# Save
ggsave(
  filename = paste0(FIGURE, "07_US_Descriptive_Statistics/us_mortgage_amount_over_time.pdf"),
  plot = plot_mortgages_amount,
  width = 10, 
  height = 6, 
  dpi = 600, 
  device = cairo_pdf
)

# 04.  Plot Total Mortgage Origination for New House Purchases and Refinancing #### 

# Collapse to Annual data + Merge both Datasets
# House Purchasing Variable
df_hp_annual <- df_hp_dep |> 
  select(year, nr_originated_loan ) |> 
  group_by(year) |> 
  summarize(
    `House Purchase` = sum(nr_originated_loan )
  )

# Refinanced Mortgages
df_ref_annual <- df_hp_ref |> 
  select(year, nr_originated_loan ) |> 
  group_by(year) |> 
  summarize(
    Refinancing = sum(nr_originated_loan )
  )

# Merge to one DF
mortgages_org <- df_hp_annual |> 
  left_join(df_ref_annual, by = c("year")) |> 
  pivot_longer(
    cols = c(`House Purchase`, Refinancing),
    names_to = "loan_type",
    values_to = "loan_amount"
  ) |> 
  mutate(
    loan_amount = loan_amount / 1000000 # in Mio 
  )

# Plot
plot_mortgages_nr <- ggplot(mortgages_org, aes(x = factor(year), y = loan_amount, fill = loan_type)) +
  geom_bar(stat = "identity",  position = position_dodge2(preserve = "single", padding = 0), width = 0.77) +
  labs(title = "House Purchase and Refinancing Loans by Year",
       subtitle = "Value: Number of Originations",
       x = "Year",
       y = "Number of Originations",
       fill = "Loan Type") +
  scale_fill_manual(
    values = c(`House Purchase` = "#1f78b4", "Refinancing" = "#33a02c"),
    label = c(`House Purchase` = "House \nPurchase",
              "Refinancing" = "Refinancing")
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, " Mio"),,
    breaks = seq(0, 4, by = 0.5)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.y.left  = element_text(size = 10, margin = margin(r = 10)),
    axis.title.y.right = element_text(size = 10, margin = margin(l = 10)),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10, margin = margin(t = 10))
  )

# Save
ggsave(
  filename = paste0(FIGURE, "07_US_Descriptive_Statistics/us_mortgage_originations_over_time.pdf"),
  plot = plot_mortgages_nr,
  width = 10, 
  height = 6, 
  dpi = 600, 
  device = cairo_pdf
)

# 05. Marketshare of Top 5 Banks over time =====================================

# load dataset
raw_sod <- LOAD(dfinput = "07_thesis_mpt_databasics_sod")
setDT(raw_sod)

# Market power of commercial banks
raw_sod <- raw_sod[, .(depsumbank = sum(depsumbr)), by = .(year, rssdid)]
raw_sod <- raw_sod[, tot_marketvalue_yearly  := as.numeric(sum(depsumbank)), by = year]
raw_sod <- raw_sod[, marketshare_yearly := depsumbank / tot_marketvalue_yearly]

# Filter data
# raw_sod <- raw_sod[marketshare_yearly > 0.01]


# Determine top 5 banks
top_banks <- raw_sod %>%
  group_by(year) %>%
  arrange(desc(marketshare_yearly)) %>%
  slice_head(n = 5) %>%
  ungroup()

# Sum up marketshare of the top 5 banks
marketshare <- top_banks |>
  group_by(year) |> 
  mutate(tot_market_share = sum( marketshare_yearly) * 100) |> 
  distinct(year, tot_market_share)

# Plotting the sum of market share of the top 5 banks
plot_marketshare_top5 <- ggplot(data = marketshare, aes(x = year, y = tot_market_share)) +
  geom_line(color = "black", size = 1) +  # Line color and thickness
  geom_point(color = "black", size = 2) +  # Point color and size
  labs(
    title = "Total Market Share of Top 5 Banks",
    x = "Year",
    y = "Market Share"
  ) +
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),  # Center title, increase size, bold
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),  # X-axis label formatting
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),  # Y-axis label formatting
    axis.text.x = element_text(angle = 0, hjust = 1)  # Rotate x-axis labels for readability
  ) +
  scale_x_continuous(breaks = seq(min(marketshare$year), max(marketshare$year), by = 2)) +  # Set x-axis breaks for each year
  scale_y_continuous(limits = c(0, 45),
                     breaks = seq(0, 45, by = 5),
                     labels = scales::percent_format(scale = 1)) 

# Save
ggsave(
  filename = paste0(FIGURE, "07_US_Descriptive_Statistics/us_market_share_top5.pdf"),
  plot = plot_marketshare_top5,
  width = 10, 
  height = 6, 
  dpi = 600, 
  device = cairo_pdf
)

# 06. Market Concentration ==========================

# Exclude all banks with a share less than 1% market share
market_concentration <- raw_sod |>  
  filter(marketshare_yearly > .01) |> 
  filter(year %in% c(2000, 2010, 2020)) |> 
  mutate(marketshare_yearly = marketshare_yearly * 100)

# Plotting market share density for each year for all banks with more than 1% market share
density_market_share <- ggplot(market_concentration, aes(x = marketshare_yearly, linetype = factor(year))) +
  geom_density(linewidth = 1) +
  labs(
    title = "Distribution of Market Concentration",
    x = "Market Share in %", 
    y = "Density",,
    caption = "Source: Summary of Deposits | Data includes all banks with a market share with more than 1%.",
    linetype = "Year"
    ) +
  scale_y_continuous(
    breaks = seq(0, .7, by = .1),
    limit = c(0, .7)
  ) +
  scale_x_continuous(
    breaks = seq(0, 12.5, by = 2.5),
    limits = c(0, 12.5)
  ) +
  scale_linetype_manual(values = c("2000" = "dotted", "2010" = "dashed", "2020" = "solid")) +
  theme(
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)), 
    plot.caption = element_text(size = 8),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    axis.text.x = element_text(angle = 0, hjust = 1)
  ) +
  theme_minimal()

# Save 
ggsave(
  filename = paste0(FIGURE, "07_US_Descriptive_Statistics/us_densityplot_market_share.pdf"),
  plot = plot_marketshare_top5,
  width = 10, 
  height = 6, 
  dpi = 600, 
  device = cairo_pdf
)

# 07. Correlation between Rate Spread and HHI ==================================

df_main <- LOAD("29_thesis_mpt_us_samplecreation_main_hp_large")

df_corr <- df_main |> 
  filter(year %in% c(2007, 2008, 2009)) |> 
  select(year, fips, log_loan_amount_pc, rate_spread_wloan, hhi) 


ggplot(df_corr, aes(x = hhi, y = log_loan_amount_pc, color = factor(year))) +
  geom_point(size = .9) + 
  geom_smooth(method = "lm" )



