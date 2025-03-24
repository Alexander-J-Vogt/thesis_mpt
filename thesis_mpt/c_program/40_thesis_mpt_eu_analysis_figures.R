# TARGET: Create Figures for Thesis
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

# 01. Display Market Concentration =============================================

## 01.1 Banking Structural Statistical Indicator ===============================

df_bsi <- LOAD(dfinput = "03_thesis_mpt_databasics_ecb_a")
df_worldscope <- LOAD(dfinput = "05_thesis_mpt_databasics_worldscope")

# Restrict to the years tha will be observed
df_bsi <- df_bsi |> 
  dplyr::select(country, year, hhi_ci_total_assets) |> 
  filter(!(country == "GR" & year < 2001)) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & year < 2007)) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & year < 2009)) # Filter for years with SK being part of the Eurozone

# Restrict sample by countries
df_bsi_large_countries <- df_bsi |> 
  filter(country %in% c("AT", "BE", "DE", "FR", "ES", "IT", "NL"))

df_bsi_small_countries <- df_bsi |> 
  filter(!(country %in%  c("AT", "BE", "DE", "FR", "ES", "IT", "NL")))

# Vector over the three different df
df_names <- c("df_bsi", "df_bsi_large_countries", "df_bsi_small_countries")

# Generate three graphs
for (names in df_names) {
  
  data <- get(names)
  
  graph <- ggplot(data = data, aes(x = year, y = hhi_ci_total_assets, color = country)) +
      geom_point(alpha = 0.7, size = 2) +  # Slight transparency and size adjustment
      geom_line(size = 0.6) + # Increase line width for better visibility
      labs(
        title = "Herfindahl-Hirschman Index (HHI) of Total Assets by Country",
        x = "Year",
        y = "HHI",
        color = "Country",
        caption = "Source: Banking Structural Statistical Indicators"
      ) +
      theme_minimal() +  # Apply a clean and simple theme
      theme(
        plot.title = element_text(size = 12, face = "bold"),  # Center and bold title
        axis.title = element_text(size = 10),  # Emphasize axis titles
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Move legend to the bottom
        # legend.title = element_text(),  # Emphasize legend title
        plot.caption = element_text(size = 10, hjust = 0) 
      ) +
      scale_x_continuous(breaks = unique(df_bsi$year))
    
  name <- str_replace(names, "df_", "")
  
  ggsave(
    filename = paste0(FIGURE,"08_EA_Descriptive_Statistics/", paste0("graph_", name, ".pdf")),  
    plot = graph,                   
    device = "pdf",                   
    width = 10,                       
    height = 6,                       
    units = "in"                      
  )
}

## 01.2 Top 5 Credit Institutions ==============================================

df_ci_top5 <- df_bsi |> 
  dplyr::select(year, country, share_top5_largest_ci_total_asset) |> 
  rename(
    share_top5 = share_top5_largest_ci_total_asset
  ) |> 
  group_by(year) |> 
  summarize(
    q1 = quantile(share_top5, probs = 0.25),
    q3 = quantile(share_top5, probs = 0.75),
    q2 = quantile(share_top5, probs = 0.5),
    mean = mean(share_top5)
  )

# Define custom color mapping
colors <- c("Interquartile Range" = "#003399", "Mean" = "orange")

market_concentration <- ggplot(df_ci_top5) +
  # Interquartile range (Q1 to Q3) using geom_tile for legend support
  geom_tile(aes(x = year, y = (q1 + q3) / 2, height = q3 - q1, fill = "Interquartile Range"), 
            width = 0.5, color = "#003399") + 
  # Mean line
  geom_line(aes(x = year, y = mean, color = "Mean"), size = 2) + 
  geom_point(aes(x = year, y = mean, color = "Mean"), size = 2) + 
  
  # Define legend colors
  scale_fill_manual(name = "", values = colors) +
  scale_color_manual(name = "", values = colors) +
  
  # Adjust axes
  scale_y_continuous(
    breaks = seq(30, 90, by = 10),
    limits = c(30, 90),
    labels = function(x) paste0(x, " %")
  ) +
  scale_x_continuous(
    breaks = seq(1999, 2023, by = 1)
  ) +
  
  # Labels
  labs(
    title = "Interquartile Range of Market Share",
    x = "",
    y = "Market Concentration",
    caption = "Source: BSI from the ECB - Annual Data on the market share of the top 5 banks in each country."
  ) +
  
  # Theme modifications
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"  # Legend placed beside the plot
  )

# SAVE
ggsave(
  filename = paste0(FIGURE,"08_EA_Descriptive_Statistics/", "ea_market_concentration_top5_banks.pdf"),  
  plot = market_concentration,                   
  device = "pdf",                   
  width = 10,                       
  height = 6,                       
  units = "in"                      
)

# 2. Monetary Shock ============================================================

df_monetary_shock <- LOAD("24_thesis_mpt_databasics_monetary_shock_eurozone")

monetary_shock <- df_monetary_shock |> 
  dplyr::select(month, MP_median, altavilla_total) |> 
  # mutate(across(-1, ~ if_else(.x == 0, NA, .x))) |> 
  mutate(MP_median = MP_median * 100)
  

# Plot Monetary Shock of Jarcinski & Karadi (2020) based on the Median Value
ms_jk <- ggplot(monetary_shock, aes(x = month, y = (MP_median))) +
  geom_col(color = "red", fill = "red") +
  scale_x_continuous(
    breaks = seq(as.Date("2000-01-01"), as.Date("2023-12-01"), by = "2 year"),
    labels = function(x) lubridate::year(x)
  ) +
  scale_y_continuous(
    breaks = seq(-20, 25, by = 5),
    limit = c(-20, 25)
  ) + 
  labs(
    title = "Jarocinski & Karadi (2020)",
    # subtitle = "Unit: Percentage Points",
    y = "Percentage Points",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45)
  ) 

# Monetary Shock: Altavilla et al. (2019) 
ms_altavilla <- ggplot(monetary_shock, aes(x = month, y = altavilla_total)) + 
  geom_col(color = "blue", fill = "blue") +
  scale_x_continuous(
    breaks = seq(as.Date("2000-01-01"), as.Date("2023-12-01"), by = "2 year"),
    labels = function(x) lubridate::year(x)
  ) +
  scale_y_continuous(
    breaks = seq(-20, 25, by = 5),
    limit = c(-20, 25)
  ) + 
  labs(
    title = "Altavilla et al. (2019)",
    # subtitle = "Unit: Percentage Points",
    y = "Standard Deviation",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Compare both Monetary Shocks
combined_ms <- ms_jk + ms_altavilla +
  plot_annotation(
    title = "Comparsion of Monetary Shock Measures",
    theme = theme(
      plot.title = element_text(size = 16, hjust = .03)
    ))

# Save
ggsave(
  filename = paste0(FIGURE,"08_EA_Descriptive_Statistics/", "ea_monetary_shock.pdf"),  
  plot = combined_ms,                   
  device = "pdf",                   
  width = 12,                       
  height = 4,                       
  units = "in"                      
)



# 3. Summary of Statistics ======================================================

# Load 
df_main <- LOAD("39_thesis_mpt_eu_samplecreation_main_m")

# Select variables for
df_summary <- df_main |> 
  dplyr::select(log_hp_total_amount, lending_rate_total, MP_median, 
        hhi_ci_total_assets, log_assets, log_overnight_deposits, log_total_loan, hicp, reer, ur, commodity_index, deposit_rate, 
        gdp, hpi, hosr, gdp_ea, hicp_ea, exr
        
        )

stargazer(
  df_summary, 
  type = "text",
  covariate.labels = c("ln Total Mortgage Amount (Domestic)",
                       "Lending Rate (Total)",
                       "MS - Jarocinsk/Karadi (2020)",
                       "HHI (based on Total Assets)",
                       "ln Total Assets (Domestic)",
                       "ln Overnight Deposits (Domestic)",
                       "ln Total Loan Amount (Domestic)",
                       "HICP (Domestic)",
                       "Real Effective Exchange Rate (Domestic)",
                       "Unemployment Rate (Domestic)",
                       "Commodity Index (Domestic)",
                       "Deposit Rate (Domestic)",
                       "GDP (Domestic)",
                       "House Price Index (Domestic)",
                       "Homeownershipe Rate (Domestic)",
                       "GDP (Euro Area)",
                       "HICP (Euro Area)",
                       "EUR-USD Exchange Rate"
                       
                       ),
  omit.summary.stat = c("n"), # Rounds values to 2 decimal places
  out = paste0(LATEX, "08_EA_Descriptive_Statistics/euro_desciptive_stats.tex")
  )  

# 4. Market Concentration by Country ===========================================

## Year 2019 -------------------------------------------------------------------
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("giscoR")
library(giscoR)


df <- LOAD("39_thesis_mpt_eu_samplecreation_main")
df_2019 <- df |> 
  filter(year == 2019) |> 
  dplyr::select(country, hhi_ci_total_assets) |> 
  mutate(country = if_else(country == "GR", "EL", country))


# Download European country geometries from Natural Earth (medium scale)
# We filter for continent = "Europe" to get all European countries.
eu_countries <- gisco_get_countries(resolution = "10", year = 2020)

# Merge your data with the European spatial data using the ISO A3 code
merged_data <- left_join(eu_countries, df_2019, by = c("CNTR_ID" = "country"))

# Create the map plot with a yellow-to-red gradient for market concentration
p_2019 <- ggplot(merged_data) +
  geom_sf(aes(fill = hhi_ci_total_assets), color = "black", size = 0.2) +
  scale_fill_gradient(low = "yellow", high = "red", 
                      name = "Market\nConcentration (HHI)",
                      na.value = "grey90",
                      guide = guide_colorbar(
                        barwidth = unit(5, "cm"),
                        barheight = unit(0.5, "cm"),
                        title.position = "top",
                        title.hjust = 0.5,
                        breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
                        labels = c("0", "0.05", "0.1", "0.15", "0.2", "0.25"),
                        limits = c(0, 0.25)
                      )) +
  labs(title = "Market Concentration by Country (2019)",
       caption = "Source: ECB") +
  # Zoom in on Europe: adjust these limits as needed for the best view.
  coord_sf(xlim = c(-25, 45), ylim = c(34, 72), expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.caption = element_text(size = 8)
  )


ggsave(filename = "europe_market_concentration_2019.pdf", 
       plot = p_2019, 
       device = "pdf", 
       width = 10, 
       height = 8) 

## Year 2019 -------------------------------------------------------------------
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("giscoR")
library(giscoR)


df_2014 <- df |> 
  filter(year == 2014) |> 
  dplyr::select(country, hhi_ci_total_assets) |> 
  mutate(country = if_else(country == "GR", "EL", country))


# Download European country geometries from Natural Earth (medium scale)
# We filter for continent = "Europe" to get all European countries.
eu_countries <- gisco_get_countries(resolution = "10", year = 2020)

# Merge your data with the European spatial data using the ISO A3 code
merged_data <- left_join(eu_countries, df_2014, by = c("CNTR_ID" = "country"))

# Create the map plot with a yellow-to-red gradient for market concentration
p_2014 <- ggplot(merged_data) +
  geom_sf(aes(fill = hhi_ci_total_assets), color = "black", size = 0.2) +
  scale_fill_gradient(low = "yellow", high = "red", 
                      name = "Market\nConcentration (HHI)",
                      na.value = "grey90",
                      guide = guide_colorbar(
                        barwidth = unit(5, "cm"),
                        barheight = unit(0.5, "cm"),
                        title.position = "top",
                        title.hjust = 0.5,
                        breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4),
                        labels = c("0", "0.05", "0.1", "0.15", "0.2", "0.25", "0.3", "0.35", "0.4")
                      )) +
  labs(title = "Market Concentration by Country (2014)",
       caption = "Source: ECB") +
  # Zoom in on Europe: adjust these limits as needed for the best view.
  coord_sf(xlim = c(-25, 45), ylim = c(34, 72), expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.caption = element_text(size = 8)
  )


ggsave(filename = "europe_market_concentration_2014.pdf", 
       plot = p_2014, 
       device = "pdf", 
       width = 10, 
       height = 8) 


# 5. Plot Loan Outstanding Amount ==============================================

## 5.1 Outstanding Mortgage Amount ---------------------------------------------

# Load Data
outst <- LOAD("39_thesis_mpt_eu_samplecreation_main_m")

# Prepare Data: Outstanding Amount
df_outst <- outst |> 
  dplyr::select(country, year, hp_outst_amount_EUR) |> 
  rename(
    outst_total = hp_outst_amount_EUR
  ) |> 
  group_by(year) |> 
  summarize(
    outst_total = sum(outst_total) / 1000
  )

# Plot total outstanding mortgage amount for the whole Eurozone Area
mortgage_amount_eurozone <- ggplot(df_outst, aes(x = factor(year), y = outst_total)) +
  geom_col(width = .85, fill = "dodgerblue") +  # Use geom_col for bar plots where y is provided
  labs(
    title = "Outstanding Mortgage Amount in Eurozone",
    x = "Year",
    y = "Mortgage Amount in Mrd."
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, " Mrd."),
    breaks = seq(0, 60000, by = 10000)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    plot.title   = element_text(size = 12, face = "bold", hjust = ".5"),
    plot.caption = element_text(size = 10)
  )

# Save
ggsave(
  filename = paste0(FIGURE, "mortgage_amount_eurozone.pdf"),
  plot = mortgage_amount_eurozone, 
  device = "pdf", 
  width = 10, 
  height = 8
)

## 5.2 Outstanding Mortgage Amount & Lending Rate ------------------------------

# Lending Rate
df_rate <- outst |> 
  dplyr::select(country, year, hp_outst_amount_EUR, lending_rate_total) |> 
  group_by(year) |> 
  mutate(total_outst = sum(hp_outst_amount_EUR)) |> 
  ungroup() |> 
  mutate(weights = hp_outst_amount_EUR / total_outst) |> 
  group_by(year) |> 
  summarize(lending_rate = sum(weights * lending_rate_total))

df_final <- df_outst |> 
  left_join(df_rate, by = "year")



# Choose a scale factor to bring lending_rate roughly into the same range as outstanding.
# Here, lending_rate values are around 2.5 to 3.5, so multiplying by 50 gives values around 125-175.
scale_factor <- 10000

comby_plot <- ggplot(df_final, aes(x = factor(year))) +
  # Bar plot for Outstanding Mortgage Amount
  geom_col(aes(y = outst_total), fill = "dodgerblue", width = 0.8) +
  # Line plot (with points) for Lending Rate (scaled)
  geom_line(aes(y = lending_rate * scale_factor, group = 1), color = "red", size = 1.2) +
  geom_point(aes(y = lending_rate * scale_factor), color = "red", size = 2) +
  # Define primary and secondary y axes with manual breaks and labels
  scale_y_continuous(
    name = "Outstanding Mortgage Amount",
    breaks = seq(0, 60000, by = 10000),                  # Left axis break points
    labels = function(x) paste0(x, " bn."),          # Append " Mrd." to left axis labels
    sec.axis = sec_axis(
      trans = ~ . / scale_factor,                   # Reverse the scaling for the secondary axis
      name = "Lending Rate",
      breaks = seq(0, 6, by = 1),                # Right axis break points (in original % scale)
      labels = function(x) paste0(x, "%")            # Append "%" to right axis labels
    )
  ) +
  labs(
    x = "Year",
    title = "Outstanding Mortgage Amount & Lending Rate",
    caption = "Source: ECB"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y        = element_text(size = 8),
    axis.title.y.left  = element_text(size = 10, margin = margin(r = 10)),
    axis.title.y.right = element_text(size = 10, margin = margin(l = 10)),
    axis.title.x       = element_text(size = 10),
    plot.title         = element_text(face = "bold", size = 12, hjust = .5),
    plot.caption       = element_text(size = 8)
  )


ggsave(
  filename = paste0(FIGURE, "mortgage_amount_lending_rate_eurozone.pdf"),
  plot = comby_plot, 
  device = "pdf", 
  width = 10, 
  height = 8
)


# 6. Relationship between Lending Rate and Market Concentration ================

outst <- LOAD("39_thesis_mpt_eu_samplecreation_main_m")

df_lending_rate <- outst |> 
  dplyr::select(month, country, lending_rate_total, hhi_ci_total_assets) |> 
  filter(month %in% c(as.Date("2003-12-01"), as.Date("2013-12-01"), as.Date("2015-12-01"), as.Date("2018-12-01"),  as.Date("2020-12-01"), 
                      as.Date("2021-12-01"),as.Date("2022-12-01"), as.Date("2023-12-01")))

ggplot(df_lending_rate, aes(x = hhi_ci_total_assets, y = lending_rate_total, color = factor(month))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)






###################################### END #####################################       
