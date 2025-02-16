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

# 01. Display Market Concentration =============================================

## 01.1 Banking Structural Statistical Indicator ===============================

df_bsi <- LOAD(dfinput = "03_thesis_mpt_databasics_ecb_a")
df_worldscope <- LOAD(dfinput = "05_thesis_mpt_databasics_worldscope")

# Restrict to the years tha will be observed
df_bsi <- df_bsi |> 
  select(country, year, hhi_ci_total_assets) |> 
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
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center and bold title
        axis.title = element_text(size = 12, face = "bold"),  # Emphasize axis titles
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Move legend to the bottom
        legend.title = element_text(face = "bold"),  # Emphasize legend title
        plot.caption = element_text(size = 10, hjust = 0) 
      ) +
      scale_x_continuous(breaks = unique(df_bsi$year))
    
  name <- str_replace(names, "df_", "")
  
  ggsave(
    filename = paste0(FIGURE, paste0("graph_", name, ".pdf")),  
    plot = graph,                   
    device = "pdf",                   
    width = 10,                       
    height = 6,                       
    units = "in"                      
  )
}

## 01.2 Worldscope  ============================================================

df_worldscope <- LOAD(dfinput = "35_thesis_mpt_eu_varcreation_treatment") 

df_worldscope <- df_worldscope |> 
  select(country, quarter, hhi_assets, hhi_liabilities) |> 
  mutate(year = year(quarter)) |> 
  filter(!(country == "GR" & year < 2001)) |> # Filter for years with GR being part of the Eurozone
  filter(!(country == "SI" & year < 2007)) |> # Filter for year with SI being part of the Eurozone
  filter(!(country == "SK" & year < 2009)) # Filter for years with SK being part of the Eurozone
  

# Restrict sample by countries
df_worldscope_large_countries <- df_worldscope |> 
  filter(country %in% c("AT", "BE", "DE", "FR", "ES", "IT", "NL"))

df_worldscope_small_countries <- df_worldscope |> 
  filter(!(country %in%  c("AT", "BE", "DE", "FR", "ES", "IT", "NL")))

# Vector over the three different df
df_names <- c("df_worldscope", "df_worldscope_large_countries", "df_worldscope_small_countries")

# Generate three graphs
for (names in df_names) {
  
  data <- get(names)
  
  graph <- ggplot(data = data, aes(x = quarter, y = hhi_assets, color = country)) +
    geom_point(alpha = 0.7, size = 2) +  # Slight transparency and size adjustment
    geom_line(size = 0.6, na.rm = FALSE) + # Increase line width for better visibility
    labs(
      title = "Herfindahl-Hirschman Index (HHI) of Total Assets by Country",
      x = "Year",
      y = "HHI",
      color = "Country",
      caption = "Source: Worldscope"
    ) +
    theme_minimal() +  # Apply a clean and simple theme
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center and bold title
      axis.title = element_text(size = 12, face = "bold"),  # Emphasize axis titles
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",  # Move legend to the bottom
      legend.title = element_text(face = "bold"),  # Emphasize legend title
      plot.caption = element_text(size = 10, hjust = 0) 
    ) +
    scale_x_continuous(breaks = unique(data$quarter[grepl("-10-01", data$quarter)]))
  
  name <- str_replace(names, "df_", "")
  
  ggsave(
    filename = paste0(FIGURE, paste0("graph_", name, ".pdf")),  
    plot = graph,                   
    device = "pdf",                   
    width = 10,                       
    height = 6,                       
    units = "in"                      
  )
}
  
# 2. Monetary Shock ============================================================

df_monetary_shock <- LOAD("24_thesis_mpt_databasics_monetary_shock_eurozone")

# Plot Monetary Shock of Jarcinski & Karadi (2020) based on the 

ggplot(df_monetary_shock, aes(x = month, y = (MP_pm + CBI_pm))) +
  geom_point()


# 3. Summary of Staistics ======================================================


df_main <- LOAD("39_thesis_mpt_eu_samplecreation_main_m")

df_summary <- df_main |> 
  select(hp_outst_amount_EUR, lending_rate_total, lending_rate_1year, lending_rate_5year, MP_median, Altavilla_target, 
        hhi_ci_total_assets, log_cr, log_tl, log_dl, hicp, reer, ur, commodity_index)

stargazer(
  df_summary, 
  type = "text",
  covariate.labels = c("Total Lending Amount HP (in Mio)",
                       "Lending Rate (Total)",
                       "Lending Rate up to 1 year",
                       "Lending Rate - Over 1 and up to 5 years",
                       "MS - Jarocinsk/Karadi (2020)",
                       "MS - Altavilla et al. (2019",
                       "HHI (based on Total Assets)",
                       "log Credit and Reserves",
                       "log Total Assets and Liabilities",
                       "log Deposit and Liabilities",
                       "HICP",
                       "Real Effective Exchange Rate",
                       "Unemployment Rate",
                       "Commodity Index"
                       )
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






###################################### END #####################################       
