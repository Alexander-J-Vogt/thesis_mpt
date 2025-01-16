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
  
