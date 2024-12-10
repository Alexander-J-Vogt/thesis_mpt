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

# 01. Descriptive Statistics ===================================================

df_main <- LOAD(dfinput = "39_thesis_mpt_eu_samplecreation_main")

DISPLAYNA(dfx = df_main)

df_stats <- df_main |> 
  select(-c("country", "month", "year", "quarter", "share_top5_largest_ci_total_asset"))

stargazer(df_stats, 
          type = "text",        # Output type: "text", "html", or "latex"
          title = "Descriptive Statistics", 
          summary = TRUE,       # Enables summary statistics
          digits = 2,
          covariate.labels = c("Housing Loan - Amount (M)",
                               "Housing Loan - Interest Rate (M)",
                               "HHI - Assets  (Q)",
                               "HHI - Liabilities - (Q)",
                               "HHI - Assets (A) ",
                               "HHI - Total Credit (A)",
                               "Capital and Reserves (M)",
                               "Deposit and Liabilities (M)",
                               "Total Assets / Liabilities (M)",
                               "HICP (M)",
                               "Effective Exchange Rates (M)",
                               "Unemployment Rate (M)",
                               "Commodity Index (M)"
                               ),
          omit.summary.stat = c("n"), # Rounds values to 2 decimal places
          out = paste0(LATEX, "euro_desciptive_stats.tex"))      


# 02. Missings

missing_summary <- df_main %>%
  summarise(across(-c(1:4), ~ sum(is.na(.)), .names = "missing_{col}")) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing") %>%
  mutate(
    Variable = gsub("missing_", "", Variable),
    Percentage = round(Missing / nrow(df_main) * 100, 2)
  ) |> 
  select(-Missing)

library(knitr)
library(kableExtra)

missings_tex <- missing_summary %>%
  kable(
    caption = "Summary of Missing Values",
    col.names = c("Variable",  "Missing in %"),
    format = "latex",    # Output format
    booktabs = TRUE      # Adds LaTeX booktabs for cleaner tables
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),  # Keeps table in its place
    position = "center",                 # Centers the table
    full_width = FALSE                   # Prevents table from spanning the full page
  )

save_kable(missings_tex, file = "eurozone_missings.tex")


###############################################################################+
################################# ENDE ########################################+
###############################################################################+