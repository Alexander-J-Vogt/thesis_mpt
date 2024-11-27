# TARGET: Download Call Reports from FFIEC & Perform Basic Data Cleaning
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

library(httr)
library(jsonlite)
library(rvest)
library(pxweb)
library(RSelenium)
library(netstat)

## 1. Scrape list of all banks ----------------------------------------------------

# Link of swedish data on monetary financial institutions
url <- "https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__FM__FM0401__FM0401X/MFIM1/"
page <- read_html(url)

# Scrape list of bank values
bank_data <- page |>
  html_nodes("#ctl00_ContentPlaceHolderMain_VariableSelector1_VariableSelector1_VariableSelectorValueSelectRepeater_ctl02_VariableValueSelect_VariableValueSelect_ValuesListBox option") |>
  html_attrs() |>
  lapply(function(x) c(name = x["value"])) |>
  bind_rows() |>
  as.data.frame()

# Scrape list of bank names
bank_names <- page |> 
  html_nodes("#ctl00_ContentPlaceHolderMain_VariableSelector1_VariableSelector1_VariableSelectorValueSelectRepeater_ctl02_VariableValueSelect_VariableValueSelect_ValuesListBox option") |>
  html_text() |> 
  as.data.frame()

# Create combined list
bank_data <- cbind(bank_data, bank_names)
colnames(bank_data) <- c("bank_value","bank_name")

# Clean bank names
replacements <- c("ä" = "ae", "ü" = "ue", "ö" = "oe", "å" = "aa")
bank_data <-  bank_data |>
  mutate(institution = bank_name) |> 
  mutate(bank_name = str_replace_all(bank_name, " ", "_"),
         bank_name = str_to_lower(bank_name),
         bank_name = str_replace_all(bank_name, ",", ""), 
         bank_name = str_replace_all(bank_name, "/", ""),
         bank_name = str_replace_all(bank_name, replacements)) |> 
  mutate(bank_name = str_replace_all(bank_name, "[0-9.]", ""),
         bank_name = str_replace(bank_name, "_$", ""),
         bank_name = str_replace(bank_name, "^_", ""),
         bank_name = str_replace_all(bank_name, "[()]", "")
         )


# 02. Scrape list of months ----------------------------------------------------
options <- page |> 
  html_nodes("#ctl00_ContentPlaceHolderMain_VariableSelector1_VariableSelector1_VariableSelectorValueSelectRepeater_ctl03_VariableValueSelect_VariableValueSelect_ValuesSelectPanel") |> 
  html_text()

# Isolate all months 
months <- page |> 
  html_elements("option") |> 
  html_text() |> 
  as.data.frame()

months <- months |> 
  filter(str_sub(months[[1]],start = -3, end = -3) == "M")

colnames(months) <- "months"

months_vector <- c(months$months)

# 03. WIP on getting all variables avaialble -----------------------------------
response <- POST(
  url = url,
  body = list(
    dropdown_name = "Account record"
  ),
  encode = "form"
)

html_content <- content(response, as = "text")
parsed_page <- html_content |> 
  read_html() |> 
  html_nodes(".values") |> 
  html_text(trim = TRUE)


# 04. Scraping Bank Data via API -----------------------------------------------

url_api <- "https://api.scb.se/OV0104/v1/doris/en/ssd/START/FM/FM0401/FM0401X/MFIM1"

# Create temp directory
dir.create(paste0(A, "e_sweden/", "temp"))

# Loop over all banks and load all datasets
lapply(seq_along(bank_data$bank_value), function(x) {

# Create for each bank a own query and loop over it 
pxweb_query_list <- list(
  "Institut" =  c(bank_data$bank_value[x]),
  "Kontopost" = c("K10100",
                  "K10200",
                  "K10300",
                  "K10400",
                  "K10500",
                  "K10600",
                  "K10700",
                  "K10800",
                  "K10900",
                  "K11000",
                  "K11100",
                  "K11200",
                  "K11300",
                  "K11400",
                  "K11500",
                  "K11600",
                  "K11700",
                  "K11800",
                  "K11900",
                  "K12000",
                  "K12100",
                  "K12200",
                  "K12300",
                  "K12400",
                  "K12500",
                  "K12600",
                  "K12700",
                  "K12800",
                  "K12900",
                  "K13000",
                  "K13100",
                  "K13200",
                  "K13300",
                  "K13400",
                  "K13500",
                  "K13600",
                  "K13700",
                  "K13800",
                  "K13900",
                  "K14000",
                  "K14100",
                  "K14200",
                  "K14300",
                  "K14400",
                  "K14500",
                  "K14600",
                  "K14700",
                  "K20100",
                  "K20400",
                  "K20500",
                  "K20600",
                  "K20700",
                  "K20800",
                  "K20900",
                  "K21000",
                  "K21100",
                  "K21200",
                  "K21300",
                  "K21400",
                  "K21500",
                  "K21600",
                  "K21700",
                  "K21800",
                  "K21900",
                  "K22000",
                  "K22100",
                  "K22200",
                  "K22300",
                  "K22400",
                  "K22500",
                  "K22600",
                  "K22700",
                  "K22800",
                  "K22900",
                  "K23000",
                  "K23100",
                  "K23200",
                  "K23300",
                  "K23400",
                  "K23500",
                  "K23600",
                  "K23700",
                  "K23800",
                  "K23900",
                  "K24000",
                  "K24100",
                  "K24200",
                  "K24210",
                  "K24220",
                  "K24221",
                  "K24700",
                  "K24800",
                  "K24810",
                  "K24820",
                  "K24900",
                  "K25000",
                  "K15100",
                  "K15110",
                  "K15200",
                  "K15210",
                  "K25100",
                  "K25110",
                  "K25200",
                  "K25210",
                  "K15300",
                  "K15400",
                  "K25300",
                  "K25400",
                  "K25500",
                  "K16100",
                  "K16110",
                  "K16120",
                  "K16200",
                  "K16210",
                  "K16220",
                  "K16300",
                  "K16310",
                  "K16320",
                  "K30100",
                  "K30200",
                  "K30300",
                  "K17000"),
  "ContentsCode" = c("FM0401XX"),
  "Valuta" = c("v0", "v1","v2"),
  "Tid" = months_vector
)

# Create query list
pxq <- pxweb_query(pxweb_query_list)

# Retrieve Data 
pxd <- pxweb_get(url_api,
                 pxq)

# Assign dynamic data name
assign(paste0("data_", x), as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))

# Save dataset
SAVE(dfx = get(paste0("data_", x)),namex = paste0("swedish_data_", bank_data$bank_name[x]), pattdir = paste0(A, "e_sweden/temp/"))

})

# 05. Creating Dataset on Swedish Banks ----------------------------------------

# List all files from API 
bank_files <- list.files(paste0(A, "e_sweden/temp/"))
bank_names <- bank_data |> 
  filter(!str_detect(bank_value, "^S")) |> 
  filter(!str_detect(bank_value, "^s"))

# Only 
bank_list <- bank_files |> 
  as.data.frame() |> 
  filter(str_sub(bank_files,start = 14, end = -5) %in% bank_names$bank_name)

# Get banking list as vector
bank_list <- bank_list$bank_files

# Initiate Swedish Banking List
swedish_banks <- list()

# Loop through each file, load it, and extract the data frame
for (file in bank_list) {

  data <- LOAD(dfinput = str_sub(file, end = -5), pattdir = paste0(A, "e_sweden/temp/"))
  
  # Assuming each .rda file has only one data frame
  swedish_banks[[file]] <- data  # Store the data frame in the list
  
  # Clean up the environment to avoid conflicts with other loaded files
  rm(data)
  
  # Print
  print(paste0("Finished: ", file))
}

# Giving each df the same column names
names <- c(
  "institution", 
  "item", 
  "currency", 
  "month", 
  "value_in_mill"
)

# Rename columns in each data frame
swedish_banks <- lapply(swedish_banks, function(df) {
  setNames(df, names[seq_len(ncol(df))])
})


# Create full datasets
swedish_banks <- bind_rows(swedish_banks)

# Clean Item Variable
df_swedish_banks <- swedish_banks |> 
  mutate(item_id = paste0("ID_", str_extract(item, "^[^ ]+")),
         item_label = str_remove(item, "^[^ ]+\\s") |> 
           str_trim() |> 
           str_replace_all(" ", "_") |> 
           str_to_lower() |> 
           str_replace_all(",", "") |> 
           str_replace_all("\\.", "")
         ) |> 
  mutate(currency = currency |> 
           str_replace_all(" ", "_") |> 
           str_to_lower()) |> 
  mutate(time = paste0(str_sub(month, start = 1, end = 4),
                        "-",
                        str_sub(month, start = 6, end = 7),
                        "-01")) 

df_swedish_banks <- df_swedish_banks |> 
  left_join(bank_data, by = c("institution")) |> 
  select(institution, bank_name, bank_value, time, item_id, item_label, currency, value_in_mill)
  

# Save raw dataset
SAVE(dfx = df_swedish_banks, namex = "df_swedish_banks_raw", pattdir = paste0(A, "e_sweden/"))



df_swedish_banks <- df_swedish_banks |> 
  select(bank_name, time, item_label, currency, value_in_mill) |> 
  filter(currency == "sek") |> 
  select(-currency)

df_swedish_banks <- df_swedish_banks |> 
  pivot_wider(names_from = item_label, values_from = value_in_mill)

test <- df_swedish_banks |> 
  group_by(bank_name) |> 
  summarize(all_present_values = all(!is.na("lending_swe_housing_credit_institutions"))) |> 
  filter(all_present_values == TRUE) |> 
  count()





