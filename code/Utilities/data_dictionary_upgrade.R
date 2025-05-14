# This script takes the data dictionary from the Excel format, reads in each sheet,
# retains the sheet name as the variable "from", then creates a concatenated string
# of all forms or sheets a given variable is present in. Ultimately, it writes a new RDS
# file and should not have to be re-run, but may be useful for future releases of SPARK.

rm(list = ls())
library(tidyverse)
# Get the sheet names
sheet_names <- excel_sheets("./data/SPARK Data Dictionary.xlsx")
data_dictionary <- lapply(sheet_names, function(sheet_name) {
  df <- read_xlsx("./data/SPARK Data Dictionary.xlsx", sheet = sheet_name)
  df$from <- sheet_name  # Add a new column with the sheet name
  return(df)
}) %>% bind_rows()

# Identify duplicates and concatenate SheetNames for removed entries
distinct_info <- data_dictionary %>%
  group_by(variable) %>%
  mutate(row_number = row_number()) %>%
  ungroup() %>%
  arrange(variable, row_number) %>%
  group_by(variable) %>%
  mutate(
    is_first = row_number == 1,
    removed_sheet_names = ifelse(is_first, "", from)) %>%
  group_by(variable) %>%
  summarise(
    variable = first(variable),
    removed_sheet_names = paste(removed_sheet_names, collapse = ", ")
  ) 
# Bind rows, keeping only the first occurrence of each variable
distinct_data <- data_dictionary %>%
  distinct(variable, .keep_all = TRUE) 

# Join to include removed sheet names in the final data
data_dictionary <- distinct_data %>%
  left_join(distinct_info, by = "variable") %>%
  mutate(from = paste0(from, removed_sheet_names)) %>%
  rename(var_name = variable)

write_rds(data_dictionary, "./data/data_dictionary.rds")
