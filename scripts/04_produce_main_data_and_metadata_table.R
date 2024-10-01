# Load necessary libraries
library(tidyverse)
library(readxl)


# Metadata ----------------------------------------------------------------
# Read the metadata from the Excel file
meta_data <- read_xlsx("data/Analysis_typology_sumsel.xlsx", sheet = 1) %>%
  # Remove the "Pembagi" column if it exists
  select(-any_of("Pembagi")) %>% 
  # Select only the first 7 columns
  select(1:7)

# Clean the metadata
meta_data_clean <- meta_data %>% 
  # Drop rows where "Data" or "nama_indikator" columns have missing values
  drop_na(any_of(c("Data", "nama_indikator"))) %>%
  # Filter out rows with specific "No" values
  filter(!No %in% c("V87", "V88", "V89", "V90", "V91", "V92", "V93", "V94", "V95", "V96"))%>%
  rename(variable = nama_indikator)

# Save the cleaned metadata to a CSV file
write_csv(meta_data_clean, "data_preprocessed/metadata.csv")



# Main data ---------------------------------------------------------------

# Read the main data from the Excel file
main_df_raw <- read_xlsx("data/Analysis_typology_sumsel.xlsx", sheet = "Data")

# Clean the main data
main_df <- main_df_raw %>% 
  # Remove the first 10 columns
  select(-any_of(1:10)) %>% 
  # Remove the first row
  slice(-1) %>% 
  # Rename columns for clarity
  rename(
    nmkab = ...11,  # Rename column 11 to "nmkab"
    nmkec = ...12   # Rename column 12 to "nmkec"
  ) %>% 
  # Remove specific columns by their names
  select(-any_of(c(
    "...61", 
    "...62",
    "...63",
    "...64",
    "...65",
    "...66"
  ))) %>% 
  filter(!jumlah_kk %in% "NA") %>% 
  drop_na(jumlah_kk) %>%
  mutate_at(7:78, as.numeric)


# check NA presence
main_df %>%
  summarize(across(everything(), ~ sum(is.na(.)))) %>% t

vis_dat(main_df)


# Save the cleaned main data to a CSV file
write_csv(main_df, "data_preprocessed/main_df_sumsel.csv")
