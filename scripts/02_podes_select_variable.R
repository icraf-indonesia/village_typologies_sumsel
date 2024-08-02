# -------------------------------------------------------------------------------------
# PODES Database Variable Selection
# -------------------------------------------------------------------------------------
# Purpose: To select relevant variables from the PODES dataset for further analysis.
# Please execute PODES 2021 Data Aggregation and Filtering Script first before running this script.
# Dependencies: readr, dplyr, purrr, readxl
# Input: Layout_PODES 2021_Desa_final.xlsx
# Input: "data/PODES TAHUN 2021/PODES_2021_SUMSEL.rds"
# Output: ""data_preprocessed/PODES_2021_SUMSEL_with_id.csv""
# -------------------------------------------------------------------------------------

# Load required libraries
library(readr)
library(readxl)
library(dplyr)
library(purrr)

# Define the file name for the PODES 2021 dataset
file_name <- "PODES_2021_SUMSEL"

# Define the file path to the layout PODES Excel file
file_path <- "data/PODES TAHUN 2021/Layout dan Kuesioner PODES 2021-DESA/Layout_PODES 2021_Desa_1133_SUMSEL_DI_HA.xlsx"

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets(file_path)

# Read all sheets into a list of data frames and combine them into a single data frame
indicator_list <- lapply(sheet_names, function(sheet) {
  read_xlsx(file_path, sheet = sheet)
}) %>%
  bind_rows() %>%
  filter(!is.na(Criteria))

# Write the combined indicator list to a CSV file
indicator_list %>%
  write_csv(paste0("data_preprocessed/layout_", file_name, ".csv"))

# Combine all variable codes into a single vector
podes_vnames <- indicator_list %>%
  pull("Kode Variabel") %>%
  as.vector() %>%
  sort()

# Read the PODES 2021 dataset
podes_2021 <- readRDS(paste0("data/PODES TAHUN 2021/", file_name, ".rds"))

# Create the output directory if it doesn't exist
if (!dir.exists("data_preprocessed")) {
  dir.create("data_preprocessed")
}

# Select relevant variables and add an ID column, then write to a CSV file
podes_2021 %>%
  select(all_of(podes_vnames)) %>%
  mutate(ID = paste0(R101, R102, R103, R104), .before = R101) %>%
  write_csv(paste0("data_preprocessed/", file_name, "_with_id.csv"))
