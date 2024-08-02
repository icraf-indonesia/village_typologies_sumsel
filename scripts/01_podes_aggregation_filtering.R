################################################################################
# PODES 2021 Data Aggregation and Filtering Script
# 
# This script performs the following operations:
# 
# 1. Retrieves and reads all .dbf files containing the PODES 2021 survey data from
#    the specified directory, combining them into a single tibble for easier manipulation.
# 2. Filters the combined dataset for specific regions of interest, namely Donggala,
#    Kota Palu, and Sigi, based on the region name variable (R102N).
# 3. Saves the filtered dataset as an RDS file for efficient storage and future analysis.
#
# The output is a single RDS file that contains the combined and filtered PODES data 
# for the specified regions, ready for further analysis.
#
# Dependencies: readr, dplyr, foreign, readxl
# Output: data/PODES TAHUN 2021/PODES_2021_PASIGALA.rds
################################################################################

# Load necessary libraries for data reading and manipulation
library(readr)
library(dplyr)
nama_prov <-
  c("Sumatera Selatan")

file_name <-
  "PODES_2021_SUMSEL"

# Retrieve the list of .dbf files from the directory where PODES 2021 data is stored
# These files contain the actual data collected during the PODES survey
podes_dbf_files <- list.files(path = "data/PODES TAHUN 2021/", pattern = "\\.dbf$", full.names = TRUE)

# Read all .dbf files and combine them into a single tibble (data frame) for easier manipulation
# This step involves reading each .dbf file as a data frame, converting it to a tibble, and then combining all tibbles
podes_df <- podes_dbf_files |>
  lapply(foreign::read.dbf) |> # Read each file using the foreign package's read.dbf function
  lapply(as_tibble)


podes_df_data <- podes_df |> 
  # remove columns related to administrative division ids
  lapply(function(df) select(df, -c("R101", "R102", "R103", "R104","R101N","R102N", "R103N", "R104N", "R105"))) |> 
  bind_cols()

# Infrastructure and micro, small, and medium enterprises (IMKM) variables
vname_village_id <- c("R101", "R102",  "R103", "R104","R101N", "R102N", "R103N", "R104N")

podes_df <- podes_df[[1]] |> 
  select(all_of(vname_village_id)) |> 
  bind_cols(podes_df_data)


# Filter the combined dataset to include only rows relevant to specific regions: Donggala, Kota Palu, and Sigi
# R102N is assumed to be the variable in the dataset that contains Kabupaten and Kota names
# The resulting filtered dataset is then saved to an RDS file for future use or analysis
podes_df |>
   filter(R101N %in% nama_prov) %>% 
 # filter(R102N %in% nama_kab) |>
  #write_csv("data/PODES TAHUN 2021/PODES_2021_PASIGALA.csv") # If you want to save as CSV uncomment this line
  saveRDS(paste0("data/PODES TAHUN 2021/",file_name,".rds")) # Save the filtered data as an RDS file

# Note: The commented-out write_csv line can be used instead of saveRDS if a CSV file is preferred.