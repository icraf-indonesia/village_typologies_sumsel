# PODES Data Processing Script
# ---------------------------------------------------------
# This script processes and transforms the PODES 2021 dataset specifically for the Pasigala region.
# Dependencies: readr, readxl, dplyr
# Operations performed:
#   - Reading the pre-filtered dataset
#   - Calculating key metrics (e.g., total households, electrification percentage)
#   - Renaming and selecting relevant variables for subsequent analysis
# The results are stored in a processed dataframe for further use in analysis.
# ---------------------------------------------------------

# Load required libraries
library(readr)
library(readxl)
library(dplyr)

# Define file name for PODES 2021 dataset
file_name <- "PODES_2021_SUMSEL"

# Read the pre-selected and filtered PODES data
df_pd <- read_csv("data_preprocessed/PODES_2021_SUMSEL_with_id.csv")

# Read reference file
ref_file <- read_xlsx("data/reference_sumsel.xlsx", sheet = 2, skip = 1)

# Process the data by calculating and renaming relevant variables
df_processed <- df_pd %>%
  # Calculate total number of households
  mutate(jumlah_kk = R501A1 + R501A2 + R501B) %>%
  # Calculate electrification percentage
  mutate(persentase_elektrifikasi = ((R501A1 + R501A2) / jumlah_kk) * 100) %>%
  # Calculate total number of high schools
  mutate(jumlah_sma = R701HK2 + R701HK3 + R701IK2 + R701IK3 + R701JK2 + R701JK3 + R701NK2 + R701NK3) %>%
  # Calculate total number of healthcare facilities
  mutate(jumlah_faskes = R704CK2 + R704DK2 + R704EK2 + R704FK2 + R704GK2 + R704HK2 + R704IK2 + R704JK2 + R704KK2 + R705A) %>%
  # Get the distance to the nearest hospital
  mutate(jarak_rs_terdekat = R704AK2) %>%
  mutate(across(jarak_rs_terdekat, ~ replace(., is.na(.), 0))) %>%
  # Calculate percentage of poor households
  mutate(persentase_surat_miskin = R711 / jumlah_kk * 100) %>%
  # Calculate total number of community organizations
  mutate(jumlah_lemkemdes = R809A + R809B + R809C + R809D + R809E + R809F) %>%
  # Get the type of internet signal
  mutate(jenis_sinyal_internet = R1005D) %>%
  mutate(across(jenis_sinyal_internet, ~ replace(., is.na(.), 0))) %>%
  # Calculate total number of micro and small industries
  mutate(jumlah_imk = rowSums(across(
    c("R1201A", "R1201B", "R1201C", "R1201D", "R1201E", "R1201F", "R1201G", "R1201H", "R1201I", "R1201J",
      "R1201K", "R1201L", "R1201M", "R1201N", "R1201O", "R1201P")
  ))) %>%
  # Calculate total number of banks
  mutate(jumlah_bank = R1205A1 + R1205A2 + R1205A3) %>%
  # Calculate total number of cooperatives
  mutate(jumlah_koperasi = R1206A1 + R1206A2 + R1206A3 + R1206A4) %>%
  # Get the main source of income
  mutate(jenis_penghasilan_utama = R403A) %>%
  # Get the main commodity
  mutate(jenis_komoditi_unggulan = R403B) %>%
  mutate(across(jenis_komoditi_unggulan, ~ replace(., is.na(.), 0))) %>%
  # Calculate percentage of households with poor housing
  mutate(persentase_kk_pem_kumuh = R512B3 / jumlah_kk * 100) %>%
  mutate(across(persentase_kk_pem_kumuh, ~ replace(., is.na(.), 0))) %>%
  # Check for pollution presence
  mutate(ada_pencemaran = as.integer(if_any(c(R513AK2, R513BK2, R513CK2), ~ . == 1))) %>%
  # Get the main source of drinking water
  mutate(jenis_air_minum = R507A) %>%
  # Get the main source of water for bathing and washing
  mutate(jenis_air_mandi_cuci = R507B) %>%
  # Check for land burning activities
  mutate(ada_bakar_lahan = R516) %>%
  # Select relevant columns for analysis
  select(
    ID,
    jumlah_kk,
    persentase_elektrifikasi,
    jumlah_sma,
    jarak_rs_terdekat,
    jumlah_faskes,
    persentase_surat_miskin,
    jumlah_lemkemdes,
    jenis_sinyal_internet,
    jumlah_imk,
    jumlah_bank,
    jumlah_koperasi,
    jenis_penghasilan_utama,
    jenis_komoditi_unggulan,
    persentase_kk_pem_kumuh,
    jenis_air_minum,
    jenis_air_mandi_cuci,
    ada_bakar_lahan,
    ada_pencemaran
  ) %>%
  # Ensure ID is a character column
  mutate(ID = as.character(ID))

# Join the processed data with the reference data
df_preprocessed_joined <- ref_file %>%
  transmute(IDBPS = as.character(iddesa)) %>%
  left_join(df_processed, by = c("IDBPS" = "ID"))

# Export the joined data to a CSV file
df_preprocessed_joined %>%
  write_csv(paste0("data_preprocessed/", file_name, "_harmonised_BPS_synced.csv"))

# Visualize missing data
df_preprocessed_joined %>%
  visdat::vis_dat()
