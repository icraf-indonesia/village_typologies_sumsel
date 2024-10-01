# Load Required Libraries ----------------------------------------------------
library(tidyverse)
library(sf)
library(writexl)

# Define Functions -----------------------------------------------------------
# Mode function for calculating the mode of a vector
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Set up Variables and Parameters ---------------------------------------------
location_name <- "SumSel"
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)

# Lists of variables based on summarization method
categorical_vars <- c(
  "jenis_sinyal_internet", "jenis_penghasilan_utama", "jenis_komoditi_unggulan",
  "jenis_air_minum", "jenis_air_mandi_cuci", "ada_bakar_lahan", "ada_pencemaran",
  "kejadian_cemar_air", "kejadian_cemar_tanah", "kejadian_cemar_udara",
  "dampak_cemar", "kejadian_limbah_di_sungai", "kejadian_tanah_longsor",
  "kejadian_banjir", "kejadian_gempa", "kejadian_tsunami",
  "kejadian_gelombang_pasang", "kejadain_angin_puyuh", "kejadian_gunung_meletus",
  "kejadian_kebakaran_hutan", "kejadian_kekeringan_lahan", "kejadian_bencana_lain",
  "ada_sistem_prngtn_dini_bencana", "ada_sistem_prngtn_dini_tsunami",
  "ada_perlengkapan_keselamatan", "ada_jalur_evakuasi"
)

skewed_vars <- c(
  "jarak_kanal", "jarak_gambut", "jarak_sawit", "jarak_karet",
  "jarak_hutan_tanaman", "jarak_jalan", "jarak_pabrik_pemrosesan",
  "jarak_konsesi_kebun", "jarak_hutan_alami", "jarak_sungai",
  "jarak_area_terbakar", "jarak_deforestasi", "jarak_rs_terdekat",
  "jarak_area_tambang"
)

mean_vars <- c(
  "persentase_feg_budidaya", "persentase_feg_lindung",
  "persentase_area_budidaya", "persentase_kebun", "persentase_hutan_alami",
  "persentase_semak_belukar", "persentase_badan_air", "luas_deforestasi",
  "persentase_lahan_garapan_potensial", "indeks_bahaya_banjir",
  "indeks_bahaya_longsor", "persentase_terlayani_irigasi", "indeks_kekeringan",
  "jumlah_kk", "persentase_elektrifikasi", "jumlah_sma", "jumlah_faskes",
  "persentase_surat_miskin", "jumlah_lemkemdes", "jumlah_imk", "jumlah_bank",
  "jumlah_koperasi", "persentase_kk_pem_kumuh", "rerata_temperatur",
  "perubahan_temperatur", "rerata_hujan", "rerata_perubahan_hujan",
  "jumlah_populasi", "kepadatan_populasi", "derajat_kemiskinan",
  "pencaharian_berbasis_sda"
)

# Read Data -------------------------------------------------------------------
# Read metadata
metadata <- read_csv("data_preprocessed/metadata.csv") %>%
  select(-any_of(c("PIC", "No"))) %>%
  drop_na(Data) 

# Read main data
df_pre_pca <- read_csv("data_preprocessed/main_df_sumsel.csv") %>%
  mutate(iddesa = as.character(iddesa))

# Read and process map data
sumsel_map <- st_read("data/podes_2019_sumsel.shp") %>%
  select(iddesa) %>%
  inner_join(df_pre_pca, by = "iddesa") %>%
  filter(!st_is_empty(.))

# Read cluster shapefile
k_8 <- st_read("output/cluster_map_SumSel.shp") %>%
  pull(k_8) %>%
  tibble(cluster = .)

# Update Metadata with Summarization Method -----------------------------------
metadata_updated <- metadata %>%
  mutate(
    summarization_method = case_when(
      variable %in% categorical_vars ~ "mode",
      variable %in% skewed_vars ~ "median",
      variable %in% mean_vars ~ "mean",
      TRUE ~ NA_character_  # For variables not in the lists
    )
  )

# Save updated metadata
write_csv(metadata_updated, "data_preprocessed/metadata.csv")

# Perform Grouped Summary -----------------------------------------------------
summary_df <- sumsel_map %>%
  bind_cols(k_8, .) %>%
  group_by(cluster) %>%
  summarise(
    across(all_of(categorical_vars), ~ Mode(.x)),
    across(all_of(skewed_vars), ~ median(.x, na.rm = TRUE)),
    across(all_of(mean_vars), ~ mean(.x, na.rm = TRUE))
  )

# Transform Summary Dataframe to Long Format -----------------------------------
summary_df_long <- summary_df %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  as_tibble() %>%
  filter(variable != "cluster") %>%
  rename_with(~ gsub("V", "k", .), starts_with("V")) %>%
  left_join(metadata_updated, by = "variable")

# Save Output ------------------------------------------------------------------
write_xlsx(summary_df_long, "output/summary_table_sumsel.xlsx")
