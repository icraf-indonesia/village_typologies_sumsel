# Load required libraries
library(tidyverse)
library(sf)
library(writexl)
library(openxlsx)

# Define functions
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Set up variables and parameters
location_name <- "SumSel"
output_dir <- "output"
k_number <- 8  # Parameter for number of clusters
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

# Read data
metadata <- read_csv("data_preprocessed/metadata.csv") %>%
  select(-any_of(c("PIC", "No"))) %>%
  drop_na(Data)

df_pre_pca <- read_csv("data_preprocessed/main_df_sumsel.csv") %>%
  mutate(iddesa = as.character(iddesa))

sumsel_map <- st_read("data/podes_2019_sumsel.shp") %>%
  select(iddesa) %>%
  inner_join(df_pre_pca, by = "iddesa") %>%
  filter(!st_is_empty(.))

# Dynamic k selection
k_col_name <- paste0("k_", k_number)
cluster_data <- st_read("output/cluster_map_SumSel.shp") %>%
  pull(!!sym(k_col_name)) %>%
  tibble(cluster = .)

# Update metadata with summarization method
metadata_updated <- metadata %>%
  mutate(
    summarization_method = case_when(
      variable %in% categorical_vars ~ "mode",
      variable %in% skewed_vars ~ "median",
      variable %in% mean_vars ~ "mean",
      TRUE ~ NA_character_
    )
  )

# Perform grouped summary
summary_df <- sumsel_map %>%
  bind_cols(cluster_data, .) %>%
  group_by(cluster) %>%
  summarise(
    across(all_of(categorical_vars), ~ mode(.x)),
    across(all_of(skewed_vars), ~ median(.x, na.rm = TRUE)),
    across(all_of(mean_vars), ~ mean(.x, na.rm = TRUE))
  )

# Transform summary dataframe to long format
summary_df_long <- summary_df %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  as_tibble() %>%
  filter(variable != "cluster") %>%
  rename_with(~ gsub("V", "k", .), starts_with("V")) %>%
  left_join(metadata_updated, by = "variable")

# Add categories
summary_df_long <- summary_df_long %>%
  mutate(
    Category = case_when(
      variable %in% c(
        "jarak_kanal", "jarak_gambut", "jarak_sawit", "jarak_karet",
        "jarak_hutan_tanaman", "jarak_jalan", "jarak_pabrik_pemrosesan",
        "jarak_konsesi_kebun", "jarak_hutan_alami", "jarak_sungai",
        "jarak_area_terbakar", "jarak_deforestasi", "jarak_rs_terdekat",
        "jarak_area_tambang", "persentase_feg_budidaya", "persentase_feg_lindung",
        "persentase_area_budidaya", "persentase_kebun", "persentase_hutan_alami",
        "persentase_semak_belukar", "persentase_badan_air", "luas_deforestasi",
        "persentase_lahan_garapan_potensial", "indeks_bahaya_banjir",
        "indeks_bahaya_longsor", "persentase_terlayani_irigasi",
        "indeks_kekeringan", "rerata_temperatur", "perubahan_temperatur",
        "rerata_hujan", "rerata_perubahan_hujan"
      ) ~ "Environment",
      variable %in% c(
        "jumlah_kk", "persentase_elektrifikasi", "jumlah_sma",
        "jumlah_faskes", "persentase_surat_miskin", "jumlah_lemkemdes",
        "jenis_sinyal_internet", "jenis_air_minum", "jenis_air_mandi_cuci",
        "ada_bakar_lahan", "ada_pencemaran", "kejadian_cemar_air",
        "kejadian_cemar_tanah", "kejadian_cemar_udara", "dampak_cemar",
        "kejadian_limbah_di_sungai", "kejadian_tanah_longsor",
        "kejadian_banjir", "kejadian_gempa", "kejadian_tsunami",
        "kejadian_gelombang_pasang", "kejadain_angin_puyuh",
        "kejadian_gunung_meletus", "kejadian_kebakaran_hutan",
        "kejadian_kekeringan_lahan", "kejadian_bencana_lain",
        "ada_sistem_prngtn_dini_bencana", "ada_sistem_prngtn_dini_tsunami",
        "ada_perlengkapan_keselamatan", "ada_jalur_evakuasi",
        "jumlah_populasi", "kepadatan_populasi", "derajat_kemiskinan",
        "pencaharian_berbasis_sda"
      ) ~ "Social",
      variable %in% c(
        "jenis_penghasilan_utama", "jenis_komoditi_unggulan",
        "jumlah_imk", "jumlah_bank", "jumlah_koperasi",
        "persentase_kk_pem_kumuh"
      ) ~ "Economic",
      TRUE ~ NA_character_
    )
  )

# Save output with dynamic filename
output_filename <- paste0("output/summary_table_sumsel_k", k_number, ".xlsx")
write_xlsx(summary_df_long, output_filename)

# Remove unnecessary columns and reshape data
summary_df_wide <- summary_df_long %>%
  select(variable, starts_with("k"), Category, summarization_method) %>%
  pivot_longer(
    cols = starts_with("k"),
    names_to = "Cluster",
    values_to = "Value"
  ) %>% 
  group_by(Cluster) %>% 
  pivot_wider(
    names_from = variable,
    values_from = Value
  )

# Create workbook and apply formatting
wb <- createWorkbook()
addWorksheet(wb, "Cluster Summary")
writeData(wb, "Cluster Summary", summary_df)

# Get numeric variables and apply conditional formatting
numeric_vars <- names(summary_df_wide)[!names(summary_df_wide) %in% c("Cluster", "Category", "summarization_method")]
for (col in numeric_vars) {
  col_index <- which(names(summary_df_wide) == col)
  conditionalFormatting(
    wb, sheet = "Cluster Summary",
    cols = col_index, rows = 2:(nrow(summary_df_wide) + 1),
    style = c("lightblue", "yellow", "salmon"),
    type = "colourScale"
  )
}

# Save workbook with dynamic filename
colored_output_filename <- paste0("output/summary_table_sumsel_colored_k", k_number, ".xlsx")
saveWorkbook(wb, file = colored_output_filename, overwrite = TRUE)