# Load Required Libraries ----------------------------------------------------
library(tidyverse)
library(sf)
library(ClustGeo)
library(factoextra)
library(e1071)
library(caret)
library(corrplot)
library(geosphere)
source("R/utilities.R")

# Setup -----------------------------------------------------------------------
location_name <- "SumSel"
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)

# Read and Prepare Main Data -------------------------------------------------
df_pre_pca <- read_csv("data_preprocessed/main_df_sumsel.csv") %>%
  mutate(iddesa = as.character(iddesa))

attribute_columns <- c("nmkab", "nmkec", "nmdesa", "iddesa", "id_raster", "luas_desa")

# Read and Process Map Data --------------------------------------------------
sumsel_map <- st_read("data/podes_2019_sumsel.shp") %>%
  select(iddesa) %>%
  inner_join(df_pre_pca %>% select(iddesa)) %>%
  filter(!st_is_empty(.))

# Remove Near-Zero Variance Columns -------------------------------------------
near_zero_vars <- nearZeroVar(df_pre_pca, saveMetrics = TRUE)
df_pre_pca <- df_pre_pca[, !near_zero_vars$nzv]

# Identify and Transform Skewed Variables -------------------------------------
# Define skewness threshold
skewness_threshold <- 1

# Calculate skewness for selected columns
skewness_vals <- df_pre_pca %>%
  select(6:63) %>%
  summarise(across(everything(), skewness)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "skewness")

# Select variables with absolute skewness above the threshold
skewed_vars <- skewness_vals %>%
  filter(abs(skewness) > skewness_threshold) %>%
  pull(variable)

# Apply log10(x + 1) transformation to skewed variables
df_pre_pca <- df_pre_pca %>%
  mutate(across(all_of(skewed_vars), ~ log10(.x + 1)))

# Examine Multicollinearity ---------------------------------------------------
cor_matrix <- cor(df_pre_pca %>% select(7:ncol(df_pre_pca)), use = "complete.obs")

# Plot Correlation Matrix
cor_plot_path <- file.path(output_dir, "correlation_matrix.png")
png(filename = cor_plot_path, width = 800, height = 800, res = 150)
corrplot(cor_matrix, method = "circle")
dev.off()

# Principal Component Analysis (PCA) -------------------------------------------
# Prepare data for PCA by removing attribute columns
df_pca_input <- df_pre_pca %>%
  select(-all_of(attribute_columns))

# Perform PCA with scaling
pca_result <- prcomp(df_pca_input, scale. = TRUE)

write_rds(pca_result, "output/pca_result_sumsel.rds")

# Plot PCA Evaluation
pca_plot_path <- file.path(output_dir, paste0("plot_pca_eval_", location_name, ".png"))
png(filename = pca_plot_path, width = 8, height = 4, units = "in", res = 300)
visualize_pca_results(pca_result, n_components = 20)
dev.off()

# Feature Space Dissimilarity -------------------------------------------------
# Determine the number of principal components with eigenvalues > 1
num_pcs_above_one <- sum(pca_result$sdev^2 > 1)
eigen_pcs <- pca_result$x[, 1:num_pcs_above_one]

# Calculate Euclidean distance in feature space
D0 <- dist(eigen_pcs, method = "euclidean")

# Spatial Dissimilarity --------------------------------------------------------
# Calculate centroids of spatial features
coordinates <- sumsel_map %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(longitude = X, latitude = Y)

# Calculate geodesic distance matrix
D1 <- geosphere::distm(coordinates) %>%
  as.dist()

# Choose Alpha for Clustering -------------------------------------------------
range_alpha <- seq(0, 1, 0.1)
K <- 11

choice_alpha_result <- choicealpha(D0, D1, range_alpha, K, graph = FALSE)

# Plot Alpha Selection
alpha_plot_path <- file.path(output_dir, paste0("plot_choice_alpha_", location_name, ".png"))
png(filename = alpha_plot_path, width = 5, height = 5, units = "in", res = 300)
plot(choice_alpha_result, norm = TRUE)
dev.off()

# Hierarchical Clustering with Spatial Constraints -----------------------------
# Perform hierarchical clustering with specified alpha
tree <- hclustgeo(D0, D1, alpha = 0.3)

# Cut tree to create clusters (k = 2 to 20)
cluster_k <- cutree(tree, k = 2:20) %>%
  as.data.frame() %>%
  as_tibble() %>%
  rename_with(~ ifelse(str_detect(.x, "^[0-9]+$"), paste0("k_", .x), .x))

# Combine Clusters with Spatial Data -------------------------------------------
sumsel_cluster_results <- cluster_k %>%
  bind_cols(sumsel_map, .) %>%
  st_make_valid() %>%
  st_zm(drop = TRUE)

# Write Clustered Map to Shapefile --------------------------------------------
shapefile_path <- file.path(output_dir, paste0("cluster_map_", location_name, ".shp"))

st_write(
  sumsel_cluster_results,
  shapefile_path,
  append = FALSE,
  driver = "ESRI Shapefile"
)

# Hierarchical Clustering Evaluation Plot --------------------------------------

# Generate Evaluation Plots
plot_elbow <- fviz_nbclust(df_pca_input, FUN = hcut, method = "wss", k.max = 12) +
  ggtitle("(A) Elbow Method")

plot_silhouette <- fviz_nbclust(df_pca_input, FUN = hcut, method = "silhouette", k.max = 12) +
  ggtitle("(B) Silhouette Method")

plot_gap_stat <- fviz_nbclust(df_pca_input, FUN = hcut, method = "gap_stat", k.max = 12) +
  ggtitle("(C) Gap Statistic")

hclust_plot_path <- file.path(output_dir, paste0("plot_hclust_eval_", location_name, ".png"))
# Combine and Display Plots
combined_plot <- plot_elbow + plot_silhouette + plot_gap_stat
png(filename = hclust_plot_path, width = 9, height = 3, units = "in", res = 300)
combined_plot
dev.off()
