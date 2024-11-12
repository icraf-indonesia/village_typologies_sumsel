library(ggplot2)
library(rlang) # For sym() and !! operator
library(ggrepel)
library(dplyr)

generate_pca_plot <- function(pca_model, pc_x, pc_y = NULL, plot_type = "contrib", df_lookup = NULL) {

  eigenvectors_df <- pca_model@model$eigenvectors %>%
    as.data.frame() %>%
    mutate(feature = row.names(.))
  
  
  lookup_ind <- df_lookup %>% select(feature = nama_variabel_input, group = `Kategori  Indikator`)
  eigenvectors_df <- eigenvectors_df %>%  left_join(lookup_ind, by="feature")

  if(plot_type == "contrib" && !is.null(pc_y)) {
    stop("For contribution plots, only one principal component should be specified (pc_y should be NULL).")
  }
  
  if(plot_type == "contrib") {
    plot <- eigenvectors_df %>%
      ggplot(aes(x = !!sym(pc_x), y = reorder(feature, !!sym(pc_x)))) +
      geom_point() +
      labs(x = pc_x, y = "Feature") + facet_wrap(vars(group))
  } else if(plot_type == "biplot" && !is.null(pc_y)) {
    plot <- eigenvectors_df %>%
      select(!!sym(pc_x), !!sym(pc_y), all_of(c("feature", "group"))) %>%
      filter(!(!!sym(pc_x) > -0.1 & !!sym(pc_x) < 0.1) | !(!!sym(pc_y) > -0.1 & !!sym(pc_y) < 0.1)) %>% 
      ggplot(aes(x = !!sym(pc_x), y = !!sym(pc_y), label = feature)) +
      geom_point(color = "darkred")  +
      geom_text_repel(point.padding = 0.5,  size = 4) +
      labs(x = pc_x, y = pc_y)+ facet_wrap(vars(group))+
      theme_bw() +
      theme(text = element_text(size = 25), # Sets a base size for all text elements
            plot.title = element_text(size = 20), # Specifically make the plot title larger
            axis.title = element_text(size = 18), # Specifically make axis titles larger
            axis.text = element_text(size = 16), # Specifically make axis text (tick labels) larger
            legend.title = element_text(size = 16), # Specifically make legend title larger
            legend.text = element_text(size = 14))+
      geom_rect(aes(xmin = -0.1, xmax = 0.1, ymin = -Inf, ymax = Inf), fill = "yellow", alpha = 0.01)+
      geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -0.1, ymax = 0.1), fill = "salmon", alpha = 0.01)
  } else {
    stop("For biplots, both pc_x and pc_y must be specified.")
  }
  
  return(plot)
}


plot_sf_clustering <- function(sf_object, cluster_column, plot_title = "") {
  # Ensure the input is an sf object
  if (!inherits(sf_object, "sf")) {
    stop("The input is not an sf object.")
  }
  
  # Check if the specified column exists in the sf object
  if (!cluster_column %in% names(sf_object)) {
    stop(paste("Column", cluster_column, "does not exist in the sf object."))
  }
  # Create the plot
  ggplot(data = sf_object) +
    geom_sf(aes(fill = factor(.data[[cluster_column]])), color = NA) + # Fill based on clustering
    grafify::scale_fill_grafify(palette = "kelly", discrete = TRUE, reverse = TRUE) + # Use a discrete color scale
    labs(title = plot_title, fill = cluster_column) + # Add title and legend title
    theme_minimal() + # Use a minimal theme
    theme(legend.position = "bottom") # Position the legend at the bottom
}


plot_sf_clustering_save <- function(sf_object, cluster_column, location) {
  # Ensure the input is an sf object
  if (!inherits(sf_object, "sf")) {
    stop("The input is not an sf object.")
  }
  
  # Check if the specified column exists in the sf object
  if (!cluster_column %in% names(sf_object)) {
    stop(paste("Column", cluster_column, "does not exist in the sf object."))
  }
  
  # Define the file path
  file_path <- paste0("output/", location,"_", cluster_column, ".png")
  
  # Create the plot
  p <- ggplot(data = sf_object) +
    geom_sf(aes(fill = factor(.data[[cluster_column]])), color = NA) + # Fill based on clustering
    grafify::scale_fill_grafify(palette = "kelly", discrete = TRUE) + # Use a discrete color scale
    labs(title = paste("Clustering with", gsub("\\D", "", cluster_column), "Groups"), fill = cluster_column) +    theme_minimal() + # Use a minimal theme
    theme(legend.position = "bottom")
  
  # Save the plot
  ggsave(file_path, plot = p, width = 10, height = 8, dpi = 300)
}


summarize_clusters <- function(raw_data, num_clusters, lookup_stat) {
  # Drop geometry and select relevant columns
  df_long <- raw_data %>% 
    select(-IDBPS, -starts_with("k_"), all_of(num_clusters)) %>%
    pivot_longer(cols = -all_of(num_clusters)) %>%
    left_join(lookup_stat, by = "name") %>% 
    rename(k = all_of(num_clusters))
  
  # Summarize the data
  summary_df <- df_long %>%
    group_by(k, name) %>%
    summarise(value = if (all(Unit == "kategorikal")) get_mode(value) else mean(value, na.rm = TRUE), .groups = 'drop') %>%
    drop_na() %>%
    pivot_wider(names_from = k, values_from = value)
  
  # Return the summarized tibble
  return(summary_df)
}

# Function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Function to create a heatmap for each group in a given column
create_heatmaps_by_group <- function(data, group_column) {
  # Split the data into a list of tibbles based on the grouping column
  data_groups <- data %>%
    group_by(!!sym(group_column)) %>%
    group_split()
  
  # Iterate over each group and create a heatmap
  lapply(data_groups, function(group_data) {
    # Prepare the matrix for the heatmap
    group_matrix <- group_data %>%
      select(-all_of(group_column)) %>%  # Remove the grouping column
      column_to_rownames(var = "name") %>%
      as.matrix()
    
    # Replace NA values with 0
    group_matrix[is.na(group_matrix)] <- 0
    
    # Generate the heatmap
    heatmap_name <- unique(group_data[[group_column]])
    message("Generating heatmap for group: ", heatmap_name)
    heatmaply::heatmaply(
      group_matrix,
      Rowv = FALSE,
      Colv = FALSE,
      scale = "row",
      colors = pals::coolwarm(12),
      cellnote = group_matrix,
      main = paste("Indikator ", heatmap_name)  # Add a title to each heatmap
    )
  })
}


library(ggplot2)
library(patchwork)
library(tidyr)

visualize_pca_results <- function(pca_result, n_components = 5) {
  # Extract eigenvalues
  eigen <- pca_result$sdev^2
  
  # If n_components is not specified, use all components
  if (is.null(n_components)) {
    n_components <- length(eigen)
  } else {
    # Ensure n_components doesn't exceed the available components
    n_components <- min(n_components, length(eigen))
  }
  
  # Create sequence for selected components
  selected_components <- seq_len(n_components)
  
  # Plot eigenvalues
  plot_eigen <- data.frame(
    PC = paste0("PC", seq_along(eigen)[selected_components]),
    Eigenvalue = eigen[selected_components]
  ) %>%
    ggplot(aes(x = reorder(PC, -Eigenvalue), y = Eigenvalue)) +
    geom_point() +
    geom_hline(yintercept = 1, color = "salmon") +
    xlab("Principal Components") +
    theme_bw()
  
  # Extract PVE and CVE
  importance <- summary(pca_result)$importance
  PVE <- importance["Proportion of Variance", ]
  CVE <- importance["Cumulative Proportion", ]
  
  # Plot PVE and CVE
  plot_pve_cve <- data.frame(
    PC = seq_along(PVE)[selected_components],
    PVE = PVE[selected_components],
    CVE = CVE[selected_components]
  ) %>%
    pivot_longer(cols = c(PVE, CVE), names_to = "metric", values_to = "variance_explained") %>%
    ggplot(aes(x = PC, y = variance_explained)) +
    geom_point() +
    theme_bw() +
    facet_wrap(~ metric, ncol = 1, scales = "free")
  
  # Plot Scree plot
  plot_scree <- data.frame(
    PC = seq_along(PVE)[selected_components],
    PVE = PVE[selected_components]
  ) %>%
    ggplot(aes(x = PC, y = PVE, group = 1, label = PC)) +
    geom_point() +
    geom_line() +
    geom_text(nudge_y = -.002) +
    theme_bw()
  
  # Combine plots
  plot_pca_evaluation_criterion_combined <- (plot_eigen / (plot_pve_cve + plot_scree))
  
  return(plot_pca_evaluation_criterion_combined)
}

create_leaflet_map <- function(clusters_kec, cluster_lookup, col_names = NULL) {
  # Ensure clusters_kec is read as an sf object
  if (!inherits(clusters_kec, "sf")) {
    clusters_kec <- st_read(clusters_kec, quiet = TRUE)
  }
  
  # Validate col_names if provided
  if (!is.null(col_names)) {
    # Check if col_names is a single string
    if (length(col_names) != 1) {
      stop("col_names must be a single character string")
    }
    
    # Check if the column exists in clusters_kec
    if (!col_names %in% names(clusters_kec)) {
      stop(sprintf("Column '%s' not found in clusters_kec dataset", col_names))
    }
    
    # Select the specified column if col_names is provided
    clusters_kec <- clusters_kec |> select(any_of(col_names)) %>% rename(cluster=1)
  }
  
  # Read the cluster lookup table
  if (is.character(cluster_lookup)) {
    cluster_lookup <- readr::read_csv(cluster_lookup)
  }
  
  # Join the data with cluster names
  clusters_kec_with_name <- clusters_kec |> 
    right_join(cluster_lookup, by = "cluster") |> 
    mutate_at(.vars = c("cluster", "name"), .funs = as.factor)
  
  # Set up the color palette
  pal <- colorFactor(palette = "Set1", domain = clusters_kec_with_name$name)

  # Constructing the label string
  clusters_kec_with_name$label_content <- with(
    clusters_kec_with_name,
    paste0(
      # "<strong>Kabupaten:</strong> ",
      # nmkab,
      # "<br>",
      # "<strong>Kecamatan:</strong> ",
      # nmkec,
      # "<br>",
      "<strong>Tipologi:</strong> ",
      name
    )
  ) |> lapply(htmltools::HTML)
  
  # Create the leaflet map with HTML-rendered labels
  leaflet_map <- leaflet(clusters_kec_with_name) |>
    addProviderTiles(providers$CyclOSM) |>
    addPolygons(
      fillColor = ~ pal(name),
      weight = 0.5,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~ label_content,
      labelOptions = labelOptions(noHide = FALSE, direction = 'auto')
    ) |>
    addLegend(pal = pal, values = ~ name, title = "Typology")
  
  return(leaflet_map)
}
