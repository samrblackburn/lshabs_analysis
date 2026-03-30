library(targets)

read_packages <- c(
  "tidyverse",
  "sf",
  "maptiles",
  "ggspatial",
  "patchwork",
  "khroma",
  "ggnewscale",
  "ggrepel",
  "ggpmisc",
  "climaemet"
)

tar_option_set(packages = c(read_packages), tidy_eval = FALSE)

tar_source()

list(
  analysis_targets,

  #figures
  basemap_targets,
  bloom_count_targets,
  bloom_map_targets,
  chl_map_targets,
  chl_box_targets,
  temperature_targets,
  tp_map_targets,
  nutrient_targets,
  bacteria_targets,
  currents_targets
)
