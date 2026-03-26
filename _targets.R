library(targets)

read_packages <- c(
  "tidyverse",
  "sf",
  "maptiles",
  "patchwork",
  "khroma",
  "climaemet"
)

tar_option_set(packages = c(read_packages), tidy_eval = FALSE)

tar_source()

list(
  analysis_targets,

  #figures
  bacteria_targets,
  currents_targets
)
