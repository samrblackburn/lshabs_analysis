basemap_targets <- list(
  ## Primary basemap ----------------------------------------------------------

  # Split into 3 to ensure we get the resolution we want
  tar_target(
    basemap_main,
    st_bbox(
      c(xmin = -92.3, ymin = 46.6, xmax = -90.5, ymax = 47.0),
      crs = 4326
    ) %>%
      get_basemap()
  ),
  tar_target(
    basemap_s,
    st_bbox(
      c(xmin = -92.3, ymin = 46.4, xmax = -90.5, ymax = 46.5),
      crs = 4326
    ) %>%
      get_basemap()
  ),
  tar_target(
    basemap_n,
    st_bbox(
      c(xmin = -92.3, ymin = 47.1, xmax = -90.5, ymax = 47.2),
      crs = 4326
    ) %>%
      get_basemap()
  ),

  ## Inset basemaps -------------------------------------------------------------
  # St. Louis Estuary
  tar_target(
    slre_basemap,
    st_bbox(
      c(xmin = -92.14, xmax = -91.94, ymin = 46.67, ymax = 46.81),
      crs = 4326
    ) %>%
      get_basemap()
  ),

  # Siskiwit/Mawike Bays
  tar_target(
    apostle_basemap,
    st_bbox(
      c(xmin = -91.22, xmax = -91.00, ymin = 46.80, ymax = 46.96),
      crs = 4326
    ) %>%
      get_basemap()
  ),

  # Western half of Lake Superior
  tar_target(
    big_basemap,
    st_bbox(
      c(xmin = -93, ymin = 46, xmax = -88, ymax = 49),
      crs = 4326
    ) %>%
      get_basemap()
  ),

  # Eastern half of Lake Superior
  tar_target(
    big_basemap_e,
    st_bbox(
      c(xmin = -88, ymin = 46, xmax = -84, ymax = 49),
      crs = 4326
    ) %>%
      get_basemap()
  )
)
