analysis_targets <- list(
  ## Helper Functions ------------------------------------------------------------

  #' Fetch basemap tiles for SF object
  #'
  #' Wrapper for maptiles::get_tiles to download cartodb's positron basemap
  #' Also converts to XY wrapper and harmonizes column names
  #'
  #' @param sf_obj SF object to use extent from
  #'
  #' @returns Data frame raster of basemap
  tar_target(get_basemap, function(sf_obj) {
    sf_obj %>%
      get_tiles(provider = "CartoDB.PositronNoLabels", forceDownload = TRUE) %>%
      as.data.frame(xy = TRUE) %>%
      mutate(
        color = if ("red" %in% colnames(.)) {
          rgb(red, green, blue, maxColorValue = 255)
        } else {
          rgb(lyr.1, lyr.2, lyr.3, maxColorValue = 255)
        }
      )
  }),

  #' Generate lag expressions
  #'
  #' Generate code string to pass into dplyr::mutate
  #' This generates columns of lagged summed data from the input column
  #'
  #' @param col name of column to generate lag sums
  #' @param n_groups number of new columns to generate
  #' (each column adds 1 block, so n_groups = 3 would give you 3 columns,
  #' with the sum of <block_size> prior rows, the sum of <block_size>x2 prior rows, and the sum of <block_size>x3 prior rows)
  #' @param block_size how many prior rows should be summed together
  #' @param lag_prefix string to attach to each column name
  #'
  #' @returns expression to use in dplyr::mutate
  tar_target(
    generate_lag_exprs,
    function(col, n_groups, block_size = 7, lag_prefix) {
      lag_sum_expr <- function(start, end) {
        lags <- map(start:end, ~ expr(lag(!!sym(col), !!.x)))
        Reduce(function(a, b) expr((!!a) + (!!b)), lags)
      }
      expr_list <- list()
      for (i in seq_len(n_groups)) {
        block_start <- (i - 1) * block_size + 1
        block_end <- i * block_size

        current_lag_expr <- lag_sum_expr(block_start, block_end)

        col_name <- paste0(lag_prefix, i)

        if (i == 1) {
          expr_list[[col_name]] <- current_lag_expr
        } else {
          prev_col <- expr(!!sym(paste0(lag_prefix, i - 1)))
          expr_list[[col_name]] <- expr((!!prev_col) + (!!current_lag_expr))
        }
      }
      expr_list
    }
  ),

  ## Synthesized Data ------------------------------------------------------------

  # File targets
  tar_target(bloom_file, "data/ls_blooms.csv", format = "file"),
  tar_target(lake_file, "data/lake_core.csv", format = "file"),
  tar_target(est_file, "data/estuary_core.csv", format = "file"),
  tar_target(trib_file, "data/tributary_core.csv", format = "file"),
  tar_target(cb_names_file, "data/cb_names.csv", format = "file"),
  tar_target(nbdc_file, "data/nbdc_daily.csv", format = "file"),

  # Read in files
  tar_target(blooms, read_csv(bloom_file)),
  tar_target(
    lake,
    read_csv(
      lake_file,
      col_types = cols(don = col_double(), cnr = col_double())
    )
  ),
  tar_target(est, read_csv(est_file)),
  tar_target(
    trib,
    read_csv(
      trib_file,
      col_types = cols(
        huc = col_character(),
        don = col_double(),
        cnr = col_double()
      )
    )
  ),
  tar_target(cb_names, read_csv(cb_names_file)),
  tar_target(
    nbdc,
    read_csv(
      nbdc_file,
      col_types = cols(
        apd_s = col_double(),
        vis_mi = col_double(),
        tide_ft = col_double(),
        lwrad_wm2 = col_double()
      )
    )
  ),

  ## Filter/clean synthesized data -------------------------------------------------

  # Remove inland/river blooms
  # Split verification status into reported/confirmed/verified
  # Add location names for figures
  # Drop unnecessary columns
  tar_target(
    bloom_filt,
    blooms %>%
      filter(str_detect(Location, "Inland", negate = TRUE)) %>%
      filter(
        !(str_detect(Location, "riverine") & str_detect(Region, "Cornucopia"))
      ) %>%
      filter(
        !(str_detect(Location, "riverine") & str_detect(Region, "North"))
      ) %>%
      mutate(
        year = year(Date),
        confirmed = !str_detect(`Verification status`, "unconfirmed"),
        verified = !str_detect(`Verification status`, "Suspect"),
        Location = if_else(
          Location ==
            "Lake Superior shoreline/nearshore environment; Lake Superior riverine and/or estuarine environment",
          "Lake Superior riverine and/or estuarine environment",
          Location
        ),
        location = case_when(
          str_detect(Location, "riverine") ~ "St. Louis Estuary",
          Lat > 48 ~ "Thunder Bay",
          .default = "Western Arm"
        ),
        region = str_remove(Region, "Lake Superior South Shore Western Arm; ")
      ) %>%
      st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
      select(
        date = Date,
        year,
        location,
        region,
        size = `Spatial extent`,
        confirmed,
        verified,
        water_condition = `Surface water conditions`,
        weather_condition = `Weather conditions`
      ) %>%
      arrange(date)
  ),

  # Trim lake data down for plotting/analysis
  # Harmonize site names for similar locations
  # Get mean values if location was sampled multiple times on same day
  # Drop data prior to 2019 (limited coverage)
  # Drop additional sites due to insufficient temporal coverage, parameter coverage, or
  # incompatible sampling location
  # Drops sites with fewer than 10 samples across 2019-2025
  tar_target(
    lake_filt,
    lake %>%
      mutate(
        site = str_replace(site, " OS", ""),
        site = str_replace(site, " NS", ""),
        site = str_replace(site, "site 19 - 10044195", "CB1"),
        site = str_replace(site, "site 18 - 10044196", "CB10"),
        site = str_replace(site, "site 15 - 10054863", "Mawikwe Bay"),
        site = str_replace(site, "site 14 - 10052513", "Siskiwit Bay"),
        site = str_replace(site, "site 13 - 10052512", "Bark Bay")
      ) %>%
      summarise(
        across(everything(), ~ median(., na.rm = T)),
        .by = c(date, site, source)
      ) %>%
      filter(year(date) > 2018) %>%
      filter(str_detect(site, "MNPCA", negate = TRUE)) %>% # Several north shore sites, only sampled in 2022
      filter(str_detect(site, "21BRBCH", negate = TRUE)) %>% # several sites, no chemistry data available
      filter(str_detect(site, "WQX", negate = TRUE)) %>% # drops several tribal sites that are not comparable - too close to river mouths, also drops dup DNR site 101
      filter(str_detect(site, "Outlet", negate = TRUE)) %>% # not comparable - too close to river mouth (1 site)
      filter(str_detect(site, "USGS", negate = TRUE)) %>% # not comparable - too close to river mouth (1 site)
      group_by(site) %>%
      mutate(n = n()) %>%
      filter(n > 10) %>%
      select(-c(n, source)) %>%
      ungroup()
  ),

  # Trim estuary data down for plotting/analysis
  # Keep only sites with consistent temporal coverage (swmp site)
  # Drop rows with only WQ sensor data
  tar_target(
    est_filt,
    est %>%
      filter(site %in% c("lksba", "lksol", "lksbl", "lkspo")) %>%
      filter(
        !if_all(
          -c(
            date,
            latitude,
            longitude,
            site,
            source,
            chl_field,
            turb,
            cond,
            ph,
            temp,
            do,
            do_sat
          ),
          is.na
        )
      ) %>%
      select(-source)
  ),

  # Trim down tributary data for analysis/plotting
  # Inconsistent
  tar_target(
    trib_filt,
    trib %>%
      filter(year(date) >= 2014) %>%
      filter(month(date) %in% c(5, 6, 7, 8, 9)) %>%
      filter(latitude < 47.2) %>%
      filter(latitude > 46.5) %>%
      filter(str_detect(source, "WIDNR", negate = TRUE)) %>%
      filter(str_detect(source, "MNPCA", negate = TRUE)) %>%
      filter(str_detect(source, "USGS", negate = TRUE)) %>%
      filter(site != "Siskiwit Lake") %>%
      left_join(cb_names, by = join_by(site)) %>%
      filter(str_detect(name, "Unnamed", negate = TRUE) | is.na(name)) %>%
      filter(str_detect(name, "Little", negate = TRUE) | is.na(name)) %>%
      filter(str_detect(name, "Pine", negate = TRUE) | is.na(name)) %>%
      filter(str_detect(name, "Ino", negate = TRUE) | is.na(name)) %>%
      mutate(
        site = if_else(source == "NCBC", str_split_i(name, " at", 1), site),
        site = case_when(
          site == "BADRIVER_WQX-BadUS2" ~ "Bad River",
          site == "BADRIVER_WQX-KakSI" ~ "Kakagon River",
          site == "REDCLIFF_WQX-SR02" ~ "Sand River",
          site == "REDCLIFF_WQX-RR01" ~ "Raspberry River",
          site == "REDCLIFF_WQX-FC01" ~ "Frog Creek",
          site == "REDCLIFF_WQX-RCC03" ~ "Red Cliff Creek",
          site == "REDCLIFF_WQX-CC03" ~ "Chicago Creek",
          site == "North Fish" ~ "North Fish Creek",
          site == "Thompsons Creek" ~ "Thompson Creek",
          site == "Poplar Creek" ~ "Poplar River",
          site == "Brule River" ~ "Bois Brule River",
          .default = site
        )
      ) %>%
      filter(str_detect(site, "WQX", negate = TRUE)) %>%
      group_by(site) %>%
      mutate(n = n()) %>%
      filter(n > 15 | (site == "Iron River" & n > 1)) %>%
      select(-c(n, source, huc, name)) %>%
      ungroup()
  ),

  ## Get median values --------------------------------------------------------------
  tar_target(
    lake_med,
    lake_filt %>%
      select(-date) %>%
      summarise(
        across(everything(), ~ median(., na.rm = T)),
        .by = site
      ) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  ),

  tar_target(
    est_med,
    est_filt %>%
      select(-date) %>%
      summarise(
        across(everything(), ~ median(., na.rm = T)),
        .by = site
      ) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  ),

  tar_target(
    trib_med,
    trib_filt %>%
      select(-date) %>%
      summarise(
        across(everything(), ~ median(., na.rm = T)),
        .by = site
      ) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  )
)
