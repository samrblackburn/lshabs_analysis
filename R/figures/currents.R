currents_targets <- list(
  ## Fetching Data ---------------------------------------------------------------------

  #' Download GLERL data for a single point
  #'
  #' Queries glerl webs service to download model data
  #' Just surface data, no temperature
  #' Goes month by month, as a bunch of small queries seem to work faster than one big one
  #' Files are saved into data/fvcom
  #'
  #' @param lat latitude in degrees
  #' @param lon longitude in degrees
  #' @param site_name string to label data with
  #' @param start_date string, earliest date to download
  #' @param end_date string, last date to download
  #'
  #' @returns Data frame of download status for all files
  tar_target(
    read_current,
    function(
      lat,
      lon,
      site_name,
      start_date = "2013-01-01",
      end_date = "2022-12-31"
    ) {
      print(str_glue(
        "Downloading data for site {site_name} from {start_date} to {end_date}"
      ))

      all_month_starts <- seq(
        as.Date(start_date),
        as.Date(end_date),
        by = "month"
      )
      month_starts <- all_month_starts[month(all_month_starts) %in% 5:10]
      month_ends <- ceiling_date(month_starts, "month") - days(1)

      base_url <- "http://apps.glerl.noaa.gov/erddap/griddap/LS_fvcom_temp.csvp"
      search_radius <- 0.01

      pull_month <- function(start_dt, end_dt) {
        out_dir <- str_glue("data/fvcom/{site_name}/{year(start_dt)}")
        dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

        file_name <- file.path(
          out_dir,
          str_glue("{site_name}_{format(start_dt, '%Y_%m')}.csv")
        )

        # Check if the file already exists; if so, skip the download
        if (file.exists(file_name)) {
          return(str_glue("Skipped (already exists): {start_dt}"))
        }

        start_str <- paste0(start_dt, "T00:00:00Z")
        end_str <- paste0(end_dt, "T00:00:00Z")

        filt_params <- str_glue(
          "%5B({start_str}):1:({end_str})%5D%5B(0.0):1:(0)%5D%5B({lat - search_radius}):1:({lat + search_radius})%5D%5B({lon - search_radius}):1:({lon + search_radius})%5D"
        )

        full_url <- str_glue(
          "{base_url}?Eastward_Water_Velocity{filt_params},Northward_Water_Velocity{filt_params}"
        )

        result_status <- tryCatch(
          {
            df <- read_csv(full_url, show_col_types = FALSE) %>%
              summarize(
                across(everything(), ~ mean(., na.rm = TRUE)),
                .by = `time (UTC)`
              ) %>%
              mutate(site = site_name)

            if (nrow(df) > 0) {
              write_csv(df, file_name, na = "")
              return(str_glue("Success: {start_dt}"))
            } else {
              return(str_glue("Empty: {start_dt}"))
            }
          },
          error = function(e) {
            return(str_glue("Failed: {start_dt} - {e$message}"))
          }
        )

        return(result_status)
      }

      results <- pmap(
        list(start_dt = month_starts, end_dt = month_ends),
        pull_month,
        .progress = TRUE
      )

      return(invisible(results))
    }
  ),

  # List of locations for nearshore monitoring sites to download glerl data for
  # Combining sites in similar areas (like Apostle Islands)
  tar_target(
    lake_sites_glerl,
    lake %>%
      mutate(
        site = str_replace(site, " OS", ""),
        site = str_replace(site, " NS", ""),
        site = str_replace(site, "CB10", "site 18 - 10044196"),
        site = str_replace(site, "CB1\\b", "site 19 - 10044195"),
        site = str_replace(site, "Mawikwe Bay", "site 15 - 10054863"),
        site = str_replace(site, "Siskiwit Bay", "site 14 - 10052513"),
        site = str_replace(site, "Bark Bay", "site 13 - 10052512")
      ) %>%
      summarise(
        across(c(chl, latitude, longitude), ~ median(., na.rm = T)),
        .by = c(date, site, source)
      ) %>%
      filter(year(date) > 2018) %>%
      filter(str_detect(site, "Outlet", negate = TRUE)) %>%
      filter(str_detect(site, "CB", negate = TRUE)) %>%
      filter(str_detect(site, "LLO", negate = TRUE)) %>%
      filter(!is.na(chl)) %>%
      group_by(site) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      filter(n > 8) %>%
      select(-c(n, source, date, chl)) %>%
      mutate(
        site = case_when(
          str_detect(site, "site") ~ str_split_i(site, " ", 2),
          .default = site
        )
      ) %>%
      summarize(across(everything(), mean), .by = site)
  ),

  # Download files and save
  # Actual target is data frame of download logs
  tar_target(
    glerl_downloads,
    pmap(
      list(
        lake_sites_glerl$latitude,
        lake_sites_glerl$longitude,
        lake_sites_glerl$site
      ),
      read_current
    )
  ),

  ## Creating Plot ----------------------------------------------------------------------
  #' Read saved GLERL data for a single location
  #'
  #' Wrapper function to read in all files in a site folder and bind together
  #'
  #' @param site_name site identifier, name of folder to read data from
  #'
  #' @returns Data frame of current data for a single site
  tar_target(read_fvcom, function(site_name) {
    list.files(
      str_glue("data/fvcom/{site_name}"),
      recursive = TRUE,
      full.names = TRUE
    ) %>%
      map(\(x) read_csv(x, show_col_types = FALSE)) %>%
      list_rbind()
  }),

  # Read in FVCOM data for sites of interest
  # Add additional columns for plotting
  # Rename sites
  # Filter to months of interest
  tar_target(
    fvcom_data,
    c("2", "6", "9", "14") %>%
      map(read_fvcom) %>%
      list_rbind() %>%
      select(
        site,
        date = `time (UTC)`,
        lat = `latitude (degrees_north)`,
        lon = `longitude (degrees_east)`,
        u = `Eastward_Water_Velocity (meters s-1)`,
        v = `Northward_Water_Velocity (meters s-1)`
      ) %>%
      mutate(
        water_spd = sqrt(u^2 + v^2),
        water_dir = (450 - atan2(v, u) * 180 / pi) %% 360, #compass degrees
        water_fr = if_else(water_dir >= 180, water_dir - 180, water_dir + 180),
        month = month(date),
        month_name = month(date, label = TRUE, abbr = FALSE),
        site = case_when(
          site == 2 ~ "Amnicon Mouth",
          site == 6 ~ "Bois Brule Mouth",
          site == 9 ~ "Flag Mouth",
          site == 14 ~ "Siskiwit Bay",
          .default = ""
        )
      ) %>%
      filter(month %in% c(6, 8, 10))
  ),

  tar_target(fvcom_plot, {
    # Create a temporary combined string for the facet argument
    combined_facet <- paste(
      fvcom_data$site,
      fvcom_data$month_name,
      sep = "___"
    )

    # Build the base windrose plot
    p <- ggwindrose(
      speed = fvcom_data$water_spd,
      direction = fvcom_data$water_fr,
      n_directions = 16,
      speed_cuts = seq(0, 0.46, length.out = 9),
      legend_title = str_wrap("Surface Water Speed (m/s)", 15),
      facet = combined_facet, # Pass the combined vector
      col_pal = "Mako"
    )

    # Split the facet column in the plot's internal data frame
    p$data <- p$data %>%
      tidyr::separate(facet, into = c("site", "month"), sep = "___")
    p$data$month <- factor(
      p$data$month,
      levels = c("June", "August", "October")
    )

    # Format plot labels
    p +
      # only label N, E, S, W
      scale_x_discrete(
        drop = FALSE, # Keeps empty directional bins from disappearing
        labels = c(
          "N",
          "",
          "",
          "",
          "E",
          "",
          "",
          "",
          "S",
          "",
          "",
          "",
          "W",
          "",
          "",
          ""
        )
      ) +
      scale_y_continuous(
        limits = c(0, 0.6),
        breaks = c(0.2, 0.4, 0.6),
        minor_breaks = NULL,
        labels = scales::percent
      ) +
      # Site as rows, month as columns
      facet_grid(site ~ month) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold"),
        panel.spacing = unit(1.5, "lines")
      )
  }),
  tar_target(
    fvcom_plot_file,
    ggsave(
      "figures/currents.png",
      fvcom_plot,
      width = 6.5,
      height = 10,
      dpi = 500
    ),
    format = "file"
  )
)
