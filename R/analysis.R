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
      col_types = cols(
        ton = col_double(),
        don = col_double(),
        cnr = col_double()
      )
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
  ),

  # Median values across all sites in lake, est, trib
  tar_target(
    lake_med_all,
    lake_filt %>%
      select(-c(date, site, latitude, longitude)) %>%
      summarise(
        across(everything(), ~ median(., na.rm = T))
      )
  ),

  tar_target(
    est_med_all,
    est_filt %>%
      select(-c(date, site, latitude, longitude)) %>%
      summarise(
        across(everything(), ~ median(., na.rm = T))
      )
  ),

  tar_target(
    trib_med_all,
    trib_filt %>%
      select(-c(date, site, latitude, longitude)) %>%
      summarise(
        across(everything(), ~ median(., na.rm = T))
      )
  ),

  ## Number of observations of each variable at each site ------------------------------
  tar_target(
    lake_var_counts,
    lake_filt %>%
      select(-c(date, chl_field, do_sat, npr, cnr, cpr, pnpr, pcnr, pcpr)) %>%
      pivot_longer(-site) %>%
      filter(!is.na(value)) %>%
      summarize(n = n(), .by = c(site, name)) %>%
      pivot_wider(values_from = n)
  ),

  tar_target(
    est_var_counts,
    est_filt %>%
      select(-c(date, chl_field, do_sat, npr, cnr, cpr, pnpr, pcnr, pcpr)) %>%
      pivot_longer(-site) %>%
      filter(!is.na(value)) %>%
      summarize(n = n(), .by = c(site, name)) %>%
      pivot_wider(values_from = n)
  ),

  tar_target(
    trib_var_counts,
    trib_filt %>%
      select(-c(date, do_sat, npr, cnr, cpr, pnpr, pcnr, pcpr)) %>%
      pivot_longer(-site) %>%
      filter(!is.na(value)) %>%
      summarize(n = n(), .by = c(site, name)) %>%
      pivot_wider(values_from = n)
  ),

  ## Create tables of correlation coefficients (r), slopes, p-values between variables in lake, est, trib
  # Uses repeated measures correlation for r and linear mixed models for slope.
  # Site is a random effect, so variable intercept but constant slope across sites
  # To check assumptions for a pair of variables, you can use the "performance package"
  # Example:
  # chl_mod <- lmer(chl ~ tp + (1 | site), data = lake_filt)
  # performance::check_model(chl_mod)

  # Function to generate table
  tar_target(cor_table, function(df, vars, site_col = "site") {
    # lmer: unscaled slope + p-value
    # return NA row if no overlap in samples between variables
    safe_lmer <- possibly(
      function(f_str, d) {
        mod <- lmerTest::lmer(as.formula(f_str), data = d)
        broom.mixed::tidy(mod, effects = "fixed") %>%
          filter(term != "(Intercept)") %>%
          slice(1) %>% # guard against multi-level factor predictors
          select(estimate, p.value)
      },
      otherwise = tibble(estimate = NA, p.value = NA)
    )

    # rmcorr: repeated measures correlation coefficient + p-value
    # return NA row if no overlap in samples between variables
    safe_rmcorr <- possibly(
      function(v1, v2, d) {
        rmc <- rmcorr::rmcorr(
          participant = d[[site_col]],
          measure1 = d[[v1]],
          measure2 = d[[v2]],
          dataset = d
        )
        tibble(r_value = unname(rmc$r), r_p_value = unname(rmc$p))
      },
      otherwise = tibble(r_value = NA, r_p_value = NA)
    )

    # create table of measures
    tibble(var1 = factor(vars, levels = vars)) %>%
      expand_grid(var2 = factor(vars, levels = vars)) %>%
      # remove duplicate pairings
      filter(as.integer(var1) < as.integer(var2)) %>%
      mutate(var1 = as.character(var1), var2 = as.character(var2)) %>%
      # create formula for lmer for each combination
      mutate(
        lmer_formula = map2_chr(
          var1,
          var2,
          ~ paste0("`", .x, "` ~ `", .y, "` + (1 | ", site_col, ")")
        ),

        # Run models
        stats_lmer = map(lmer_formula, ~ safe_lmer(.x, d = df)),
        stats_rmcorr = map2(var1, var2, ~ safe_rmcorr(.x, .y, d = df))
      ) %>%

      # Clean up the list-columns before unnesting to prevent name collisions
      mutate(
        stats_lmer = map(
          stats_lmer,
          ~ rename(.x, slope = estimate, slope_p_value = p.value)
        )
      ) %>%

      # make final table
      unnest(c(stats_lmer, stats_rmcorr)) %>%
      select(
        variable_y = var1,
        variable_x = var2,
        r_value,
        r_p_value,
        slope,
        slope_p_value
      )
  }),

  # make tables for each dataset
  # warnings are suppressed because rmcorr doesn't handle variable names well
  tar_target(
    lake_cor_table,
    suppressWarnings(
      cor_table(
        lake_filt,
        c(
          "chl",
          "temp",
          "do_sat",
          "cond",
          "ph",
          "turb",
          "tss",
          "tp",
          "tdp",
          "pp",
          "po4",
          "tn",
          "tdn",
          "no3",
          "nh3",
          "toc",
          "doc",
          "poc",
          "si"
        )
      )
    )
  ),
  tar_target(
    est_cor_table,
    suppressWarnings(
      cor_table(
        est_filt,
        c(
          "chl",
          "temp",
          "do_sat",
          "cond",
          "ph",
          "turb",
          "tss",
          "tp",
          "po4",
          "tn",
          "no3",
          "nh3"
        )
      )
    )
  ),
  tar_target(
    trib_cor_table,
    suppressWarnings(
      cor_table(
        trib_filt,
        c(
          "chl",
          "temp",
          "do_sat",
          "cond",
          "ph",
          "turb",
          "tss",
          "tp",
          "tdp",
          "pp",
          "po4",
          "tn",
          "tdn",
          "no3",
          "nh3",
          "toc",
          "doc",
          "poc",
          "si"
        )
      )
    )
  ),

  ## Plot correlation coefficients between variables for each dataset

  # Function to create plots
  tar_target(
    plot_cor_matrix,
    function(
      results,
      vars = target_vars,
      alpha = 0.05,
      title = "Repeated Measures Correlation",
      val_col = "r_value",
      p_col = "r_p_value",
      fill_lab = "r"
    ) {
      # Polished labels for plot
      var_labels <- c(
        chl = "Chl-a",
        temp = "Temp.",
        do_sat = "DO(%)",
        cond = "SpCond.",
        ph = "pH",
        turb = "Turb.",
        tss = "TSS",
        tp = "TP",
        tdp = "TDP",
        pp = "PP",
        po4 = "PO4",
        tn = "TN",
        tdn = "TDN",
        no3 = "NO3",
        nh3 = "NH3",
        toc = "TOC",
        doc = "DOC",
        poc = "POC",
        si = "Si"
      )
      # Ordered polished labels, matching the order of `vars`
      label_order <- unname(var_labels[vars])

      plot_df <- results %>%
        mutate(
          value = .data[[val_col]],
          p_value = .data[[p_col]],
          significant = !is.na(p_value) & p_value < alpha,
          label = ifelse(significant, sprintf("%.2f", value), NA),
          var_y = factor(unname(var_labels[variable_y]), levels = label_order),
          var_x = factor(
            unname(var_labels[variable_x]),
            levels = rev(label_order)
          )
        )

      ggplot(plot_df, aes(x = var_x, y = var_y)) +
        geom_tile(aes(fill = value), color = "grey85", linewidth = 0.3) +
        geom_tile(
          data = filter(plot_df, significant),
          fill = NA,
          color = "black",
          linewidth = 0.9
        ) +
        geom_text(aes(label = label), size = 3.2, color = "black") +
        scale_fill_BuRd(
          name = fill_lab,
          limits = c(-1, 1),
          midpoint = 0,
          reverse = TRUE
        ) +
        coord_equal() +
        labs(title = title, x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "inside",
          legend.position.inside = c(0.8, 0.8)
        )
    }
  ),

  # plots
  tar_target(
    lake_cor_plot,
    ggsave(
      "figures/lake_cor_plot.png",
      plot_cor_matrix(
        lake_cor_table,
        vars = c(
          "chl",
          "temp",
          "do_sat",
          "cond",
          "ph",
          "turb",
          "tss",
          "tp",
          "tdp",
          "pp",
          "po4",
          "tn",
          "tdn",
          "no3",
          "nh3",
          "toc",
          "doc",
          "poc",
          "si"
        ),
        title = "Lake Variable Correlation"
      ),
      dpi = 500
    )
  ),
  tar_target(
    est_cor_plot,
    ggsave(
      "figures/est_cor_plot.png",
      plot_cor_matrix(
        est_cor_table,
        vars = c(
          "chl",
          "temp",
          "do_sat",
          "cond",
          "ph",
          "turb",
          "tss",
          "tp",
          "po4",
          "tn",
          "no3",
          "nh3"
        ),
        title = "Estuary Variable Correlation"
      ),
      dpi = 500
    )
  ),
  tar_target(
    trib_cor_plot,
    ggsave(
      "figures/trib_cor_plot.png",
      plot_cor_matrix(
        trib_cor_table,
        vars = c(
          "chl",
          "temp",
          "do_sat",
          "cond",
          "ph",
          "turb",
          "tss",
          "tp",
          "tdp",
          "pp",
          "po4",
          "tn",
          "tdn",
          "no3",
          "nh3",
          "toc",
          "doc",
          "poc",
          "si"
        ),
        title = "Tributary Variable Correlation"
      ),
      dpi = 500
    )
  ),

  ## Function to plot chl-a against a different variable ---------------------------
  # Adds linear regression R2 and P to plot
  tar_target(
    plot_chl_relationship,
    function(
      df,
      x_var,
      x_label,
      annual = FALSE,
      legend_pos = c(0.8, 0.92),
      label_pos = c(0.95, 0.85)
    ) {
      chl_text <- if_else(
        annual,
        "Annual Mean Chlorophyll-a (µg/L)",
        "Chlorophyll-a (µg/L)"
      )
      ggplot(df, aes(x = {{ x_var }}, y = chl)) +
        geom_point(alpha = 0.7) +
        geom_smooth(
          aes(color = "linear regression"),
          method = "lm",
          se = FALSE
        ) +
        stat_poly_eq(
          aes(
            label = paste(
              after_stat(rr.label),
              after_stat(p.value.label),
              sep = "*\", \"*"
            )
          ),
          formula = y ~ x,
          parse = TRUE,
          label.x = label_pos[1],
          label.y = label_pos[2],
          size = 3
        ) +
        scale_color_manual(
          name = NULL,
          values = c("linear regression" = "#4477aa")
        ) +
        labs(x = x_label, y = chl_text) +
        theme_bw(base_size = 12) +
        theme(
          legend.position = "inside",
          legend.position.inside = legend_pos,
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
        )
    }
  ),

  ## Compare relationships between variables in Lake Superior

  ## Additional summary breakdowns --------------------------------------------------
  # Nearshore/offshore comparison from BRICO study
  tar_target(
    brico,
    brico <- lake %>%
      filter(source == "BRICO") %>%
      mutate(
        type = case_when(
          str_detect(site, "\\da") ~ "nearshore",
          str_detect(site, "\\dc") ~ "offshore",
          .default = NA
        )
      ) %>%
      filter(!is.na(type))
  ),
  tar_target(
    brico_med,
    brico %>%
      select(-c(date, latitude, longitude, source)) %>%
      summarise(
        across(everything(), ~ median(., na.rm = T)),
        .by = c(site, type)
      )
  ),
  tar_target(
    brico_type_med,
    brico %>%
      select(-c(date, latitude, longitude, site, source)) %>%
      summarise(
        across(everything(), ~ median(., na.rm = T)),
        .by = type
      )
  ),
  tar_target(
    brico_tp_comp,
    t.test(
      #tar_read(brico_tp_comp)
      brico_med %>%
        filter(type == "nearshore") %>%
        pull(tp),
      brico_med %>%
        filter(type == "offshore") %>%
        pull(tp)
    )
  ),
  tar_target(
    brico_no3_comp,
    t.test(
      #tar_read(brico_no3_comp)
      brico_med %>%
        filter(type == "nearshore") %>%
        pull(no3),
      brico_med %>%
        filter(type == "offshore") %>%
        pull(no3)
    )
  ),
  # E and W WI Coast medians comparison
  tar_target(
    lake_med_reg,
    lake_med %>%
      mutate(
        lon = st_coordinates(.)[, 1],
        region = case_when(
          str_detect(site, "CB") ~ "CB",
          lon < -91.5 ~ "W",
          lon > -91.5 ~ "E",
          .default = NA
        )
      ) %>%
      select(-c(site, lon)) %>%
      st_drop_geometry() %>%
      summarise(
        across(everything(), ~ median(., na.rm = T)),
        .by = region
      )
  ),
  # Faxon creek comparison to other tributaries
  tar_target(
    faxon_comp,
    trib %>%
      filter(source %in% c("UMD", "NPS")) %>% # sites with comparable sampling schedules
      filter(year(date) %in% c(2021, 2022)) %>% # only sampled 2021-2022
      mutate(faxon = site == "Faxon Creek") %>%
      select(-c(date, site, source, huc, latitude, longitude)) %>%
      summarise(across(everything(), ~ median(., na.rm = T)), .by = faxon)
  )
)
