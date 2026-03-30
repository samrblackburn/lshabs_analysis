temperature_targets <- list(
  ## Daily average water temperature ----------------------------------------
  # Lake - average of LLO nearshore and offshore
  tar_target(
    nbdc_temp,
    nbdc %>%
      filter(site %in% c("45027", "45028")) %>%
      pivot_wider(
        id_cols = c(date),
        names_from = site,
        values_from = wtemp_c
      ) %>%
      mutate(temp = rowMeans(select(., `45027`, `45028`), na.rm = TRUE)) %>%
      select(date, temp)
  ),
  # Estuary - LSNERR sites
  tar_target(
    est_temp,
    est %>%
      filter(source == "LSNERR") %>%
      filter(!is.na(temp)) %>%
      select(date, site, temp) %>%
      summarize(temp = mean(temp), .by = c(date, site))
  ),

  ## Maximum Daily Mean Temperatures -----------------------------------------
  tar_target(
    lake_year_max,
    nbdc_temp %>%
      mutate(year = year(date)) %>%
      select(year, temp) %>%
      summarize(temp = max(temp, na.rm = TRUE), .by = year)
  ),

  ## Line plot of degree days by year ----------------------------------------
  tar_target(
    dd_line_plot,
    nbdc_temp %>%
      mutate(
        year = year(date),
        temp = if_else(is.na(temp) | temp < 10, 0, temp)
      ) %>%
      group_by(year) %>%
      mutate(dd = cumsum(temp)) %>%
      ungroup() %>%
      mutate(date = yday(date), year = factor(year)) %>%
      mutate(
        bloom = case_when(
          year %in% c("2011", "2013", "2014", "2015") ~ "No Blooms",
          .default = "Bloom Occurred"
        ),
        linecolor = case_when(
          year %in% c("2012", "2018") ~
            "red",
          .default = "black"
        )
      ) %>%
      mutate(dd = if_else(year == 2020, NA, dd)) %>%
      filter(date > 140 & date < 280) %>%
      ggplot(aes(date, dd, group = year)) +
      geom_line(
        aes(linetype = bloom, color = linecolor),
        linewidth = 1,
        alpha = 0.6
      ) +
      scale_x_continuous(
        name = NULL,
        breaks = c(152, 182, 213, 244, 274),
        labels = c("Jun 1", "July 1", "Aug 1", "Sep 1", "Oct 1")
      ) +
      scale_linetype_manual(
        name = NULL,
        values = c("solid", "dotted")
      ) +
      scale_color_manual(values = c("black", "red")) +
      guides(color = "none") +
      ylab("Degree Days (ºC)") +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.8, 0.1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
      )
  ),

  ## Boxplot of daily water temperatures in each year -------------------------
  tar_target(
    dd_box_plot,
    nbdc_temp %>%
      mutate(year = year(date), year = as_factor(year)) %>%
      filter(month(date) %in% c(6, 7, 8, 9)) %>%
      mutate(temp = if_else(year == 2020, NA, temp)) %>%
      ggplot(aes(year, temp)) +
      geom_boxplot(outliers = FALSE) +
      geom_jitter(width = 0.2, height = 0, alpha = 0.1) +
      ylab("Daily Mean Water Temperature (ºC)") +
      xlab(NULL) +
      theme_bw(base_size = 12)
  ),

  ## Degree Days/water temp vs. blooms --------------------------------------------------
  # Degree days in previous 1-8 weeks
  tar_target(
    lake_dd_lag,
    tibble(
      date = seq.Date(
        from = as.Date("2011-01-01"),
        to = as.Date("2025-12-31"),
        by = "day"
      )
    ) %>%
      left_join(nbdc_temp) %>%
      mutate(
        !!!generate_lag_exprs(
          col = "temp",
          n_groups = 8,
          block_size = 7,
          lag_prefix = "dd"
        ),
        location = "Western Arm"
      )
  ),
  tar_target(
    est_dd_lag,
    tibble(
      date = seq.Date(
        from = as.Date("2013-01-01"),
        to = as.Date("2025-12-31"),
        by = "day"
      )
    ) %>%
      left_join(filter(est_temp, site == "lksba"), by = join_by(date)) %>%
      select(-site) %>%
      mutate(
        !!!generate_lag_exprs(
          col = "temp",
          n_groups = 8,
          block_size = 7,
          lag_prefix = "dd"
        ),
        location = "St. Louis Estuary"
      )
  ),

  # Average prior DD/water temp on each day of year
  tar_target(
    lake_dd_mean,
    lake_dd_lag %>%
      mutate(yday = yday(date)) %>%
      summarize(
        across(contains("dd"), ~ mean(., na.rm = TRUE), .names = "{.col}_mean"),
        mean_temp = mean(temp, na.rm = TRUE),
        .by = c(yday, location)
      )
  ),
  tar_target(
    est_dd_mean,
    est_dd_lag %>%
      mutate(yday = yday(date)) %>%
      summarize(
        across(contains("dd"), ~ mean(., na.rm = TRUE), .names = "{.col}_mean"),
        mean_temp = mean(temp, na.rm = TRUE),
        .by = c(yday, location)
      )
  ),

  # Combine lake and estuary prior DD and mean DD/Temp
  tar_target(dd_lag, bind_rows(lake_dd_lag, est_dd_lag)),
  tar_target(dd_mean, bind_rows(lake_dd_mean, est_dd_mean)),

  # Add in bloom data
  # remove thunder bay blooms
  tar_target(
    bloom_dd,
    left_join(bloom_filt, dd_lag) %>%
      mutate(lat = st_coordinates(.)[, 2]) %>%
      filter(lat < 48) %>%
      mutate(yday = yday(date)) %>%
      left_join(dd_mean)
  ),

  #Water temp vs blooms
  tar_target(
    #tar_read(bloom_temp_t)
    bloom_temp_t,
    t.test(
      bloom_dd$temp,
      bloom_dd$mean_temp,
      paired = TRUE,
      alternative = "greater"
    )
  ),
  tar_target(
    bloom_temp_plot,
    bloom_dd %>%
      ggplot(aes(mean_temp, temp, color = location)) +
      geom_abline(slope = 1, intercept = 0) +
      annotate("text", x = 23, y = 22.5, label = "1:1", size = 4) +
      geom_point(size = 2) +
      scale_x_continuous(
        name = "Average Water Temperature on Bloom Date (ºC)",
        limits = c(14, 23)
      ) +
      scale_y_continuous(
        name = "Actual Water Temperature During Bloom (ºC)",
        limits = c(14, 23)
      ) +
      scale_color_manual(name = NULL, values = c("#228833", "#4477aa")) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.78, 0.1),
        legend.background = element_rect(
          fill = "white",
          color = "black",
          linewidth = 0.2
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
      )
  ),

  #DD in prior 8 weeks vs blooms
  tar_target(
    #tar_read(bloom_dd8_t)
    bloom_dd8_t,
    t.test(
      bloom_dd$dd8,
      bloom_dd$dd8_mean,
      paired = TRUE,
      alternative = "greater"
    )
  ),

  ## Chl-a vs water temperature ----------------------------------------------
  tar_target(
    chl_temp_plot,
    lake_filt %>%
      mutate(year = year(date)) %>%
      summarize(
        across(c(temp, chl), ~ mean(., na.rm = TRUE)),
        .by = c(year, site)
      ) %>%
      filter(temp > 12) %>% # drop outlier cold temperatures
      plot_chl_relationship(
        temp,
        "Annual Mean Water Temperature (ºC)",
        TRUE,
        legend_pos = c(0.78, 0.93),
        label_pos = c(0.97, 0.87)
      )
  ),

  ## Combine plots and save --------------------------------------------------
  tar_target(
    dd_comb_plot,
    dd_line_plot /
      dd_box_plot +
      plot_annotation(tag_levels = 'a') &
      theme(plot.tag.position = c(0.13, 0.93))
  ),
  tar_target(
    dd_plot_file,
    ggsave(
      "figures/dd_comb.png",
      dd_comb_plot,
      width = 6.5,
      height = 8,
      dpi = 500
    ),
    format = "file"
  ),
  tar_target(
    temp_comb_plot,
    bloom_temp_plot /
      chl_temp_plot +
      plot_annotation(tag_levels = 'a') &
      theme(plot.tag.position = c(0.16, 0.94))
  ),
  tar_target(
    temp_plot_file,
    ggsave(
      "figures/temp_comb.png",
      temp_comb_plot,
      width = 4.25,
      height = 8,
      dpi = 500
    ),
    format = "file"
  ),

  ## Degree days vs chl-a ----------------------------------------------------
  tar_target(
    chl_dd_comp,
    lake_filt %>%
      left_join(lake_dd_lag, by = join_by(date)) %>%
      plot_chl_relationship(
        dd4,
        "Degree Days In Prior 8 Weeks (ºC)",
        FALSE,
        legend_pos = c(0.78, 0.93),
        label_pos = c(0.97, 0.87)
      )
  ),
  tar_target(
    chl_dd_comp_file,
    ggsave(
      "figures/chl_dd_comp.png",
      chl_dd_comp,
      width = 4.25,
      height = 4,
      dpi = 500
    ),
    format = "file"
  ),

  ## Buoy Temperature vs Sample Location Temperature -------------------------
  tar_target(
    buoy_temp_comp,
    lake_filt %>%
      left_join(nbdc_temp, by = join_by(date)) %>%
      ggplot(aes(x = temp.x, y = temp.y)) +
      xlab("Sample Location Water Temperature (ºC)") +
      ylab("Buoy Water Temperature (ºC)") +
      xlim(5, 25) +
      ylim(5, 25) +
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
        label.x = 0.75,
        label.y = 0.95,
        size = 3
      ) +
      scale_color_manual(
        name = NULL,
        values = c("linear regression" = "#4477aa")
      ) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.22, 0.93),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
      )
  ),
  tar_target(
    buoy_temp_comp_file,
    ggsave(
      "figures/buoy_temp_comp.png",
      buoy_temp_comp,
      width = 4.25,
      height = 4,
      dpi = 500
    ),
    format = "file"
  )
)
