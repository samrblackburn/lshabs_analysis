bloom_count_targets <- list(
  ## Count blooms ------------------------------------------------------------

  # Count by year and verification status
  tar_target(
    bloom_count,
    bloom_filt %>%
      st_drop_geometry() %>%
      distinct(date, location, .keep_all = TRUE) %>%
      summarise(
        all = n(),
        confirmed = sum(confirmed),
        verified = sum(verified),
        .by = c(location, year)
      ) %>%
      bind_rows(data.frame(
        year = c(2013, 2014, 2015),
        all = c(0, 0, 0),
        confirmed = c(0, 0, 0),
        verified = c(0, 0, 0),
        location = rep("Western Arm", 3)
      )) %>%
      mutate(scale1 = "Verified", scale2 = "Reported")
  ),

  # Count by year
  tar_target(
    bloom_count_total,
    bloom_filt %>%
      st_drop_geometry() %>%
      distinct(date, location, .keep_all = TRUE) %>%
      summarise(
        all = n(),
        confirmed = sum(confirmed),
        verified = sum(verified),
        .by = c(year)
      ) %>%
      bind_rows(data.frame(
        year = c(2013, 2014, 2015),
        all = c(0, 0, 0),
        confirmed = c(0, 0, 0),
        verified = c(0, 0, 0)
      )) %>%
      mutate(scale1 = "Verified", scale2 = "Reported")
  ),

  ## Create plot -------------------------------------------------------------
  tar_target(
    bloom_count_plot,
    ggplot() +
      geom_bar(
        data = bloom_count,
        aes(
          year,
          verified,
          fill = factor(
            location,
            levels = c("St. Louis Estuary", "Thunder Bay", "Western Arm")
          )
        ),
        stat = "identity"
      ) +
      geom_line(
        data = bloom_count_total,
        aes(year, all, linetype = scale2),
        linewidth = 1
      ) +
      geom_point(data = bloom_count_total, aes(year, all), size = 2) +
      theme_bw(base_size = 12) +
      scale_y_continuous(
        name = "Reported Blooms",
        breaks = c(0, 2, 4, 6, 8, 10, 12)
      ) +
      scale_x_continuous(
        name = "",
        breaks = c(2012, 2014, 2016, 2018, 2020, 2022, 2024)
      ) +
      scale_fill_manual(
        name = "Verified",
        values = c("#228833", "#66CCEE", "#4477aa")
      ) +
      scale_linetype_manual(
        name = "Reported",
        values = c("solid"),
        labels = "All Locations"
      ) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.65)
      )
  )
)

# ggsave("figures/bloom_count.png", tar_read(bloom_count_plot), width = 6.5, height = 3.5, dpi = 500)
