nutrient_targets <- list(
  ## Plots of nitrate and TP ----------------------------------------
  # Drop outlier high TP, convert to ug/L
  # Convert to annual mean
  tar_target(
    chl_tp_plot,
    lake_filt %>%
      filter(tp < 0.1) %>%
      mutate(tp = tp * 1000) %>%
      mutate(year = year(date)) %>%
      summarize(
        across(c(chl, tp), ~ mean(., na.rm = TRUE)),
        .by = c(year, site)
      ) %>%
      plot_chl_relationship(tp, "Annual Mean Total Phosphorus (µg/L)", TRUE)
  ),
  # Drop outlier high and low NO3
  tar_target(
    chl_no3_plot,
    lake_filt %>%
      filter(no3 > 0.1 & no3 < 0.5) %>%
      mutate(year = year(date)) %>%
      summarize(
        across(c(chl, no3), ~ mean(., na.rm = TRUE)),
        .by = c(year, site)
      ) %>%
      plot_chl_relationship(
        no3,
        "Annual Mean Nitrate (mg/L)",
        TRUE,
        legend_pos = c(0.2, 0.13),
        label_pos = c(0.08, 0.03)
      )
  ),
  # Combine and save to file
  tar_target(
    chl_nut_plot,
    chl_tp_plot /
      chl_no3_plot +
      plot_annotation(tag_levels = 'a') &
      theme(plot.tag.position = c(0.13, 0.93))
  ),
  tar_target(
    chl_nut_file,
    ggsave(
      "figures/chl_no3_tp.png",
      chl_nut_plot,
      width = 4.5,
      height = 8,
      dpi = 500
    )
  )
)
