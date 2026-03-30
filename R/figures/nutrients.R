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
  ),

  ## Correlations between nutrients in lake ----------------------------
  # Correlation matrix
  tar_target(
    lake_nut_cor,
    lake_filt %>%
      select(
        no3,
        tdn,
        tn,
        nh3,
        tp,
        tdp,
        pp,
        po4,
        doc,
        poc,
        toc,
        si
      ) %>%
      cor(use = "pairwise.complete.obs", method = "pearson") %>%
      as.table() %>%
      as.data.frame() %>%
      ggplot(aes(x = Var1, y = Var2, fill = Freq)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "red",
        high = "blue",
        mid = "white",
        midpoint = 0,
        limit = c(-1, 1),
        name = "Pearson\nCorrelation"
      ) +
      geom_text(
        aes(label = round(Freq, 2)),
        color = "black",
        size = 2.5
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank()
      ) +
      coord_fixed()
  ),
  tar_target(
    lake_nut_cor_file,
    ggsave(
      "figures/lake_nut_cor.png",
      lake_nut_cor,
      width = 6.5,
      height = 6.5,
      dpi = 500
    )
  )
)
