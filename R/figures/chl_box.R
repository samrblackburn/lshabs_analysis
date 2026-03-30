chl_box_targets <- list(
  ## Make the plot

  # Remove outlier date
  # Assign Regions
  tar_target(
    chl_box,
    lake_filt %>%
      filter(date != ymd("2024-06-10")) %>%
      mutate(
        region = case_when(
          str_detect(site, "CB") ~ "CB",
          longitude < -91.5 ~ "W",
          longitude > -91.5 ~ "E",
          .default = NA
        )
      ) %>%
      mutate(year = as_factor(year(date))) %>%
      ggplot(aes(year, chl)) +
      geom_jitter(aes(color = region), width = 0.2, height = 0, alpha = 0.5) +
      geom_boxplot(
        outliers = FALSE,
        fill = NA,
        box.linewidth = 0.5,
        whisker.linewidth = 1
      ) +
      scale_color_manual(
        name = NULL,
        values = c("#009988", "#0077BB", "#33BBEE")
      ) +
      ylab("Chl-a (µg/L)") +
      xlab(NULL) +
      ylim(0, 10) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.1, 0.8),
        legend.background = element_rect(
          fill = "white",
          color = "black",
          linewidth = 0.2
        ),
        legend.margin = margin(t = 5, r = 8, b = 5, l = 5)
      )
  ),

  tar_target(
    chl_box_file,
    ggsave(
      "figures/chl_box.png",
      chl_box,
      width = 6.5,
      height = 4,
      dpi = 500
    ),
    format = "file"
  )
)
