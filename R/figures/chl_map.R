chl_map_targets <- list(
  ## Labels and inset boxes ------------------------------------------------
  tar_target(
    chl_credit,
    tibble(
      x = -92.02,
      y = 46.555,
      label = get_credit("CartoDB.PositronNoLabels")
    )
  ),

  tar_target(
    chl_slre_box,
    st_bbox(
      c(xmin = -92.3, xmax = -92, ymin = 46.63, ymax = 46.79),
      crs = 4326
    ) %>%
      st_as_sfc() %>%
      st_cast("LINESTRING")
  ),

  ## Create plots ----------------------------------------------------------
  tar_target(
    main_chl,
    ggplot() +
      geom_raster(data = basemap_main, aes(x, y, fill = color)) +
      geom_raster(data = basemap_n, aes(x, y, fill = color)) +
      geom_raster(data = basemap_s, aes(x, y, fill = color)) +
      scale_fill_identity() +
      new_scale_fill() +
      geom_sf(
        data = filter(lake_med, !is.na(chl)),
        aes(fill = chl),
        size = 4,
        shape = 21
      ) +
      scale_fill_viridis_c(name = str_wrap("Chl-a (µg/L)", 5), end = 0.9) +
      geom_sf(data = chl_slre_box, linewidth = 0.2) +
      annotate("text", x = -92.03, y = 46.65, label = "a", size = 3) +
      geom_label(
        data = chl_credit,
        aes(x, y, label = label),
        fill = "white",
        color = "black",
        size = 2.5,
        label.r = unit(0, "lines")
      ) +
      annotation_scale(
        location = "br",
        pad_x = unit(0, "cm"),
        pad_y = unit(0.1, "cm"),
        width_hint = 0.1
      ) +
      annotation_north_arrow(
        location = "br",
        height = unit("0.75", "cm"),
        width = unit("0.75", "cm"),
        style = north_arrow_minimal,
        pad_x = unit(0, "cm"),
        pad_y = unit(0.5, "cm")
      ) +
      scale_x_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(-92.3, -90.4)
      ) +
      scale_y_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(46.54, 47.16)
      ) +
      theme_bw(base_size = 10) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.92, 0.65),
        legend.background = element_rect(
          fill = "white",
          color = "black",
          linewidth = 0.2
        ),
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
        plot.margin = margin(0, 0, 0, 0, "pt")
      )
  ),
  tar_target(
    est_chl,
    ggplot() +
      geom_raster(data = slre_basemap, aes(x, y, fill = color)) +
      scale_fill_identity() +
      new_scale_fill() +
      geom_sf(
        data = est_med,
        aes(fill = chl),
        size = 4,
        shape = 21
      ) +
      scale_fill_viridis_c(name = str_wrap("Chl-a (µg/L)", 5), end = 0.9) +
      guides(fill = guide_colorbar(barheight = unit(2, "cm"))) +
      annotate("text", x = -92.03, y = 46.65, label = "a", size = 5) +
      scale_x_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(-92.3, -92)
      ) +
      scale_y_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(46.63, 46.785)
      ) +
      theme_bw(base_size = 10) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.14, 0.6),
        legend.background = element_rect(
          fill = "white",
          color = "black",
          linewidth = 0.2
        ),
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
        plot.margin = margin(0, 0, 0, 0, "pt")
      )
  ),
  tar_target(
    chl_map,
    main_chl +
      est_chl +
      plot_layout(
        design = c(
          area(t = 1, b = 24, l = 1, r = 24),
          area(t = 1, b = 14, l = 1, r = 8)
        )
      )
  )
)

# ggsave("figures/chl_map.png", tar_read(chl_map), width = 6.5, height = 3.5, dpi = 500)
