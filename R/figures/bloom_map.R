bloom_map_targets <- list(
  ## Map label prep -----------------------------------------------------
  tar_target(
    bloom_credit,
    tibble(
      x = -90.77,
      y = 46.565,
      label = get_credit("CartoDB.PositronNoLabels")
    )
  ),
  tar_target(
    bloom_key_locs,
    tibble(
      x = c(-92.08, -91.08, -90.7, -91.1),
      y = c(46.61, 46.76, 47, 46.62),
      label = c(
        "Duluth / St. Louis River Estuary",
        "Siskiwit & Mawikwe Bays",
        "Apostle Islands",
        "Chequamegon Bay"
      )
    )
  ),
  tar_target(
    bloom_key_locs_big,
    tibble(
      x = c(-91.55, -90),
      y = c(47.25, 48.2),
      label = c(
        "Western Arm",
        "Thunder Bay"
      )
    )
  ),

  ## Extent boxes for inset maps -----------------------------------------
  tar_target(
    bloom_slre_box,
    st_bbox(
      c(xmin = -92.14, xmax = -91.94, ymin = 46.67, ymax = 46.81),
      crs = 4326
    ) %>%
      st_as_sfc() %>%
      st_cast("LINESTRING")
  ),
  tar_target(
    bloom_apostle_box,
    st_bbox(
      c(xmin = -91.22, xmax = -91.00, ymin = 46.80, ymax = 46.96),
      crs = 4326
    ) %>%
      st_as_sfc() %>%
      st_cast("LINESTRING")
  ),
  tar_target(
    bloom_main_box,
    st_bbox(
      c(xmin = -92.3, xmax = -90.5, ymin = 46.55, ymax = 47.1),
      crs = 4326
    ) %>%
      st_as_sfc() %>%
      st_cast("LINESTRING")
  ),

  ## Create Plots ---------------------------------------------------------
  tar_target(
    main_blooms,
    bloom_filt %>%
      mutate(lat = st_coordinates(.)[, 2]) %>%
      filter(lat < 48) %>%
      filter(confirmed) %>%
      mutate(year = factor(year)) %>%
      ggplot() +
      geom_raster(data = basemap_main, aes(x, y, fill = color)) +
      geom_raster(data = basemap_n, aes(x, y, fill = color)) +
      geom_raster(data = basemap_s, aes(x, y, fill = color)) +
      scale_fill_identity() +
      new_scale_fill() +
      geom_sf(
        aes(fill = year),
        alpha = 0.7,
        size = 4,
        shape = 21,
        linewidth = 0.1
      ) +
      scale_fill_discreterainbow(name = NULL) +
      guides(fill = guide_legend(ncol = 2)) +
      geom_sf(data = bloom_slre_box, linewidth = 0.2) +
      geom_sf(data = bloom_apostle_box, linewidth = 0.2) +
      annotate("text", x = -92.26, y = 46.59, label = "b", size = 5) +
      annotate("text", x = -92.12, y = 46.685, label = "c") +
      annotate("text", x = -91.20, y = 46.815, label = "d") +
      geom_label(
        data = bloom_key_locs,
        aes(x, y, label = str_wrap(label, 15)),
        fill = "white",
        lineheight = 0.8,
        label.r = unit(0, "lines")
      ) +
      geom_label(
        data = bloom_credit,
        aes(x, y, label = label),
        fill = "white",
        color = "black",
        size = 2.5,
        label.r = unit(0, "lines")
      ) +
      annotation_scale(
        location = "br",
        pad_x = unit(0, "cm"),
        pad_y = unit(0.5, "cm"),
        width_hint = 0.1
      ) +
      annotation_north_arrow(
        location = "br",
        height = unit("0.75", "cm"),
        width = unit("0.75", "cm"),
        style = north_arrow_minimal,
        pad_x = unit(0, "cm"),
        pad_y = unit(1, "cm")
      ) +
      scale_x_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(-92.3, -90.5)
      ) +
      scale_y_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(46.55, 47.1)
      ) +
      theme_bw(base_size = 10) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.11, 0.75),
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
    slre_blooms,
    bloom_filt %>%
      filter(confirmed) %>%
      mutate(year = factor(year)) %>%
      ggplot() +
      geom_raster(data = slre_basemap, aes(x, y, fill = color)) +
      scale_fill_identity() +
      new_scale_fill() +
      geom_sf(
        aes(fill = year),
        alpha = 0.7,
        size = 4,
        shape = 21,
        linewidth = 0.1
      ) +
      annotate("text", x = -92.12, y = 46.685, label = "c", size = 5) +
      scale_fill_discreterainbow(name = NULL) +
      scale_x_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(-92.14, -91.94)
      ) +
      scale_y_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(46.67, 46.81)
      ) +
      theme_bw(base_size = 10) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "pt")
      )
  ),
  tar_target(
    apostle_blooms,
    bloom_filt %>%
      filter(confirmed) %>%
      mutate(year = factor(year)) %>%
      ggplot() +
      geom_raster(data = apostle_basemap, aes(x, y, fill = color)) +
      scale_fill_identity() +
      new_scale_fill() +
      geom_sf(
        aes(fill = year),
        alpha = 0.7,
        size = 4,
        shape = 21,
        linewidth = 0.1
      ) +
      scale_fill_discreterainbow(name = NULL) +
      annotate("text", x = -91.20, y = 46.815, label = "d", size = 5) +
      scale_x_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(-91.22, -91)
      ) +
      scale_y_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(46.80, 46.96)
      ) +
      theme_bw(base_size = 10) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "pt")
      )
  ),
  tar_target(
    big_blooms,
    bloom_filt %>%
      filter(confirmed) %>%
      mutate(year = factor(year)) %>%
      ggplot() +
      geom_raster(data = big_basemap, aes(x, y, fill = color)) +
      scale_fill_identity() +
      new_scale_fill() +
      geom_sf(
        aes(fill = year),
        alpha = 0.7,
        size = 4,
        shape = 21,
        linewidth = 0.1
      ) +
      scale_fill_discreterainbow(name = NULL) +
      geom_sf(data = bloom_main_box, linewidth = 0.2) +
      annotate("text", x = -90.4, y = 46.7, label = "b") +
      annotate("text", x = -92.2, y = 48.55, label = "a", size = 5) +
      geom_label(
        data = bloom_key_locs_big,
        aes(x, y, label = label),
        fill = "white",
        label.r = unit(0, "lines")
      ) +
      scale_x_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(-92.5, -88.25)
      ) +
      scale_y_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(46.5, 48.75)
      ) +
      theme_bw(base_size = 10) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "pt")
      )
  ),
  tar_target(
    bloom_comb_map,
    (big_blooms | slre_blooms | apostle_blooms) /
      main_blooms +
      plot_layout(heights = c(1.4, 2))
  ),
  tar_target(
    bloom_map_file,
    ggsave(
      "figures/bloom_map.png",
      bloom_comb_map,
      width = 6.5,
      height = 5,
      dpi = 500
    ),
    format = "file"
  )
)
