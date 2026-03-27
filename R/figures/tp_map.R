tp_map_targets <- list(
  ## Annotations for map ---------------------------------------------------
  # Map credit statement
  tar_target(
    tp_credit,
    tibble(
      x = -92.09,
      y = 46.46,
      label = get_credit("CartoDB.PositronNoLabels")
    )
  ),

  # Tributary flowlines
  tar_target(
    trib_lines,
    read_sf("data/GLHD_LS_NHD_flowlines/Superior_Flowslines.shp") %>%
      mutate(
        GNIS_NAME = case_when(
          GLHDID == 196 ~ "Newton Creek",
          GLHDID == 194 ~ "Faxon Creek",
          .default = GNIS_NAME
        )
      ) %>%
      mutate(
        GNIS_NAME = str_remove(GNIS_NAME, "East Fork "),
        GNIS_NAME = str_remove(GNIS_NAME, "West Fork ")
      ) %>%
      filter(GNIS_NAME %in% trib_med$site)
  ),

  # Abbreviated names for tributaries
  tar_target(
    trib_med_shortnames,
    trib_med %>%
      mutate(
        site = str_remove(site, " River"),
        site = str_remove(site, " Creek"),
        lat = st_coordinates(.)[, 2],
        lon = st_coordinates(.)[, 1]
      )
  ),

  ## Create plots -------------------------------------------------------------
  tar_target(
    main_tp,
    ggplot() +
      geom_raster(data = basemap_main, aes(x, y, fill = color)) +
      geom_raster(data = basemap_n, aes(x, y, fill = color)) +
      geom_raster(data = basemap_s, aes(x, y, fill = color)) +
      scale_fill_identity() +
      new_scale_fill() +
      geom_sf(
        data = filter(lake_med, !is.na(tp)),
        aes(fill = tp * 1000),
        size = 4,
        shape = 21
      ) +
      scale_fill_viridis_c(
        name = str_wrap("TP (µg/L)", 5),
        end = 0.9,
        option = "cividis"
      ) +
      geom_sf(data = chl_slre_box, linewidth = 0.2) +
      annotate("text", x = -92.03, y = 46.65, label = "c", size = 3) +
      annotate("text", x = -90.45, y = 46.6, label = "a", size = 5) +
      annotation_scale(
        location = "tl",
        pad_x = unit(0, "cm"),
        pad_y = unit(0.1, "cm"),
        width_hint = 0.1
      ) +
      annotation_north_arrow(
        location = "tl",
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
        limits = c(46.54, 47.03)
      ) +
      theme_bw(base_size = 10) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.93, 0.6),
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
    est_tp,
    ggplot() +
      geom_raster(data = slre_basemap, aes(x, y, fill = color)) +
      scale_fill_identity() +
      new_scale_fill() +
      geom_sf(
        data = est_med,
        aes(fill = tp * 1000),
        size = 4,
        shape = 21
      ) +
      scale_fill_viridis_c(
        name = "TP (µg/L)",
        end = 0.9,
        option = "cividis"
      ) +
      guides(fill = guide_colorbar(barheight = unit(2, "cm"))) +
      annotate("text", x = -92.03, y = 46.65, label = "c", size = 5) +
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
        legend.position.inside = c(0.18, 0.64),
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
    trib_tp,
    ggplot() +
      geom_raster(data = basemap_main, aes(x, y, fill = color)) +
      geom_raster(data = basemap_s, aes(x, y, fill = color)) +
      scale_fill_identity() +
      geom_sf(data = trib_lines) +
      new_scale_fill() +
      geom_sf(
        data = trib_med,
        aes(fill = tp * 1000),
        size = 4,
        shape = 21
      ) +
      scale_fill_viridis_c(
        name = str_wrap("TP (µg/L)", 5),
        limits = c(0, 80),
        option = "cividis",
        na.value = "#EE7733"
      ) +
      guides(fill = guide_colorbar(direction = "horizontal")) +
      annotate("text", x = -90.55, y = 46.5, label = "b", size = 5) +
      geom_label_repel(
        data = trib_med_shortnames,
        aes(lon, lat, label = str_wrap(site, 5)),
        size = 2.5,
        lineheight = 0.8,
        label.r = unit(0, "lines"),
        label.padding = 0.1
      ) +
      geom_label(
        data = tp_credit,
        aes(x, y, label = label),
        fill = "white",
        color = "black",
        size = 2,
        label.r = unit(0, "lines")
      ) +
      scale_x_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(-92.3, -90.5)
      ) +
      scale_y_continuous(
        name = NULL,
        expand = c(0, 0),
        limits = c(46.45, 47.04)
      ) +
      theme_bw(base_size = 10) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.55, 0.9),
        legend.background = element_rect(
          fill = "white",
          color = "black",
          linewidth = 0.2
        ),
        legend.margin = margin(t = 5, r = 8, b = 5, l = 5),
        plot.margin = margin(0, 0, 0, 0, "pt")
      )
  ),

  tar_target(
    tp_map,
    main_tp +
      trib_tp +
      est_tp +
      plot_layout(
        design = c(
          area(t = 1, b = 24, l = 1, r = 24),
          area(t = 24, b = 24 * 2, l = 1, r = 24),
          area(t = 24 - 4, b = 24 + 8, l = 1, r = 24 / 3)
        )
      )
  )
)

# ggsave("figures/comb_tp_map.png", tar_read(tp_map), width = 6.5, height = 6.2, dpi = 500)
