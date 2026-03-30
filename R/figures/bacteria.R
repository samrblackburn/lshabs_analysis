bacteria_targets <- list(
  # Load data --------------------------------------------
  tar_target(bac_file, "data/dnr_bac_out.csv", format = "file"),
  tar_target(bac, read_csv(bac_file)),

  # Data organizing --------------------------------------
  tar_target(
    site_order,
    c(
      "SCHAFFER BEACH",
      "AMNICON MOUTH",
      "POPLAR MOUTH",
      "BARDON CREEK",
      "PEARSON CREEK",
      "BRULE MOUTH",
      "DOUGLAS CO LINE",
      "IRON MOUTH",
      "FLAG MOUTH",
      "BLOCK 11",
      "CRANBERRY MOUTH",
      "BARK BAY",
      "SISKIWIT BAY",
      "MAWIKWE BAY",
      "LITTLE SAND BAY",
      "RED CLIFF BAY",
      "CHEQUAMEGON BAY",
      "BAD MOUTH"
    )
  ),

  tar_target(
    chlorophyta_list,
    c(
      "ankistrodesmus",
      "carteria",
      "chlamydomonas",
      "chlorella",
      "chlorococcum",
      "coelastrum",
      "crucigenia",
      "dictyosphaerium",
      "dimorphococcus",
      "dysmorphococcus",
      "eudorina",
      "gloeocystis",
      "golenkinia",
      "micractinium",
      "oocystis",
      "pandorina",
      "quadrigula",
      "scenedesmus",
      "schroederia",
      "selenastrum",
      "sphaerocystis",
      "tetraedron",
      "treubaria"
    )
  ),
  tar_target(
    heterokontophyta_list,
    c(
      "asterionella",
      "aulacoseira",
      "centrales",
      "cyclotella",
      "cymbella",
      "diatoma",
      "dinobryon",
      "fragilaria",
      "gonyostomum",
      "mallomonas",
      "meridion",
      "navicula",
      "nitzschia",
      "pennales",
      "placoneis",
      "stephanodiscus",
      "synedra",
      "synura",
      "tabellaria",
      "urosolenia"
    )
  ),
  tar_target(
    cyanophyta_list,
    c(
      "anabaena",
      "aphanizomenon",
      "aphanocapsa",
      "aphanothece",
      "chroococcus",
      "coelosphaerium",
      "limnothrix",
      "merismopedia",
      "nostoc",
      "planktolyngbya",
      "planktothrix",
      "pseudanabaena",
      "woronichinia"
    )
  ),
  tar_target(
    charophyta_list,
    c(
      "closterium",
      "cosmarium",
      "mougeotia"
    )
  ),
  tar_target(
    euglenophyta_list,
    c(
      "euglena",
      "phacus",
      "strombomonas",
      "trachelomonas"
    )
  ),
  tar_target(
    dinoflagellata_list,
    c(
      "ceratium",
      "gymnodinium",
      "peridinium"
    )
  ),
  tar_target(
    cryptista_list,
    c(
      "cryptomonas",
      "komma"
    )
  ),

  # Assign phyla to data
  # Calculate phylum proportions and total counts
  tar_target(
    bac_sum,
    bac %>%
      select(-c(station, latitude, longitude)) %>%
      pivot_longer(
        -c(date, name),
        names_to = "species",
        values_to = "cells_ml"
      ) %>%
      mutate(
        phylum = case_when(
          species %in% chlorophyta_list ~ "chlorophyta",
          species %in% heterokontophyta_list ~ "heterokontophyta",
          species %in% cyanophyta_list ~ "cyanophyta",
          species %in% charophyta_list ~ "charophyta",
          species %in% euglenophyta_list ~ "euglenophyta",
          species %in% dinoflagellata_list ~ "dinoflagellata",
          species %in% cryptista_list ~ "cryptista",
          .default = "other"
        )
      ) %>%
      mutate(date = round_date(date, "week")) %>%
      summarize(cells_ml = sum(cells_ml), .by = c(date, name, phylum)) %>%
      group_by(date, name) %>%
      mutate(
        total = sum(cells_ml),
        pct = cells_ml / total,
        name = factor(name, levels = site_order)
      ) %>%
      ungroup()
  ),

  # Proportion plot ---------------------------------------
  # Main WI Coast sites (2023-24)
  tar_target(
    main_comp_plot,
    bac_sum %>%
      mutate(
        year = year(date),
        date = if_else(year == 2024, date - years(1), date)
      ) %>%
      filter(year != 2022) %>%
      filter(
        !(name %in%
          c("LITTLE SAND BAY", "CHEQUAMEGON BAY", "BAD MOUTH"))
      ) %>%
      ggplot(aes(date, pct, fill = phylum)) +
      facet_grid(rows = vars(name), cols = vars(year)) +
      geom_area(position = "fill", color = "black", linewidth = 0.25) +
      ylab(NULL) +
      scale_fill_manual(
        name = NULL,
        values = c(
          "#999933",
          "#117733",
          "#CC6677",
          "#44AA99",
          "#AA4499",
          "#88CCEE",
          "#DDCC77"
        )
      ) +
      scale_x_date(
        name = NULL,
        date_labels = "%d-%b"
      ) +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  ),

  # Far east WI coast/CB sites (2024)
  tar_target(
    small_comp_plot,
    bac_sum %>%
      mutate(year = year(date)) %>%
      filter(
        (name %in%
          c("LITTLE SAND BAY", "CHEQUAMEGON BAY", "BAD MOUTH"))
      ) %>%
      ggplot(aes(date, pct, fill = phylum)) +
      facet_grid(rows = vars(name), cols = vars(year)) +
      geom_area(position = "fill", color = "black", linewidth = 0.25) +
      ylab(NULL) +
      scale_fill_manual(
        name = NULL,
        values = c(
          "#999933",
          "#117733",
          "#CC6677",
          "#44AA99",
          "#AA4499",
          "#88CCEE",
          "#DDCC77"
        )
      ) +
      scale_x_date(
        name = NULL,
        limits = c(ymd("2024-07-01"), ymd("2024-10-06")),
        breaks = c(
          ymd("2024-07-01"),
          ymd("2024-08-01"),
          ymd("2024-09-01"),
          ymd("2024-10-01")
        ),
        date_labels = "%d-%b"
      ) +
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  ),

  # Layout for combining plots
  tar_target(
    comp_plot_layout,
    "
AAB
AAB
AAB
AAC
AAC
"
  ),

  # Combine plots
  tar_target(
    bac_comp_plot,
    main_comp_plot +
      small_comp_plot +
      guide_area() +
      plot_layout(guides = 'collect', design = comp_plot_layout)
  ),

  # Total count plot --------------------------------------
  tar_target(
    bac_conc_plot,
    bac_sum %>%
      mutate(
        year = year(date),
        date = if_else(year == 2024, date - years(1), date)
      ) %>%
      filter(year != 2022) %>%
      ggplot(aes(date, total, color = name)) +
      facet_grid(rows = vars(year)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(
        name = "cells/mL",
        breaks = c(0, 2500, 5000, 7500, 10000, 12500)
      ) +
      scale_x_date(
        name = NULL,
        date_labels = "%d-%b"
      ) +
      scale_color_muted(name = NULL) +
      guides(color = guide_legend(ncol = 2)) +
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.35, 0.35),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  ),

  ## Save figures to file -------------------------------------
  tar_target(
    bac_comp_file,
    ggsave(
      "figures/bac_comp.png",
      bac_comp_plot,
      width = 6.5,
      height = 8.5,
      dpi = 500
    ),
    format = "file"
  ),
  tar_target(
    bac_conc_file,
    ggsave(
      "figures/bac_conc.png",
      bac_conc_plot,
      width = 6,
      height = 5.5,
      dpi = 500
    ),
    format = "file"
  )
)
