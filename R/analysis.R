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

  # Read in files
  tar_target(blooms, read_csv(bloom_file)),
  tar_target(
    lake,
    read_csv(
      lake_file,
      col_types = cols(don = col_double(), cnr = col_double())
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
  )
)
