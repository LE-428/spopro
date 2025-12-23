{
  #' 3D surface: percentage distribution of release years over months
  #'
  #' Computes, per month, the relative frequency (%) of streams by release year,
  #' optionally smooths the percentage matrix with a separable moving average,
  #' and draws an interactive 3D surface using plotly.
  #'
  #' @param df_ext Extended dataframe (must contain columns: ts, ms_played, release_date)
  #' @param smooth Logical, whether to apply 2D moving-average smoothing (default TRUE)
  #' @param window Integer window size for the moving average (odd recommended, default 3)
  #' @return plotly object (invisibly) and prints a small preview table
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(plotly)
}

release_years_over_months_plot <- function(df_ext, smooth = TRUE, window = 3) {
  # Input validation
  if (!all(c("ts", "ms_played", "release_date") %in% names(df_ext))) {
    stop("Input must contain columns: ts, ms_played, release_date")
  }
  if (!is.logical(smooth)) smooth <- as.logical(smooth)
  window <- as.integer(window)
  if (is.na(window) || window < 1) window <- 1
  
  # --- Build the monthly x release_year distribution (robust extraction) ---
  df <- df_ext %>%
    dplyr::filter(ms_played > 30000) %>%
    dplyr::mutate(
      ts = as.character(ts),
      month = substr(ts, 1, 7),
      MonthDate = as.Date(paste0(month, "-01")),
      release_date = as.character(release_date)
    ) %>%
    dplyr::mutate(
      release_year_raw = sub("^([0-9]{4}).*", "\\1", release_date),
      release_year = suppressWarnings(as.integer(release_year_raw))
    ) %>%
    dplyr::filter(!is.na(MonthDate), !is.na(release_year), release_year != 0)
  
  if (nrow(df) == 0) {
    message("No data after filtering (either no rows >30s or all release years removed). Nothing to plot.")
    return(invisible(NULL))
  }
  
  all_months <- seq(min(df$MonthDate, na.rm = TRUE),
                    max(df$MonthDate, na.rm = TRUE),
                    by = "month")
  all_years  <- seq(min(df$release_year, na.rm = TRUE),
                    max(df$release_year, na.rm = TRUE),
                    by = 1)
  
  counts <- df %>%
    dplyr::group_by(MonthDate, release_year) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  grid <- tidyr::expand_grid(
    MonthDate = all_months,
    release_year = all_years
  )
  
  monthly_distribution <- grid %>%
    dplyr::left_join(counts, by = c("MonthDate", "release_year")) %>%
    replace_na(list(n = 0)) %>%
    dplyr::group_by(MonthDate) %>%
    dplyr::mutate(
      month_total = sum(n),
      percent = ifelse(month_total > 0, 100 * n / month_total, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(release_year, MonthDate)
  
  # small preview
  print(utils::head(monthly_distribution, n = 12))
  
  # pivot to matrix: rows = years (ascending), cols = months (ascending)
  wide <- monthly_distribution %>%
    dplyr::mutate(MonthChar = format(MonthDate, "%Y-%m")) %>%
    dplyr::select(release_year, MonthChar, percent) %>%
    tidyr::pivot_wider(names_from = MonthChar, values_from = percent, values_fill = 0) %>%
    dplyr::arrange(release_year)
  
  years_vec  <- wide$release_year
  months_vec <- colnames(wide)[-1]           # "YYYY-MM"
  z_mat      <- as.matrix(wide[ , -1 ])      # numeric matrix rows=years, cols=months
  storage.mode(z_mat) <- "double"
  
  # --- smoothing: separable moving average (row then column) ---
  smooth_matrix <- function(mat, w) {
    if (w <= 1) return(mat)
    # create symmetric kernel
    kernel <- rep(1 / w, w)
    # smooth rows (apply over columns)
    rows_smoothed <- t(apply(mat, 1, function(r) as.numeric(stats::filter(r, kernel, sides = 2))))
    rows_smoothed[is.na(rows_smoothed)] <- mat[is.na(rows_smoothed)]
    # smooth columns (apply over rows)
    cols_smoothed <- apply(rows_smoothed, 2, function(cvec) as.numeric(stats::filter(cvec, kernel, sides = 2)))
    cols_smoothed[is.na(cols_smoothed)] <- rows_smoothed[is.na(cols_smoothed)]
    return(cols_smoothed)
  }
  
  z_plot <- z_mat
  if (smooth && window > 1) {
    # cap window to reasonable size
    max_dim <- max(nrow(z_mat), ncol(z_mat))
    if (window > max_dim) window <- max_dim
    z_plot <- smooth_matrix(z_mat, window)
  }
  
  # --- 3D surface plot (plotly) ---
  p <- plot_ly(
    x = months_vec,
    y = years_vec,
    z = ~z_plot,
    type = "surface",
    colorscale = "Viridis",
    hoverinfo = "x+y+z"
  ) %>%
    layout(
      title = paste0("Release-year distribution over months (smooth=", smooth, ", window=", window, ")"),
      scene = list(
        xaxis = list(title = "Month (YYYY-MM)"),
        yaxis = list(title = "Release year"),
        zaxis = list(title = "Percent (%)")
      )
    )
  
  print(p)
  invisible(p)
}
