{
  #' Plot cumulative first-time discoveries of artists by month
  #'
  #' For each artist we take the month of the first stream (after filtering).
  #' We then count how many artists were first-heard in each month and plot the running total.
  #'
  #' @param df Raw dataframe containing at least: ts, ms_played, master_metadata_album_artist_name
  #' @return ggplot object (invisibly) and prints a preview of the monthly table
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
}

unique_artists_cumulated_plot <- function(df) {
  # drop podcasts and keep month + artist; require >30s streams
  df <- drop_podcasts(df)
  df$ts <- substr(df$ts, 1, 7)  # "YYYY-MM"
  df <- subset(
    df,
    ms_played > 30000 & !is.na(master_metadata_album_artist_name),
    select = c(ts, master_metadata_album_artist_name)
  )
  
  # convert month string to Date representing first day of month
  df <- df %>%
    dplyr::mutate(ts = as.character(ts),
                  Timestamp = as.Date(paste0(ts, "-01"), format = "%Y-%m-%d"))
  
  # For each artist find the month of first appearance
  first_by_artist <- df %>%
    dplyr::group_by(master_metadata_album_artist_name) %>%
    dplyr::summarise(
      first_month = min(Timestamp, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Count how many artists were first-heard in each month
  new_artists_per_month <- first_by_artist %>%
    dplyr::group_by(first_month) %>%
    dplyr::summarise(
      new_artists = n(),
      .groups = "drop"
    ) %>%
    dplyr::rename(Timestamp = first_month) %>%
    dplyr::arrange(Timestamp)
  
  # Ensure continuous monthly sequence and fill missing months with zeros
  if (nrow(new_artists_per_month) == 0) {
    message("No artist discovery data after filtering; nothing to plot.")
    return(invisible(NULL))
  }
  
  all_months <- seq(min(new_artists_per_month$Timestamp, na.rm = TRUE),
                    max(new_artists_per_month$Timestamp, na.rm = TRUE),
                    by = "month")
  all_months_df <- data.frame(Timestamp = all_months)
  
  plot_table <- dplyr::full_join(all_months_df, new_artists_per_month, by = "Timestamp") %>%
    tidyr::replace_na(list(new_artists = 0)) %>%
    dplyr::arrange(Timestamp)
  
  # Compute cumulative (running) sum of first-time artists
  plot_table <- plot_table %>%
    dplyr::mutate(cumulative_artists = cumsum(new_artists))
  
  # Print preview
  print(utils::head(plot_table, n = 12))
  
  # Plot: months on x-axis, cumulative distinct artists on y-axis
  p <- ggplot(plot_table, aes(x = Timestamp, y = cumulative_artists)) +
    geom_line(size = 1, color = "steelblue") +
    geom_point(size = 1.5, color = "steelblue") +
    labs(
      title = "Cumulative first-time artist discoveries over time",
      x = "Month",
      y = "Cumulative number of distinct artists"
    ) +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
  
  print(p)
  invisible(p)
}
