{
  #' Plot monthly distinct artists, distinct tracks and total streams
  #'
  #' This function computes, per month, the number of distinct artists,
  #' distinct tracks and total streams (only streams > 30s). It then plots
  #' all three series on the same time axis with a single common y-scale.
  #'
  #' @param df Raw dataframe containing at least: ts, ms_played, master_metadata_album_artist_name, master_metadata_track_name
  #' @return ggplot object (invisibly) and prints a preview of the monthly table
  library(dplyr)
  library(tidyr)
  library(ggplot2)
}

distinct_tracks_artists_plot <- function(df) {
  # Preprocess: drop podcasts, shorten timestamp to YYYY-MM and keep relevant columns
  df <- drop_podcasts(df)
  df$ts <- substr(df$ts, 1, 7)  # "YYYY-MM"
  df <- subset(
    df,
    ms_played > 30000,
    select = c(ts, master_metadata_album_artist_name, master_metadata_track_name)
  )
  
  # Convert month string to Date representing first day of month
  df <- df %>%
    dplyr::mutate(ts = as.character(ts)) %>%
    dplyr::mutate(Timestamp = as.Date(paste0(ts, "-01"), format = "%Y-%m-%d"))
  
  # Group by month and count distinct artists, distinct tracks and total streams
  monthly <- df %>%
    dplyr::group_by(Timestamp) %>%
    dplyr::summarise(
      unique_artists = dplyr::n_distinct(master_metadata_album_artist_name),
      unique_tracks  = dplyr::n_distinct(master_metadata_track_name),
      total_streams  = dplyr::n(),    # total streams (rows) in the month after filtering
      .groups = "drop"
    ) %>%
    dplyr::arrange(Timestamp)
  
  # Ensure continuous monthly sequence and fill missing months with zeros
  all_months <- seq(min(monthly$Timestamp, na.rm = TRUE),
                    max(monthly$Timestamp, na.rm = TRUE),
                    by = "month")
  all_months_df <- data.frame(Timestamp = all_months)
  
  plot_table <- dplyr::full_join(all_months_df, monthly, by = "Timestamp") %>%
    tidyr::replace_na(list(unique_artists = 0, unique_tracks = 0, total_streams = 0)) %>%
    dplyr::arrange(Timestamp)
  
  # Print a preview of the monthly counts
  print(utils::head(plot_table, n = 12))
  
  # Create ggplot: all three series on the same y-axis
  p <- ggplot(plot_table, aes(x = Timestamp)) +
    geom_line(
      aes(y = unique_tracks, color = "Unique tracks"),
      linewidth = 0.8
    ) +
    geom_point(
      aes(y = unique_tracks, color = "Unique tracks"),
      size = 1.5
    ) +
    geom_line(
      aes(y = unique_artists, color = "Unique artists"),
      linewidth = 0.8
    ) +
    geom_point(
      aes(y = unique_artists, color = "Unique artists"),
      size = 1.5
    ) +
    geom_line(
      aes(y = total_streams, color = "Total streams"),
      linewidth = 0.8
    ) +
    geom_point(
      aes(y = total_streams, color = "Total streams"),
      size = 1.5
    ) +
    scale_color_manual(
      values = c(
        "Unique tracks"  = "orangered",
        "Unique artists" = "steelblue",
        "Total streams"  = "darkgreen"
      ),
      name = NULL
    ) +
    labs(
      title = "Monthly distinct artists, distinct tracks and total streams",
      x = "Month",
      y = "Count per month"
    ) +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      legend.position = "top"
    )
  
  print(p)
  invisible(p)
}
