library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)

# Plot listening time of the top artists summed up over the year

top_artists_ranking_plot <- function(df, year = 0, top_x = 10) {
  # All years
  years <- unique(substr(df$ts, 1, 4))
  
  if (year %in% years == 0) {
    year = last(years)
  }
  
  # Extract year
  df <- extract_year(year, df)
  
  # Convert timestamps to POSIXct
  df <- df %>%
    mutate(ts = as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  
  # 10-day intervals
  df <- df %>%
    filter(!is.na(master_metadata_album_artist_name)) %>%
    mutate(interval = floor_date(ts, unit = "10 days")) %>%
    group_by(master_metadata_album_artist_name, interval) %>%
    summarise(minutes = sum(ms_played) / 1000 / 60, .groups = "drop")
  
  # Total sum per artist in the year
  artist_total <- df %>%
    group_by(master_metadata_album_artist_name) %>%
    summarise(total_minutes = sum(minutes)) %>%
    arrange(desc(total_minutes)) %>%
    slice_head(n = top_x)
  
  top_artists <- artist_total$master_metadata_album_artist_name
  
  # Only top artists and accumulate over time
  df_top <- df %>%
    filter(master_metadata_album_artist_name %in% top_artists) %>%
    group_by(master_metadata_album_artist_name) %>%
    arrange(interval) %>%
    mutate(cumulative_minutes = cumsum(minutes)) %>%
    ungroup()
  
  # Factor for sorted colors/legend
  df_top <- df_top %>%
    mutate(artist = fct_reorder(master_metadata_album_artist_name,
                                -artist_total$total_minutes[match(master_metadata_album_artist_name, top_artists)]))
  
  df_top <- df_top %>% mutate(interval = as.Date(interval))
  
  # Plot
  ggplot(df_top, aes(x = interval, y = cumulative_minutes, color = artist)) +
    geom_line(size = 1.1) +
    labs(
      title = paste0("Top ", top_x, " artists – cumulated minutes over the year ", year),
      x = "Month",
      y = "Cumulated minutes",
      color = "Artist"
    ) +
    theme_minimal() +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
}