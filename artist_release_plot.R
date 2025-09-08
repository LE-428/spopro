# Plot the releases of the specified artist and the users streaming activity
library(ggplot2)
library(dplyr)
library(tidyr)

artist_release_plot <- function(data_frame_ext, artist_string, feature_search_bool = FALSE, exact_search = FALSE) {
  
  artist_string <- gsub("([()])", "\\\\\\1", artist_string)
  artist_string <- gsub("\\$", "\\\\\\$", artist_string)
  
  # Filter für den Künstler
  if (exact_search) {
    if (feature_search_bool) {
      aux_table <- data_frame_ext[union(
        which(grepl(paste0("^", artist_string, "$"), data_frame_ext$master_metadata_album_artist_name, ignore.case = TRUE)),
        which(grepl(paste0("^", artist_string, "$"), data_frame_ext$master_metadata_track_name, ignore.case = TRUE))
      ), ]
    } else {
      aux_table <- data_frame_ext[which(grepl(paste0("^", artist_string, "$"), data_frame_ext$master_metadata_album_artist_name, ignore.case = TRUE)), ]
    }
  } else {
    if (feature_search_bool) {
      aux_table <- data_frame_ext[union(
        which(grepl(artist_string, data_frame_ext$master_metadata_album_artist_name, ignore.case = TRUE)),
        which(grepl(artist_string, data_frame_ext$master_metadata_track_name, ignore.case = TRUE))
      ), ]
    } else {
      aux_table <- data_frame_ext[which(grepl(artist_string, data_frame_ext$master_metadata_album_artist_name, ignore.case = TRUE)), ]
    }
  }
  
  
  # Release-Datum als Date konvertieren
  aux_table$release_date <- as.Date(aux_table$release_date)
  
  ###################################
  # Part of the artist_time_plot.R function
  
  aux_table$ts <- substr(aux_table$ts, 1, 7)  # Extract year and month from timestamp
  aux_table_time <- subset(aux_table, ms_played > 30000, select = c(ts))  # Filter songs with more than 30,000 ms played

  # Group by timestamp and count occurrences
  aux_table_time <- aux_table_time %>%
    group_by(ts) %>%
    summarise(Monthly_Plays = length(ts)) %>%
    rename(Timestamp = ts)

  # Convert the Timestamp column to Date format
  aux_table_time$Timestamp <- as.Date(paste0(aux_table_time$Timestamp, "-01"), format="%Y-%m-%d")

  # Create a sequence of all months within the range of the data
  all_months <- seq(min(aux_table_time$Timestamp, na.rm = TRUE),
                    max(aux_table_time$Timestamp, na.rm = TRUE),
                    by = "month")

  # Create a data frame with all months and join it with aux_table
  all_months_df <- data.frame(Timestamp = all_months)
  aux_table_full <- full_join(all_months_df, aux_table_time, by = "Timestamp") %>%
    mutate(Monthly_Plays = replace_na(Monthly_Plays, 0))  # Replace NA with 0 for missing months

  ####################################
  
  # Gruppieren nach Release-Datum
  release_summary <- aux_table %>%
    group_by(release_date) %>%
    summarise(
      unique_songs = n_distinct(master_metadata_track_name),
      album_name = first(master_metadata_album_album_name)  # ersten Albumnamen nehmen
    ) %>%
    arrange(release_date)
  
  # Alle Tage zwischen erstem und letztem Release abdecken
  all_dates <- data.frame(release_date = seq(min(release_summary$release_date, na.rm = TRUE),
                                             max(release_summary$release_date, na.rm = TRUE),
                                             by = "day"))
  
  # Gruppieren nach Release-Datum und Anzahl unique Songs zählen
  release_summary_full <- release_summary %>%
    mutate(album_name = replace_na(album_name, ""))  # nur Release-Tage
  
  # Plot with only releases by artist
  # # Plot mit Balken und optionalen Album-Labels
  # ggplot(release_summary_full, aes(x = release_date, y = unique_songs)) +
  #   geom_col(fill = "darkgreen", width = 5) +
  #   geom_text(aes(label = ifelse(unique_songs > 5, album_name, "")),
  #             angle = 45, hjust = 0, vjust = -0.3, size = 3) +
  #   labs(title = paste("Release Activity for", artist_string),
  #        x = "Release Date",
  #        y = "Number of released songs") +
  #   theme_minimal() +
  #   scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #         plot.title = element_text(hjust = 0.5),
  #         panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
  
  ggplot() +
    # Release-Balken
    geom_col(data = release_summary_full,
             aes(x = release_date, y = unique_songs),
             fill = "darkgreen", width = 5) +
    geom_text(data = release_summary_full,
              aes(x = release_date, y = unique_songs,
                  label = ifelse(unique_songs > 5, album_name, "")),
              angle = 45, hjust = 0, vjust = -0.3, size = 3) +

    # Streaming-Aktivität als Linie + Punkte
    geom_line(data = aux_table_full,
              aes(x = as.Date(Timestamp, format="%Y-%m"), y = Monthly_Plays),
              color = "blue", size = 1) +
    geom_point(data = aux_table_full,
               aes(x = as.Date(Timestamp, format="%Y-%m"), y = Monthly_Plays),
               color = "red", size = 1.5) +

    # Achsen & Titel
    labs(title = paste("Artist release & personal streaming activity for", artist_string),
         x = "Date",
         y = "Number of released tracks/streams") +

    # Skalen & Theme
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
  
}
