# Plot the song length against the streams and show the average repetition count across the dataset

{
library(ggplot2)
library(dplyr)

duration_repetition_plot <- function(df_ext) {
  # Daten vorverarbeiten
  df_ext <- drop_podcasts(df_ext)  # Entfernen von Podcasts (falls Funktion definiert)
  df_ext <- subset(df_ext, ms_played > 1000, select = c(master_metadata_album_artist_name, master_metadata_track_name, duration_ms, ms_played, shuffle, reason_end))
  
  # Berechnung des Completion-Werts
  df_ext$completion = as.double(ifelse(df_ext$ms_played <= df_ext$duration_ms, df_ext$ms_played / df_ext$duration_ms, NA))
  df_ext <- subset(df_ext, !is.na(completion))  # Entferne NA-Werte
  
  # Aussortieren der Tracks mit Completion < 0.5
  df_ext <- subset(df_ext, completion >= 0.5 & duration_ms < 750000)
  
  # Gruppieren nach Track und Artist und Zählen der Vorkommen (Anzahl der Streams)
  df_grouped <- df_ext %>%
    group_by(master_metadata_album_artist_name, master_metadata_track_name, duration_ms) %>%
    summarise(
      total_streams = n(),  # Anzahl der Streams für jedes Lied
    ) %>% 
    ungroup()
  
  print(paste("Average song repetition", mean(df_grouped$total_streams)))
  
  # print(head(df_grouped))

  # Scatterplot erstellen
  p <- ggplot(df_grouped, aes(x = as.integer(duration_ms / 1000), y = total_streams)) +
    geom_point(alpha = 0.1) +  # Alpha-Wert für Transparenz
    labs(
      title = "Song Duration vs. Number of Streams",
      x = "Duration (seconds)",
      y = "Number of Streams"
    ) +
    ylim(0, min(max(df_grouped$total_streams), 200)) + 
    theme_minimal() +
    geom_vline(aes(xintercept = mean(duration_ms / 1000)), color = "red") +
    theme(plot.title = element_text(hjust = 0.5),  # Center title
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

  # Plot anzeigen
  print(p)
}

}
