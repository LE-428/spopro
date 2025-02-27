# Scatter plot with song duration and completion rate

duration_completion_plot <- function(df_ext){
  df_ext <- drop_podcasts(df_ext)
  df_ext <- subset(df_ext, ms_played > 1000, select = c(master_metadata_album_artist_name, duration_ms, ms_played, shuffle, reason_end))
  
  # df_ext$completion = as.double(df_ext$ms_played / df_ext$duration_ms)
  df_ext$completion = as.double(ifelse(df_ext$ms_played <= df_ext$duration_ms, df_ext$ms_played / df_ext$duration_ms, NA))
  # print(head(df_ext))
  df_ext <- subset(df_ext, !is.na(completion) & duration_ms < 750000)
  
  # Scatterplot mit Dauer vs. Completion Rate
  ggplot(df_ext, aes(x = as.integer(duration_ms / 1000), y = completion)) +  # Umrechnung in Minuten
    geom_point(alpha = 0.1) +
    labs(title = "Completion Rate vs. Song Duration",
         x = "Song Duration (s)",
         y = "Completion Rate") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),  # Center title
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
}
