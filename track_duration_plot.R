# die Dauer der gehörten Lieder plotten
# benötigt data_table_ext

track_duration_plot <- function(data_table_ext, sort_by_streams = TRUE) {
  # Vollständig gehörte Lieder verwenden und Spalten aussuchen
  listened_tracks <-
    subset(
      data_table_ext,
      data_table_ext$ms_played == data_table_ext$duration_ms,
      select = c(id, duration_ms)
    )
  unique_tracks <- unique(listened_tracks)
  # print(head(unique_tracks))
  # Jedes Lied nur einmal verwenden oder jeden Stream
  if (sort_by_streams == FALSE) {
    # Die timestamps in Intervalle von 30s einteilen mit floor division
    duration_classes <- unique_tracks$duration_ms %/% (30 * 1000)
    ylabel = "Number of unique songs"
  } else {
    duration_classes <- listened_tracks$duration_ms %/% (30 * 1000)
    ylabel = "Number of streams"
  }
  # print(head(duration_classes))
  # Die Klassen zählen
  duration_frequencies <- table(duration_classes)
  # print(duration_frequencies)
  
  # Erstelle die Achsenbeschriftung als "0-30", "30-60", ...
  interval_labels <- paste0(as.numeric(names(duration_frequencies)) * 30, "-", 
                            (as.numeric(names(duration_frequencies)) + 1) * 30)
  
  # Barplot mit der Verteilung der Lieder-Länge in 30s-Intervallen
  barplot_heights <- barplot(
    height = duration_frequencies,                # Prozentsätze als Höhe der Balken
    #names.arg = interval_labels,      # Jahreszahlen als Namen der X-Achse
    col = "plum",                          # Farbe der Balken
    main = "Distribution of song length",  # Titel des Plots
    xlab = "Duration in seconds",                            # Beschriftung der X-Achse
    ylab = ylabel,         # Beschriftung der Y-Achse
    ylim = c(0, max(duration_frequencies) + 10),     # Y-Achse anpassen
    #las = 2  # Dreht die Achsenbeschriftung, falls sie zu lang ist
    xaxt = "n"
  )
  
  # Platziere die X-Achsen-Beschriftungen innerhalb der Balken
  text(
    x = barplot_heights + 0.2,                          # X-Position der Balken
    y = rep(0.05 * max(duration_frequencies), length(barplot_heights)),                 # Y-Position bei 5% der Höhe der y-Achse
    labels = interval_labels,                     # Die erstellten Labels
    srt = 90,                                      # Beschriftung nicht rotieren
    pos = 3,                                      # Position innerhalb der Balken
    cex = 0.8                                      # Schriftgröße
  )
  
  
  
}
