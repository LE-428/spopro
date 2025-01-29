# Die Anzahl der Follower der gehörten Künstler plotten
# benötigt data_table_ext

artist_follower_plot <- function(data_table_ext, sort_by_streams = TRUE){
  # kurze Streams aussortieren und Spalten aussuchen
  follower_table <- subset(data_table_ext, data_table_ext$ms_played > 30000, select = c(artist_id, artist_followers))
  unique_artists <- unique(follower_table)
  # print(head(unique_artists))
  if (sort_by_streams == FALSE) {
    # Die Anzahl der Follower in Größenordnungen umwandeln mit Logarithmus
    followers_scaled <- as.integer(log10(unique_artists$artist_followers))
    ylabel = "Anzahl Künstler im Datensatz"
  } else {
    followers_scaled <- as.integer(log10(follower_table$artist_followers))
    ylabel = "Streams von Künstlern"
  }
  # print(head(followers_scaled))
  # Vorkommende Größenordnungen zählen
  scaled_frequencies <- table(followers_scaled)
  # print(scaled_frequencies)
  
  # Erstelle Kategorien basierend auf dem Logarithmus
  unique_scales <- sort(unique(followers_scaled))
  lower_bound <- 10^unique_scales  # Untere Grenze jedes Intervalls (z.B. 10^1 = 10, 10^2 = 100)
  upper_bound <- 10^(unique_scales + 1)  # Obere Grenze jedes Intervalls (z.B. 10^2 = 100, 10^3 = 1000)
  
  # Erstelle die Achsenbeschriftung als "1-10", "10-100", "100-1000", ...
  interval_labels <- paste0(lower_bound, "-", upper_bound)
  # print(interval_labels)
  
  # # Barplot mit der Verteilung der Lieder-Länge in 30s-Intervallen
  barplot_heights <- barplot(
    height = scaled_frequencies,                # Prozentsätze als Höhe der Balken
    #names.arg = interval_labels,      # Jahreszahlen als Namen der X-Achse
    col = "skyblue",                          # Farbe der Balken
    main = "Künstler",  # Titel des Plots
    xlab = "Anzahl Follower, logarithmisch",                            # Beschriftung der X-Achse
    ylab = ylabel,         # Beschriftung der Y-Achse
    ylim = c(0, max(scaled_frequencies) + 10),     # Y-Achse anpassen
    #las = 2  # Dreht die Achsenbeschriftung, falls sie zu lang ist
    xaxt = "n"
  )

  # Platziere die X-Achsen-Beschriftungen innerhalb der Balken
  text(
    x = barplot_heights + 0.2,                          # X-Position der Balken
    y = rep(0.1 * max(scaled_frequencies), length(barplot_heights)),                 # Y-Position innerhalb der Balken (Hälfte der Höhe)
    labels = interval_labels,                     # Die erstellten Labels
    srt = 90,                                      # Beschriftung nicht rotieren
    pos = 3,                                      # Position innerhalb der Balken
    cex = 0.8                                      # Schriftgröße
  )
}
