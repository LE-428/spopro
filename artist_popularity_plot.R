# Die Häufigkeiten der popularities der Künstler plotten
# benötigt erweiterte Tabelle, y-Achse wird entweder skaliert nach den Streams,
# welche mit einem Künstler der bestimmten popularity zusammenhängen oder nach
# unique Künstlern und absoluten Zahlen

artist_popularity_plot <- function(data_table_ext, sort_by_streams = TRUE){
  if (sort_by_streams == TRUE){
    artist_popularities <- subset(data_table_ext, data_table_ext$ms_played > 30000, select = c(artist_popularity))
    artist_popularity_absolute <- table(artist_popularities)
    # print(artist_popularity_absolute)
    
    # Barplot der prozentualen Anteile der Jahre
    barplot(
      height = artist_popularity_absolute,                # Prozentsätze als Höhe der Balken
      names.arg = names(artist_popularity_absolute),      # Jahreszahlen als Namen der X-Achse
      col = "slateblue",                          # Farbe der Balken
      main = "Verteilung der Künstler-Beliebtheit",  # Titel des Plots
      xlab = "Beliebtheit (Popularity)",                            # Beschriftung der X-Achse
      ylab = "Anzahl der Streams",         # Beschriftung der Y-Achse
      ylim = c(0, max(artist_popularity_absolute) + 10)   # Y-Achse anpassen
    )
    
  } else {
    artist_popularities <- subset(data_table_ext, data_table_ext$ms_played > 30000, select = c(artist_id, artist_popularity))
    unique_artists <- unique(artist_popularities)     
    # print(unique_artists)
    distribution <- table(unique_artists$artist_popularity)  
    
    # Barplot der prozentualen Anteile der Jahre
    barplot(
      height = distribution,                # Prozentsätze als Höhe der Balken
      names.arg = names(distribution),      # Jahreszahlen als Namen der X-Achse
      col = "slateblue",                          # Farbe der Balken
      main = "Verteilung der Künstler-Beliebtheit",  # Titel des Plots
      xlab = "Beliebtheit (Popularity)",                            # Beschriftung der X-Achse
      ylab = "Anzahl der Künstler",         # Beschriftung der Y-Achse
      ylim = c(0, max(distribution) + 10)   # Y-Achse anpassen
    ) 
  }
}
