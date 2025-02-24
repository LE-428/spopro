# Plot the average song length and popularity together with the genre of all artists

{
  
library(ggplot2)
library(RColorBrewer)

artist_cluster_plot <- function(df_ext){
  top_20_genres <- top_genres(df_ext, top_x = 20)
  # print(top_20_genres)
  
  # Schritt 1: Erstellen des kleinen DataFrames und Verarbeiten von NA-Werten
  small_df <- df_ext %>%
    subset(select = c(master_metadata_album_artist_name, duration_ms, artist_followers, artist_genres)) %>%
    mutate(artist_genres = ifelse(is.na(artist_genres), "", as.character(artist_genres))) %>%
    distinct()
  
  # Schritt 2: Gruppieren nach Künstlernamen und Berechnen der Durchschnitts-Songlänge
  df_artist_grouped <- small_df %>%
    group_by(master_metadata_album_artist_name) %>%
    summarise(
      avg_duration_ms = mean(duration_ms),  # Durchschnittliche Songlänge
      artist_followers = first(artist_followers),  # Annahme: Alle Einträge haben die gleiche Follower-Zahl
      artist_genres = first(artist_genres)  # Genre
    ) %>%
    arrange(desc(avg_duration_ms)) %>%
    ungroup()
  
  # Schritt 3: Umbenennen der Spalten
  df_artist_grouped <- df_artist_grouped %>%
    rename(
      Artist = master_metadata_album_artist_name
    )
  
  # Schritt 4: Konvertieren von Millisekunden in Sekunden
  df_artist_grouped <- df_artist_grouped %>%
    mutate(avg_duration_ms = floor(avg_duration_ms / 1000))
  
  # print(head(df_artist_grouped))
  
  
  # PLOT 
  
  # Schritt 1: Filter für 'avg_duration_ms' unter 800
  df_artist_grouped <- df_artist_grouped %>% 
    filter(avg_duration_ms < 600)
  
  # Schritt 2: Erstelle die Farbkodierung für die Genres
  # Angenommene top_genres, du solltest sie anpassen
  top_genres <- top_20_genres$all_genres
  palette <- brewer.pal(12, "Paired")  # Wir nutzen eine Palette mit bis zu 12 Farben
  
  # Erstelle eine benutzerdefinierte Farbpalette für die verbleibenden Genres
  additional_colors <- colorRampPalette(c("gray", "red", "blue", "green", "yellow", "purple", "orange"))(8)  # 8 zusätzliche Farben
  
  # Kombiniere die beiden Paletten
  palette <- c(palette, additional_colors)
  
  # Genre-Farbzuordnung
  genre_color_map <- setNames(palette, top_genres)
  
  # Funktion, um eine Farbe basierend auf Genre zuzuweisen
  get_artist_color <- function(genres) {
    genre_list <- strsplit(genres, ",")[[1]]
    for (genre in genre_list) {
      genre <- trimws(genre)
      if (genre %in% names(genre_color_map)) {
        return(genre)
      }
    }
    return("")  # Standardfarbe für Genres, die nicht in den Top-Genres sind
  }
  
  # Schritt 3: Neue Spalte 'color' hinzufügen
  df_artist_grouped$top_genre <- sapply(df_artist_grouped$artist_genres, get_artist_color)
  
  # Schritt 4: Erstelle zwei DataFrames für graue und farbige Punkte
  df_gray <- df_artist_grouped %>% filter(top_genre == "")
  df_colored <- df_artist_grouped %>% filter(top_genre != "")
  
  # print(head(df_colored))
  print(genre_color_map)
  
  
  # Schritt 5: Scatterplot erstellen
  ggplot() +
    # Graue Punkte (für nicht-top Genres) 
    geom_point(data = df_gray, aes(x = avg_duration_ms, y = artist_followers), 
               shape = 1, color = "gray", size = 3, stroke = 0.5, alpha = 0.5) +
    # Farbige Punkte (für Top Genres)
    geom_point(data = df_colored, aes(x = avg_duration_ms, y = artist_followers, color = top_genre), 
               size = 3, stroke = 0.5, alpha = 0.8) +
    # Linie für den Durchschnitt der Songlänge
    geom_vline(aes(xintercept = mean(df_artist_grouped$avg_duration_ms)), color = "red") +
    # Linie für den Durchschnitt der Follower
    geom_hline(aes(yintercept = mean(df_artist_grouped$artist_followers)), color = "blue") +
    scale_y_log10(breaks = scales::log_breaks(base = 10)) +
    ggtitle("Scatter plot of average song length and followers") +
    # theme(plot.title = element_text(hjust = 1)) +
    labs(
      x = "Average song length in seconds",
      y = "Number of artist followers"
    ) +
    theme_minimal() +
    # Farbskala manuell setzen (nur für Top-Genres)
    scale_fill_identity() +  # Setzt direkte Farbwerte, ohne eine Skala zu definieren
    # scale_color_manual(values = c(
    #   "german hip hop" = "#A6CEE3",
    #   "rap" = "#1F78B4",
    #   "lo-fi" = "#B2DF8A",
    #   "lo-fi beats" = "#33A02C",
    #   "cloud rap" = "#FB9A99",
    #   "hip hop" = "#E31A1C",
    #   "melodic rap" = "#FDBF6F",
    #   "west coast hip hop" = "#FF7F00",
    #   "east coast hip hop" = "#CAB2D6",
    #   "turkish hip hop" = "#6A3D9A",
    #   "emo rap" = "#FFFF99",
    #   "gangster rap" = "#B15928",
    #   "german pop" = "#BEBEBE",
    #   "rage rap" = "#F51B1B",
    #   "trap" = "#4800B6",
    #   "chill beats" = "#00916D",
    #   "jazz rap" = "#6DFF00",
    #   "southern hip hop" = "#E3BF44",
    #   "drill" = "#AD33CD",
    #   "g-funk" = "#FFA500"
    # )) +
    scale_color_manual(values = genre_color_map) + 
    theme(legend.position = "right") +  # Platzierung der Legende
    # theme(legend.title = element_text(text = "Top 20 Genres"))
    # Hinzufügen eines Rahmens um den gesamten Plot
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.ticks.y = element_line(color = "black", linewidth = 1),  # Ticks auf der Y-Achse
      axis.ticks.x = element_line(color = "black", linewidth = 1),  # Ticks auf der X-Achse
      plot.title = element_text(hjust = 0.5)
    )
}

}
