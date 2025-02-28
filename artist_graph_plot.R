{
library(stringr)
library(igraph)
library(ggraph)
library(tidygraph)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

artist_graph_plot <- function(data_frame, top_x = 100, extrapolate_genres = FALSE, save_plot = FALSE) {
  data_frame <- get_first_streams(data_frame)
  feature_table <- subset(data_frame, data_frame$ms_played > 10000, 
                          select = c(master_metadata_track_name, master_metadata_album_artist_name))
  
  # Extrahiere die Top-Genres
  top_20_genres <- top_genres(data_frame, top_x = 30)
  top_genres <- top_20_genres$Genre
  print(top_genres)
  palette <- brewer.pal(12, "Paired")  # Wir nutzen eine Palette mit bis zu 12 Farben
  additional_colors <- colorRampPalette(c("cyan", "red", "blue", "green", "yellow", "purple", "orange", "magenta", "coral", "darkred", "royalblue", "plum", "darkcyan", "black", "khaki", "wheat", "darkolivegreen1", "aquamarine"))(18)  # 8 zusätzliche Farben
  palette <- c(palette, additional_colors)
  
  genre_color_map <- setNames(palette, top_genres)
  print(genre_color_map)
  
  # Funktion, um eine Farbe basierend auf Genre zuzuweisen
  get_artist_color <- function(genres) {
    genre_list <- strsplit(genres, ",")[[1]]
    for (genre in genre_list) {
      genre <- trimws(genre)
      if (genre %in% names(genre_color_map)) {
        return(genre)  # Farbe aus der Zuordnung zurückgeben
      }
    }
    return("")  # Standardfarbe für Genres, die nicht in den Top-Genres sind
  }
  
  artist_genres_df <- subset(data_frame,
                             select = c(master_metadata_album_artist_name, artist_genres))
  artist_genres_df <- artist_genres_df %>% 
    rename(
      Artist = master_metadata_album_artist_name
    ) %>% 
    mutate(
      artist_genres = ifelse(is.na(artist_genres), "", as.character(artist_genres))
    ) %>% 
    group_by(Artist) %>% 
    summarise(
     top_genre = get_artist_color(artist_genres) 
    ) %>% 
    ungroup()
  
  print(head(artist_genres_df, n = 15))
  
  
  matches <- feature_table$master_metadata_track_name |>
    str_extract_all("\\[(.*?)\\]|\\((.*?)\\)") |>
    unlist() |>
    str_extract_all("(?<=feat\\.\\s|WITH\\s|ft\\.\\s)(?=[^\\)\\]]*)(.*?)(?=[\\)\\]])") |>
    unlist()
  
  print(head(matches))
  print(nrow(feature_table))
  print(length(matches))
  
  split_matches <- str_split(matches, "(\\s&\\swith\\s|\\s&\\sfeat.\\s|,\\sand\\s|,\\s|\\s&\\s|\\sand\\s|\\sfeat\\.\\s)", simplify = TRUE)
  non_empty_matches <- split_matches[split_matches != ""]

  top_features <- sort(table(non_empty_matches), decreasing = TRUE)
  top_x_features <- head(top_features, n = top_x)
  top_artists <- names(top_x_features)
  
  print(head(top_artists, n = 10))
  
  
  # Ergänze artist_genres_df um alle fehlenden Künstler aus top_artists
  missing_artists <- setdiff(top_artists, artist_genres_df$Artist)
  if(length(missing_artists) > 0) {
    missing_df <- data.frame(Artist = missing_artists, top_genre = "", stringsAsFactors = FALSE)
    artist_genres_df <- rbind(artist_genres_df, missing_df)
  }

  # Adjazenzliste als Edge-Liste
  edges <- list()
  for (i in seq_len(nrow(feature_table))) {
    main_artist <- feature_table$master_metadata_album_artist_name[i]
    track <- feature_table$master_metadata_track_name[i]
    
    # Features nur aus dem aktuellen Track extrahieren
    track_features <- str_extract_all(track, "\\[(.*?)\\]|\\((.*?)\\)") |>
      unlist() |>
      str_extract_all("(?<=feat\\.\\s|WITH\\s|ft\\.\\s)(?=[^\\)\\]]*)(.*?)(?=[\\)\\]])") |>
      unlist()
    
    if (length(track_features) > 0) {
      seperated_features  <- str_split(track_features, "(\\s&\\swith\\s|\\s&\\sfeat.\\s|,\\sand\\s|,\\s|\\s&\\s|\\sand\\s|\\sfeat\\.\\s)", simplify = TRUE)
      artists <- unique(c(main_artist, seperated_features))
    } else{
      artists <- c(main_artist)
    }
    # print(artists)
    
    # Falls kein Feature-Artist gefunden wurde, bleibt nur der Main-Artist
    artists <- artists[artists %in% top_artists]
    
    if (length(artists) > 1) {
      combs <- combn(artists, 2, simplify = FALSE)
      for (pair in combs) {
        key <- paste(sort(pair), collapse = " - ")
        edges[[key]] <- edges[[key]] + 1L
      }
    }
  }

  # Dataframe für Graph erstellen
  edge_df <- data.frame(
    from = sub(" - .*", "", names(edges)),
    to = sub(".* - ", "", names(edges)),
    weight = as.integer(edges)
  )
  
  print(head(edge_df))
  print(head(artist_genres_df))
  print(head(top_artists))
  
  
  #### Die Künstler unter den top_artists, welche noch kein Genre haben, durch Betrachten der Nachbarn und deren Genres, verarbeiten
  
  if (extrapolate_genres == TRUE){
    
    # Setze den Schwellenwert für die minimale Anzahl an Nachbarn
    min_neighbors <- 7
    
    # Erstelle eine Kopie der Tabelle, in der wir die Genres aktualisieren
    artist_genres_updated <- artist_genres_df
    
    # Hilfsfunktion: Bestimme für einen Künstler das Mehrheitsgenre seiner Nachbarn
    get_majority_genre <- function(artist, edge_df, genres_df) {
      # Finde alle Kanten, in denen der Künstler vorkommt
      rel_edges <- edge_df[edge_df$from == artist | edge_df$to == artist, ]
      
      if(nrow(rel_edges) == 0) return(NA)
      
      # Bestimme die Nachbarn: wenn artist in "from", dann der "to", andernfalls "from"
      neighbors <- unique(c(rel_edges$to[rel_edges$from == artist],
                            rel_edges$from[rel_edges$to == artist]))
      
      # Hole für jeden Nachbarn das top_genre aus genres_df
      neighbor_genres <- sapply(neighbors, function(nb) {
        g <- genres_df$top_genre[genres_df$Artist == nb]
        if(length(g) > 0 && g != "") g else NA
      })
      neighbor_genres <- neighbor_genres[!is.na(neighbor_genres)]
      
      if(length(neighbor_genres) < min_neighbors) {
        return(NA)  # Nicht genug informative Nachbarn
      } else {
        # Wähle das am häufigsten vorkommende Genre
        maj_genre <- names(sort(table(neighbor_genres), decreasing = TRUE))[1]
        return(maj_genre)
      }
    }
    
    # Iterativer Prozess
    iteration <- 1
    repeat {
      cat("Iteration:", iteration, "\n")
      new_assignments <- 0
      
      # Erhalte die Liste der Künstler, bei denen top_genre noch leer ist
      missing_idx <- which(artist_genres_updated$top_genre == "")
      if(length(missing_idx) == 0) break  # Alle Künstler haben ein Genre
      
      # Für jeden fehlenden Künstler: versuche, das Genre zu konstruieren
      for(i in missing_idx) {
        artist <- artist_genres_updated$Artist[i]
        maj_genre <- get_majority_genre(artist, edge_df, artist_genres_updated)
        if(!is.na(maj_genre) && maj_genre != "") {
          artist_genres_updated$top_genre[i] <- maj_genre
          new_assignments <- new_assignments + 1
        }
      }
      
      cat("Neue Zuordnungen in dieser Iteration:", new_assignments, "\n")
      
      if(new_assignments == 0) break  # Keine Verbesserung mehr
      iteration <- iteration + 1
    }
    
  } else{
    artist_genres_updated <- artist_genres_df
  }
  
  # Zum Schluss: Aktualisiere artist_genres_df (z.B. per Merge) oder verwende artist_genres_updated weiter
  #print(artist_genres_updated)
  
  ##############

  # Graph mit igraph erstellen
  g <- graph_from_data_frame(edge_df, directed = FALSE)

  # Zeichne das Netzwerk mit ggraph
    # ggraph(g, layout = "fr") +
    # geom_edge_link(aes(width = weight / 10, colour = "white"), alpha = 0.0) +
    # geom_node_point(size = 2 * log(degree(g) + 1), color = "blue") +
    # geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    # theme_minimal()
  
  # Füge das Attribut für die Farben hinzu
  # V(g)$color <- artist_genres_df$top_genre[match(V(g)$name, artist_genres_df$Artist)]
  
  # Manuelle Zuordnung der Farben für Genres
  # genre_color_map_manual <- setNames(genre_color_map, names(genre_color_map))
  
  
  # Statischer Graph mit Kanten
  # p <- ggraph(g, layout = 'fr') + 
  #   geom_edge_parallel(alpha = 0.2, color = "black") +  # Kanten ausblenden, aber als Arc darstellen
  #   geom_node_point(aes(color = color), size = 2 * log(degree(g) + 1)) +  # Knoten anpassen
  #   geom_node_text(aes(label = name), repel = TRUE, size = 3, color = "black") +  # Beschriftungen
  #   scale_fill_identity() +  # Setzt direkte Farbwerte, ohne eine Skala zu definieren
  #   scale_color_manual(values = genre_color_map,
  #                      guide = guide_legend(title = "Genres", override.aes = list(size = 5))) +  # Legende für Genres
  #   coord_fixed() +  # Feste Koordinaten für bessere Visualisierung
  #   theme_minimal()
  
  # Funktioniert mit plotly, aber Kanten fehlen
  # p <- ggraph(g, layout = 'fr') + 
  #   geom_edge_link(alpha = 1.0, color = "black") +  # Kanten ausblenden, aber als Arc darstellen
  #   geom_node_point(aes(color = color), size = 2 * log(degree(g) + 1)) +  # Knoten anpassen
  #   geom_node_text(aes(label = name), repel = FALSE, size = 3, color = "black") +  # Beschriftungen
  #   scale_fill_identity() +  # Setzt direkte Farbwerte, ohne eine Skala zu definieren
  #   scale_color_manual(values = genre_color_map,
  #                      guide = guide_legend(title = "Genres", override.aes = list(size = 5))) +  # Legende für Genres
  #   coord_fixed() +  # Feste Koordinaten für bessere Visualisierung
  #   theme_minimal()
  
  # Zeichne das Netzwerk mit ggraph
  layout <- layout_with_fr(g)
  
  # Erstelle eine DataFrame für Knotenpositionen
  node_df <- data.frame(
    name = V(g)$name,
    x = layout[,1],
    y = layout[,2]
  )
  
  # Erstelle eine DataFrame für Kanten
  edge_df <- as.data.frame(as_edgelist(g))
  colnames(edge_df) <- c("from", "to")
  
  # Füge die Farben der Knoten hinzu
  node_df$color <- artist_genres_updated$top_genre[match(node_df$name, artist_genres_updated$Artist)]
  
  # Berechne den Grad jedes Knotens
  node_df$Degree <- degree(g)
  
  # Plotly Visualisierung
  p <- ggplot() +
    geom_segment(data = edge_df,
                 aes(x = node_df$x[match(from, node_df$name)], 
                     y = node_df$y[match(from, node_df$name)],
                     xend = node_df$x[match(to, node_df$name)],
                     yend = node_df$y[match(to, node_df$name)]),
                 color = "gray", alpha = 0.5) +
    geom_point(data = node_df, aes(x = x, y = y, color = color, size = Degree)) +
    geom_text(data = node_df, aes(x = x, y = y, label = name), color = "black", size = 2.5, vjust = -1) +
    scale_color_manual(values = genre_color_map,
                       guide = guide_legend(title = "Genres", override.aes = list(size = 5))) +  # Legende für Genres
    scale_size_continuous(range = c(2, 5)) +  # Skaliere die Punktgröße
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  
  # Exportiere den ggplot als hochauflösendes PDF
  if (save_plot == TRUE) {
    ggsave(
      filename = "artist_graph.pdf",  # Dateiname
      plot = p,                         # Dein ggplot2-Objekt
      device = pdf,               # Verwende cairo für bessere Text-Darstellung
      width = 12,                       # Breite in Zoll
      height = 8,                       # Höhe in Zoll
      dpi = 300                         # Auflösung (dots per inch)
    )
  }
  
  return(p)
}

}
