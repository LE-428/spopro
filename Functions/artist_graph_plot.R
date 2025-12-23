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
    
    # Extract the top genres
    top_20_genres <- top_genres(data_frame, top_x = 30)
    top_genres <- top_20_genres$Genre
    print(top_genres)
    palette <- brewer.pal(12, "Paired")  # We use a palette with up to 12 colors
    additional_colors <- colorRampPalette(c("cyan", "red", "blue", "green", "yellow", "purple", "orange", "magenta", "coral", "darkred", "royalblue", "plum", "darkcyan", "black", "khaki", "wheat", "darkolivegreen1", "aquamarine"))(18)  # 8 additional colors
    palette <- c(palette, additional_colors)
    
    genre_color_map <- setNames(palette, top_genres)
    print(genre_color_map)
    
    # Function to assign a color based on genre
    get_artist_color <- function(genres) {
      genre_list <- strsplit(genres, ",")[[1]]
      for (genre in genre_list) {
        genre <- trimws(genre)
        if (genre %in% names(genre_color_map)) {
          return(genre)  # Return the color from the mapping
        }
      }
      return("")  # Default color for genres not in the top genres
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
    
    # Add all missing artists from top_artists to artist_genres_df
    missing_artists <- setdiff(top_artists, artist_genres_df$Artist)
    if(length(missing_artists) > 0) {
      missing_df <- data.frame(Artist = missing_artists, top_genre = "", stringsAsFactors = FALSE)
      artist_genres_df <- rbind(artist_genres_df, missing_df)
    }
    
    # Adjacency list as edge list
    edges <- list()
    for (i in seq_len(nrow(feature_table))) {
      main_artist <- feature_table$master_metadata_album_artist_name[i]
      track <- feature_table$master_metadata_track_name[i]
      
      # Extract features only from the current track
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
      
      # Keep only artists that are in top_artists
      artists <- artists[artists %in% top_artists]
      
      if (length(artists) > 1) {
        combs <- combn(artists, 2, simplify = FALSE)
        for (pair in combs) {
          key <- paste(sort(pair), collapse = " - ")
          edges[[key]] <- edges[[key]] + 1L
        }
      }
    }
    
    # Create dataframe for graph
    edge_df <- data.frame(
      from = sub(" - .*", "", names(edges)),
      to = sub(".* - ", "", names(edges)),
      weight = as.integer(edges)
    )
    
    print(head(edge_df))
    print(head(artist_genres_df))
    print(head(top_artists))
    
    #### Process top_artists without genre by considering neighbors and their genres
    if (extrapolate_genres == TRUE){
      
      # Set threshold for minimum number of neighbors
      min_neighbors <- 7
      
      # Create a copy of the table to update genres
      artist_genres_updated <- artist_genres_df
      
      # Helper function: determine the majority genre of an artist's neighbors
      get_majority_genre <- function(artist, edge_df, genres_df) {
        # Find all edges where the artist appears
        rel_edges <- edge_df[edge_df$from == artist | edge_df$to == artist, ]
        
        if(nrow(rel_edges) == 0) return(NA)
        
        # Determine neighbors: if artist is in "from", then take "to", else "from"
        neighbors <- unique(c(rel_edges$to[rel_edges$from == artist],
                              rel_edges$from[rel_edges$to == artist]))
        
        # Get top_genre for each neighbor from genres_df
        neighbor_genres <- sapply(neighbors, function(nb) {
          g <- genres_df$top_genre[genres_df$Artist == nb]
          if(length(g) > 0 && g != "") g else NA
        })
        neighbor_genres <- neighbor_genres[!is.na(neighbor_genres)]
        
        if(length(neighbor_genres) < min_neighbors) {
          return(NA)  # Not enough informative neighbors
        } else {
          # Choose the most frequent genre
          maj_genre <- names(sort(table(neighbor_genres), decreasing = TRUE))[1]
          return(maj_genre)
        }
      }
      
      # Iterative process
      iteration <- 1
      repeat {
        cat("Iteration:", iteration, "\n")
        new_assignments <- 0
        
        # Get list of artists with empty top_genre
        missing_idx <- which(artist_genres_updated$top_genre == "")
        if(length(missing_idx) == 0) break  # All artists have a genre
        
        # For each missing artist: try to infer the genre
        for(i in missing_idx) {
          artist <- artist_genres_updated$Artist[i]
          maj_genre <- get_majority_genre(artist, edge_df, artist_genres_updated)
          if(!is.na(maj_genre) && maj_genre != "") {
            artist_genres_updated$top_genre[i] <- maj_genre
            new_assignments <- new_assignments + 1
          }
        }
        
        cat("New assignments in this iteration:", new_assignments, "\n")
        
        if(new_assignments == 0) break  # No more improvement
        iteration <- iteration + 1
      }
      
    } else{
      artist_genres_updated <- artist_genres_df
    }
    
    # Finally: update artist_genres_df (e.g., via merge) or continue using artist_genres_updated
    #print(artist_genres_updated)
    
    ##############
    
    # Create graph with igraph
    g <- graph_from_data_frame(edge_df, directed = FALSE)
    
    # Draw the network with ggraph
    # Draw the network with ggraph
    layout <- layout_with_fr(g)
    
    # Create dataframe for node positions
    node_df <- data.frame(
      name = V(g)$name,
      x = layout[,1],
      y = layout[,2]
    )
    
    # Create dataframe for edges
    edge_df <- as.data.frame(as_edgelist(g))
    colnames(edge_df) <- c("from", "to")
    
    # Add node colors
    node_df$color <- artist_genres_updated$top_genre[match(node_df$name, artist_genres_updated$Artist)]
    
    # Compute node degree
    node_df$Degree <- degree(g)
    
    # Plotly visualization
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
                         guide = guide_legend(title = "Genres", override.aes = list(size = 5))) +  # Legend for genres
      scale_size_continuous(range = c(2, 5)) +  # Scale point size
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
    
    # Export ggplot as high-resolution PDF
    if (save_plot == TRUE) {
      ggsave(
        filename = "artist_graph.pdf",  # Filename
        plot = p,                         # Your ggplot2 object
        device = pdf,               # Use cairo for better text rendering
        width = 12,                       # Width in inches
        height = 8,                       # Height in inches
        dpi = 300                         # Resolution (dots per inch)
      )
    }
    
    return(p)
  }
  
}
