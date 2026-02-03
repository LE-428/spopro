library(dplyr)
library(lubridate)
library(tidyr)
library(igraph)
library(ggraph)


# This plot shows the flow between artists while listening
# Listening to a song by Artist A, then a song by Artist B contributes once to the transition between Artist A and B

artist_transitions_plot <- function(df, top_n_artists = 50, top_n_edges = 40) {
  
  # 1. Prepare Data and Ensure Chronological Order
  # Handling the timestamp format explicitly based on your sample
  if ("ts" %in% names(df)) {
    # If ts is character/factor, convert it. If already POSIXct, this is harmless.
    if (!is.POSIXct(df$ts)) {
      df$ts <- lubridate::as_datetime(df$ts)
    }
  } else {
    stop("Column 'ts' is missing in the dataframe.")
  }
  
  # Explicitly use dplyr::arrange
  df <- df %>% dplyr::arrange(ts)
  
  # 2. Check and Recreate 'session_global' if Missing
  if (!"session_global" %in% names(df)) {
    
    # Check for ip_addr based on your sample row
    if (!"ip_addr" %in% names(df)) {
      df$ip_addr <- "user_1"
    }
    
    # Sessionization logic
    # Using dplyr:: functions explicitly to avoid masking errors
    df <- df %>%
      dplyr::arrange(ip_addr, ts) %>%
      dplyr::group_by(ip_addr) %>%
      dplyr::mutate(
        prev_ts = dplyr::lag(ts),
        # Calculate time difference in minutes
        time_diff = as.numeric(difftime(ts, prev_ts, units = "mins")),
        # Logic: New session if gap is NA (start) or > 30 mins
        new_session = is.na(time_diff) | time_diff > 30,
        session_id = cumsum(new_session)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(session_global = paste0(ip_addr, "_", session_id)) %>%
      # FIX: Use dplyr::select to ensure correct function is called
      dplyr::select(-prev_ts, -time_diff, -new_session, -session_id) %>% 
      dplyr::arrange(ts)
  }
  
  # 3. Filter Relevant Columns
  # Using your column name: master_metadata_album_artist_name
  artist_df <- df %>%
    dplyr::filter(!is.na(master_metadata_album_artist_name), !is.na(session_global)) %>%
    dplyr::select(ts, session_global, master_metadata_album_artist_name)
  
  # 4. Identify Top Artists (Global Filter)
  top_artists_list <- artist_df %>%
    dplyr::count(master_metadata_album_artist_name, sort = TRUE) %>%
    dplyr::slice_head(n = top_n_artists) %>%
    dplyr::pull(master_metadata_album_artist_name)
  
  # 5. Generate Transitions (Bigrams)
  transitions <- artist_df %>%
    dplyr::arrange(session_global, ts) %>%
    dplyr::group_by(session_global) %>%
    dplyr::mutate(
      source = master_metadata_album_artist_name,
      target = dplyr::lead(master_metadata_album_artist_name) # Vectorized look-ahead
    ) %>%
    dplyr::filter(
      !is.na(target), 
      source %in% top_artists_list,
      target %in% top_artists_list,
      source != target 
    ) %>%
    dplyr::ungroup()
  
  # 6. Count Frequencies and Select Top Edges
  edge_list <- transitions %>%
    dplyr::count(source, target, name = "weight", sort = TRUE) %>%
    dplyr::slice_head(n = top_n_edges)
  
  # Console Output
  cat(sprintf("Total transitions analyzed (top artists, no self-loops): %d\n", nrow(transitions)))
  
  if (nrow(edge_list) == 0) {
    stop("No transitions found. Check if 'master_metadata_album_artist_name' contains valid data or reduce filtering criteria.")
  }
  
  # 7. Create Graph Object
  g <- graph_from_data_frame(edge_list, directed = TRUE)
  
  # Add degree centrality for node size
  V(g)$degree <- degree(g, mode = "all")
  
  # 8. Visualization
  plot <- ggraph(g, layout = 'linear', circular = TRUE) + 
    geom_edge_arc(aes(width = weight, alpha = weight), 
                  arrow = arrow(length = unit(4, 'mm'), type = "closed"), 
                  start_cap = circle(5, 'mm'),
                  end_cap = circle(5, 'mm'),
                  color = "blue",
                  strength = 1.2) + 
    scale_edge_width(range = c(0.5, 4)) +
    scale_edge_alpha(range = c(0.4, 1)) +
    geom_node_point(aes(size = degree), color = "lightgreen") +
    geom_node_text(aes(label = name), 
                   repel = TRUE, 
                   point.padding = unit(0.2, "lines"), 
                   size = 4, 
                   fontface = "bold") +
    theme_void() +
    labs(title = paste("Listening Flow: Top", top_n_edges, "Artist Transitions"),
         subtitle = "Excluding self-loops; Arcs show direction") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "none"
    )
  
  return(plot)
}
