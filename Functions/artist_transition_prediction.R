{
library(dplyr)
library(lubridate)
}

# Return the artists, which appeared after each other most consistently during
# listening sessions. Count every combination of distinct tracks from source artist 
# (first listened) and target artist (second listened) only once, so we remove the influence of 
# playlists (same song/artist transition over and over again)

# Example:
# source         target     percentage n_unique_pairs total_unique_outgoing
# 1 Monk           BHZ         26.8         15                    56
# 26.8 % of all transitions from a song by Monk leaded to a stream of BHZ next
# If this value is high, this means that the two artists are linked strongly by the listener

artist_transition_prediction <- function(df, top_x=10,
                                                 min_source_count = 30,
                                                 exclude_self_loops = TRUE,
                                                 time_threshold_mins = 30,
                                                 drop_colums = TRUE) {
  # Ensure timestamp column exists and is POSIXct
  if (!"ts" %in% names(df)) stop("Column 'ts' is missing.")
  if (!inherits(df$ts, "POSIXct")) df$ts <- lubridate::as_datetime(df$ts)
  
  # Basic cleaning and ordering
  df_clean <- df %>%
    dplyr::filter(!is.na(master_metadata_album_artist_name)) %>%
    dplyr::arrange(ts)
  
  # Create session_global if not present
  if (!"session_global" %in% names(df_clean)) {
    if (!"ip_addr" %in% names(df_clean)) df_clean$ip_addr <- "user_1"
    
    df_clean <- df_clean %>%
      dplyr::arrange(ip_addr, ts) %>%
      dplyr::group_by(ip_addr) %>%
      dplyr::mutate(
        # compute time difference to previous play in minutes
        time_diff = as.numeric(difftime(ts, dplyr::lag(ts), units = "mins")),
        # new session if first row or gap larger than threshold
        new_session = is.na(time_diff) | (time_diff > time_threshold_mins),
        # numeric session id by cumulative sum of new_session flags
        session_id = cumsum(as.integer(new_session))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(session_global = paste0(ip_addr, "_", session_id)) %>%
      # explicitly call dplyr::select to remove intermediate columns safely
      dplyr::select(-time_diff, -new_session, -session_id)
  }
  
  # Build transitions at track level: source artist + source track -> target artist + target track
  transitions <- df_clean %>%
    dplyr::arrange(session_global, ts) %>%
    dplyr::group_by(session_global) %>%
    dplyr::mutate(
      source_artist = master_metadata_album_artist_name,
      source_track  = dplyr::coalesce(spotify_track_uri, master_metadata_track_name),
      target_artist = dplyr::lead(master_metadata_album_artist_name),
      target_track  = dplyr::coalesce(dplyr::lead(spotify_track_uri), dplyr::lead(master_metadata_track_name))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(target_artist), !is.na(target_track), !is.na(source_track))
  
  # Optionally exclude artist self-loops
  if (exclude_self_loops) {
    transitions <- transitions %>% dplyr::filter(source_artist != target_artist)
  }
  
  # Deduplicate globally so each unique (source_artist, source_track, target_artist, target_track)
  # is counted at most once in the whole dataset
  unique_pairs <- transitions %>%
    dplyr::distinct(source_artist, source_track, target_artist, target_track)
  
  # Count number of unique track-pairs from source -> target (numerator)
  transition_counts <- unique_pairs %>%
    dplyr::count(source = source_artist, target = target_artist, name = "n_unique_pairs")
  
  # Count total unique outgoing pairs per source artist (denominator)
  source_totals <- unique_pairs %>%
    dplyr::count(source = source_artist, name = "total_unique_outgoing")
  
  # Keep only sources with at least min_source_count unique outgoing pairs
  valid_sources <- source_totals %>%
    dplyr::filter(total_unique_outgoing >= min_source_count)
  
  # Build result table and compute probability
  result_table <- transition_counts %>%
    dplyr::inner_join(valid_sources, by = "source") %>%
    dplyr::mutate(
      probability = n_unique_pairs / total_unique_outgoing,
      ratio  = round(probability, 2)
    ) %>%
    dplyr::arrange(dplyr::desc(probability)) %>%
    dplyr::select(source, target, ratio, n_unique_pairs, total_unique_outgoing) %>% 
    slice_head(n=top_x)
  
  if (drop_colums) {
    result_table <- result_table %>% 
      dplyr::select(source, target, ratio)
  }
  
  # This line is just for debugging purposes (shows all distinct combinations for first row)
  # unique_pairs %>% filter(source_artist == result_table$source[1], target_artist == result_table$target[1]) %>% dplyr::select(source_artist, source_track, target_artist, target_track) %>% arrange(source_track, target_track) %>% print(n = Inf) # Debug: show all unique track-pairs for top source->target

  
  return(result_table)
}


