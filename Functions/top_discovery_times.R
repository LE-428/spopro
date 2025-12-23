top_discovery_times <- function(df){
  #' Return one row per  (day, artist, track, total plays) 
  #'                     (day, resulting plays)
  #'                     (month, resulting plays)
  #' with first discovery date/month and play count
  #'
  df <- drop_podcasts(df)
  df$ts <- substr(df$ts, 1, 10)
  df <- subset(df, ms_played > 30000, select = c(ts, master_metadata_album_artist_name, master_metadata_track_name))
  
  # convert to Date so min() behaves correctly
  df$ts <- as.Date(df$ts)
  
  # group by artist+track, count plays and keep earliest date
  result <- df %>%
    group_by(master_metadata_album_artist_name, master_metadata_track_name) %>%
    summarise(
      plays = n(),
      first_ts = min(ts),
      .groups = "drop"
    ) %>%
    # order by first discovery date (oldest first) and tie-breaker by plays (most plays earlier)
    arrange(first_ts, desc(plays)) %>%
    # rename columns to something shorter and return as data.frame
    rename(
      ts = first_ts,
      artist = master_metadata_album_artist_name,
      track = master_metadata_track_name
    ) %>%
    as.data.frame()
  
  print(head(result))
  
  # Now we want to group this table by the discovery days and sum up the plays resulting from the day
  
  discovery_days <- result %>% 
    group_by(ts) %>% 
    summarise(
      resulting_plays = sum(plays),
      .groups = "drop"
    ) %>% 
    # arrange(ts) %>% 
    arrange(desc(resulting_plays)) %>% 
    as.data.frame()
  
  print(head(discovery_days))
  
  # Now let us group this by months
  
  discovery_months <- discovery_days %>%
    mutate(ts = substr(as.character(ts), 1, 7)) %>%
    group_by(ts) %>%
    summarise(
      resulting_plays = sum(resulting_plays),
      .groups = "drop"
    ) %>%
    arrange(desc(resulting_plays)) %>%
    as.data.frame()
  
  print(head(discovery_months))
  
  return(list(
    by_track = result,
    by_day = discovery_days,
    by_month = discovery_months
  ))
  
}
