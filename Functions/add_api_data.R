# Function that adds additional Spotify API data to a table
# track_popularity, artist_popularity, artist_followers, artist_genres




add_api_data <- function(data_frame = all_data, access = access_token, write_to_csv = TRUE, playtime_threshold = 30000) {
  
  # Remove all rows with a runtime of less than 30 seconds
  data_frame <- subset(data_frame, ms_played > playtime_threshold)
  data_frame <- drop_podcasts(data_frame)
  # The table is prepared: columns are renamed, columns are removed, and track IDs are truncated
  
  cols_to_drop <- c(
    "episode_name",
    "episode_show_name",
    "spotify_episode_uri",
    "offline_timestamp",
    "audiobook_chapter_uri",
    "audiobook_chapter_title",
    "audiobook_title",
    "audiobook_uri"
  )
  
  # Keep only those columns that exist in the data frame
  cols_to_drop <- intersect(cols_to_drop, names(data_frame))
  
  names(data_frame)[names(data_frame) == "spotify_track_uri"] <- "id"
  if (length(cols_to_drop) > 0) {
    data_frame <- data_frame[ , !(names(data_frame) %in% cols_to_drop), drop = FALSE]
  }
  # data_frame <- subset(data_frame, !is.na(id), select = -c(
  #                                              #platform, 
  #                                              #username, 
  #                                              #user_agent_decrypted, 
  #                                              episode_name, 
  #                                              episode_show_name, 
  #                                              spotify_episode_uri, 
  #                                              offline_timestamp, 
  #                                              audiobook_chapter_uri,
  #                                              audiobook_chapter_title,
  #                                              audiobook_title,
  #                                              audiobook_uri))
  data_frame$id <- substring(data_frame$id, first = 15)
  # print(head(data_frame))
  # Lengths of the tables with one row per track/artist
  unique_tracks <- unique(data_frame$id)
  len_uniq_tracks <- length(unique_tracks)
  
  # len_uniq_tracks = length(data_frame$id), old version
  # Use one row per artist
  print(nrow(data_frame))
  print(sum(is.na(data_frame$master_metadata_album_artist_name))) 
  print(head(data_frame))
  uniq_artists <- aggregate(. ~ master_metadata_album_artist_name,
                            data = data_frame, FUN = function(x) x[1])
  uniq_artists <- subset(uniq_artists, select = c(master_metadata_album_artist_name, id))
  # print(head(uniq_artists, 25))
  len_uniq_artists = length(uniq_artists$id)
  print(len_uniq_artists)
  # print(length(uniq_artists$id))
  print(head(uniq_artists))
  
  
  # For each artist, the artist ID is determined using one track, and the genres are retrieved via that ID
  
  for (j in seq(1, len_uniq_artists, 50)) {
    # print(j)
    # print(min(j + 49, len_uniq_artists))
    # print(uniq_artists$id[j:min(j + 49, len_uniq_artists)])
    ids_chunk <- uniq_artists$id[j:min(j + 49, len_uniq_artists)]
    # print(length(ids_chunk))
    tracks <- get_tracks(id = ids_chunk, authorization = access_token)
    #print(tracks)
    for (n in seq_along(ids_chunk)) {
      if (length(tracks$artists[[n]]$id[1]) == 0) {
        uniq_artists$artist_id[j + n - 1] <- ""
      } else {
        uniq_artists$artist_id[j + n - 1] <- tracks$artists[[n]]$id[1]
      }
    }
    #print(head(uniq_artists))
    # API request using the newly obtained artist IDs
    artists <- get_artists(id = uniq_artists$artist_id[j:min(j + 49, len_uniq_artists)],
                           authorization = access_token)
    
    for (m in seq_along(ids_chunk)) {
      index <- j + m - 1
      # Artist popularity
      uniq_artists$artist_popularity[index] <- artists$popularity[m]
      # Artist followers
      uniq_artists$artist_followers[index] <- artists$followers.total[m]
      
      # Artist genres
      if (length(artists$genres[[m]]) == 0) {
        uniq_artists$artist_genres[index] <- ""
      } else {
        uniq_artists$artist_genres[index] <- paste(artists$genres[[m]], collapse = ",")
      }
    }
    #print(head(uniq_artists))
    
  }
  artist_ids <- subset(uniq_artists, select = c(artist_id,
                                                master_metadata_album_artist_name,
                                                artist_genres, artist_popularity,
                                                artist_followers))
  output_table <- data.frame()
  
  # Create a new table for track popularities
  metadata_frame <- data.frame(id = character(), popularity = numeric(), release_date = character(),
                               duration_ms = numeric(), stringsAsFactors = FALSE)
  
  # Iterate over all tracks and call the API to retrieve additional information
  
  
  print(len_uniq_tracks)
  
  for(i in seq(1, len_uniq_tracks, 100)){
    
    index_end <- min(i + 99, len_uniq_tracks)
    # ids <- data_frame$id[i:index_end]
    ids <- unique_tracks[i:index_end]  # Now iterate only over unique IDs
    len_ids <- length(ids)
    # print(length(ids))
    
    
    # Retrieve track popularity values between 0 and 100
    
    if (len_ids < 51) {
      track_table <- get_tracks(id = ids[1:len_ids], authorization = access_token)
      #print(track_table)
      metadata <- data.frame(id = ids[1:len_ids], popularity = track_table$popularity,
                             release_date = track_table$album.release_date,
                             duration_ms = track_table$duration_ms)
    } else {
      # print(ids[1:50])
      track_table1 <- get_tracks(id = ids[1:50], authorization = access_token)
      track_table2 <- get_tracks(id = ids[51:len_ids], authorization = access_token)
      #metadata <- c(track_table1$popularity, track_table2$popularity)
      # print(track_table2$popularity)
      
      metadata <- data.frame(
        id = ids,
        popularity = c(track_table1$popularity, track_table2$popularity),
        release_date = c(track_table1$album.release_date, track_table2$album.release_date),
        duration_ms = c(track_table1$duration_ms, track_table2$duration_ms)
      )
    }
    
    #print(metadata)
    
    # Append metadata to the table
    metadata_frame <- rbind(metadata_frame, metadata)
    
    
  } # End of the for loop
  
  # Merge the tables again using matching track IDs and artist names
  
  data_frame <- unique(data_frame)
  #output_table <- unique(output_table)
  metadata_frame <- unique(metadata_frame)
  artist_ids <- unique(artist_ids)
  
  #print(head(data_frame))
  #print(head(popularities_table))
  #print(head(artist_ids))
  
  almighty_table <- merge(data_frame, metadata_frame, by = "id")
  #almighty_table <- merge(data_frame, output_table, by = "id")
  almighty_table <- merge(almighty_table, artist_ids,
                          by = "master_metadata_album_artist_name")
  
  colnames(almighty_table)[colnames(almighty_table) == "id"] <- "spotify_track_uri"
  
  #print(almighty_table)
  if (write_to_csv == TRUE){
    write.csv(almighty_table, "your_extended_dataframe.csv", row.names = FALSE)
  }
  return(almighty_table)
}
