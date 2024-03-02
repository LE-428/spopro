add_api_data <- function(data_table = all_data, access = access_token) {
  
  # Die Tabelle wird aufbereitet, Spalten umbenannt, Spalten entfernt und die ids der Tracks abgeschnitten
  
  names(data_table)[names(data_table) == "spotify_track_uri"] <- "id"
  data_table <- subset(data_table, select = -c(username, 
                                               platform, 
                                               user_agent_decrypted, 
                                               episode_name, 
                                               episode_show_name, 
                                               spotify_episode_uri, 
                                               skipped))
  data_table$id <- substring(data_table$id, first = 15)
  
  # Längen der Tabellen mit je einer Zeile pro Lied/Künstler
  
  len_uniq_tracks = length(data_table$id)
  uniq_artists <- aggregate(. ~ master_metadata_album_artist_name,
                            data = data_table, FUN = function(x) x[1])
  uniq_artists <- subset(uniq_artists, select = c(master_metadata_album_artist_name, id))
  len_uniq_artists = length(uniq_artists$id)
  # print(length(uniq_artists$id))
  
  
  # Zu jedem Künstler wird mithilfe eines Tracks die Artist ID bestimmt und darüber dann die Genres
  
  for (j in seq(1, len_uniq_artists, 50)) {
    # print(j)
    # print(min(j + 49, len_uniq_artists))
    # print(uniq_artists$id[j:min(j + 49, len_uniq_artists)])
    ids_chunk <- uniq_artists$id[j:min(j + 49, len_uniq_artists)]
    # print(length(ids_chunk))
    tracks <- get_tracks(id = ids_chunk, authorization = access_token)
    
    for (n in seq_along(ids_chunk)) {
      if (length(tracks$artists[[n]]$id[1]) == 0) {
        uniq_artists$artist_id[j + n - 1] <- ""
      } else {
        uniq_artists$artist_id[j + n - 1] <- tracks$artists[[n]]$id[1]
      }
    }
    
    artists <- get_artists(id = uniq_artists$artist_id[j:min(j + 49, len_uniq_artists)],
                           authorization = access_token)
    
    for (m in seq_along(ids_chunk)) {
      index <- j + m - 1
      uniq_artists$artist_popularity[index] <- artists$popularity[m]
      uniq_artists$artist_followers[index] <- artists$followers.total[m]
      
      if (length(artists$genres[[m]]) == 0) {
        uniq_artists$artist_genres[index] <- ""
      } else {
        uniq_artists$artist_genres[index] <- paste(artists$genres[[m]], collapse = ",")
      }
    }
    
  
  }
    artist_ids <- subset(uniq_artists, select = c(artist_id,
                                                master_metadata_album_artist_name,
                                                artist_genres, artist_popularity,
                                                artist_followers))
    output_table <- data.frame()
  
  # Alle Tracks werden durchlaufen und die API aufgerufen und die zusätzlichen Informationen abgerufen
    
  for(i in seq(1, len_uniq_tracks, 100)){
   
    index_end <- min(i + 99, len_uniq_tracks)
    ids <- data_table$id[i:index_end]
    len_ids <- length(ids)
    # print(length(ids))
    
    
    # Die Popularity der Tracks zwischen 0 und 100 wird abgerufen
    
    if (len_ids < 51) {
      track_table <- get_tracks(id = ids[1:len_ids], authorization = access_token)
      popularities <- track_table$popularity
    } else {
      track_table1 <- get_tracks(id = ids[1:50], authorization = access_token)
      track_table2 <- get_tracks(id = ids[51:len_ids], authorization = access_token)
      popularities <- c(track_table1$popularity, track_table2$popularity)
      # print(track_table2$popularity)
    }
    
    
    # Die restlichen Informationen werden abgerufen
    
    api_table <- get_track_audio_features(id = ids, authorization = access_token)
    api_table <- subset(api_table, select = -c(track_href, type, uri, analysis_url))
    api_table$popularity <- popularities
    
    if (nrow(output_table) == 0) {
      output_table <- api_table
    } else {
      output_table <- rbind(output_table, api_table)
    }
    
  }
  
  # Die Tabellen werden wieder zusammengefügt anhand von zusammengehörigen Track ids und Künstlernamen
  data_table <- unique(data_table)
  output_table <- unique(output_table)
  artist_ids <- unique(artist_ids)
  almighty_table <- merge(data_table, output_table, by = "id")
  almighty_table <- merge(almighty_table, artist_ids,
                          by = "master_metadata_album_artist_name")
 
  return(almighty_table)
  
}
