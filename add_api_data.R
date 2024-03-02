add_api_data <- function(data_table = all_data, access = access_token) {
  
  # Die Tabelle wird aufbereitet, Spalten umbenannt, Spalten entfernt und die ids der Tracks abgeschnitten
  names(data_table)[names(data_table) == "spotify_track_uri"] <- "id"
  data_table <- subset(data_table, select = -c(username, platform, user_agent_decrypted, episode_name, episode_show_name, spotify_episode_uri, skipped))
  data_table$id <- substring(data_table$id, first = 15)
  
  # Längen der Tabellen mit je einer Zeile pro Lied/Künstler
  len_uniq_tracks = length(data_table$ts)
  len_uniq_artists = length(unique(data_table$master_metadata_album_artist_name))
  uniq_artists <- aggregate(. ~ master_metadata_album_artist_name, data = data_table, FUN = function(x) x[1])
  uniq_artists <- subset(uniq_artists, select = c(master_metadata_album_artist_name, id))
  
  #return(uniq_artists)
  # Zu jedem Künstler wird mithilfe eines Tracks die Artist ID bestimmt und darüber dann die Genres
  for (j in seq(1, len_uniq_artists)) {
    track <- get_track(id = uniq_artists$id[j], authorization = access_token)
    artist_id <- track$artists$id[1]
    uniq_artists$artist_id[j] <- artist_id
    artist <- get_artist(id = artist_id, authorization = access_token)
    if (length(artist$genres) == 0) {
      uniq_artists$artist_genres[j] <- ""
    } else {
      uniq_artists$artist_genres[j] <- paste(artist$genres, collapse = ",")
    }
    uniq_artists$artist_popularity[j] <- artist$popularity
  }
  
  artist_ids <- subset(uniq_artists, select = c(artist_id, master_metadata_album_artist_name, artist_genres, artist_popularity))
  output_table <- data.frame()
  
  # Alle Tracks werden durchlaufen und die API aufgerufen und die zusätzlichen Informationen abgerufen
  for(i in seq(1, len_uniq_tracks, 100)){
    # print(i)
    if (i == len_uniq_tracks - (len_uniq_tracks %% 100) + 1) {
      id_index = (len_uniq_tracks %% 100)
      index_end = len_uniq_tracks
    } else {
      id_index = 100
      index_end = i + 99
    }
    # print(id_index)
    
    ids <- data_table$id[i:index_end]
    
    # Die Popularity der Tracks zwischen 0 und 100 wird abgerufen
    if (id_index < 51) {
      track_table <- get_tracks(id = ids[1:id_index], authorization = access_token)
      popularities <- track_table$popularity
    } else {
      track_table1 <- get_tracks(id = ids[1:50], authorization = access_token)
      track_table2 <- get_tracks(id = ids[51:id_index], authorization = access_token)
      popularities <- c(track_table1$popularity, track_table2$popularity)
      # print(track_table2$popularity)
    }
    
    # Die restlichen Informationen werden abgerufen
    api_table <- get_track_audio_features(id = ids, authorization = access_token)
    api_table <- subset(api_table, select = -c(track_href, type, uri, analysis_url))
    api_table$popularity <- popularities
    # test_table <- get_track(ids[1], authorization = access_token)
    
    if (nrow(output_table) == 0) {
      output_table <- api_table
    } else {
      output_table <- rbind(output_table, api_table)
    }
    
  }
  # Die Tabellen werden wieder zusammengefügt anhand von zusammengehörigen Track ids und Künstlernamen
  almighty_table <- merge(data_table, output_table, by = "id")
  almighty_table <- merge(almighty_table, artist_ids, by = "master_metadata_album_artist_name")
  return(almighty_table)
}
