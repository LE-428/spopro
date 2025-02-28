# Funktion, welche einer Tabelle zusätzliche Daten der Spotify-API hinzufügt
# track_popularity, artist_populartiy, artist_followers, artist_genres




add_api_data <- function(data_frame = all_data, access = access_token, write_to_csv = TRUE, playtime_threshold = 30000) {
  
  # Alle Zeilen mit weniger als 30s Laufzeit entfernen
  data_frame <- subset(data_frame, ms_played > playtime_threshold)
  data_frame <- drop_podcasts(data_frame)
  # Die Tabelle wird aufbereitet, Spalten umbenannt, Spalten entfernt und die ids der Tracks abgeschnitten
  
  names(data_frame)[names(data_frame) == "spotify_track_uri"] <- "id"
  data_frame <- subset(data_frame, !is.na(id), select = -c(
                                               #platform, 
                                               #username, 
                                               #user_agent_decrypted, 
                                               episode_name, 
                                               episode_show_name, 
                                               spotify_episode_uri, 
                                               offline_timestamp))
  data_frame$id <- substring(data_frame$id, first = 15)
  # print(head(data_frame))
  # Längen der Tabellen mit je einer Zeile pro Lied/Künstler
  unique_tracks <- unique(data_frame$id)
  len_uniq_tracks <- length(unique_tracks)
  
  # len_uniq_tracks = length(data_frame$id), alte Version
  # Jeden Künstler mit einer Zeile verwenden
  uniq_artists <- aggregate(. ~ master_metadata_album_artist_name,
                            data = data_frame, FUN = function(x) x[1])
  uniq_artists <- subset(uniq_artists, select = c(master_metadata_album_artist_name, id))
  # print(head(uniq_artists, 25))
  len_uniq_artists = length(uniq_artists$id)
  print(len_uniq_artists)
  # print(length(uniq_artists$id))
  print(head(uniq_artists))
  
  
  # Zu jedem Künstler wird mithilfe eines Tracks die Artist ID bestimmt und darüber dann die Genres
  
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
    # API-Anfrage über die gerade erhaltenen Artist IDs
    artists <- get_artists(id = uniq_artists$artist_id[j:min(j + 49, len_uniq_artists)],
                           authorization = access_token)
    
    for (m in seq_along(ids_chunk)) {
      index <- j + m - 1
      # Artist Popularity
      uniq_artists$artist_popularity[index] <- artists$popularity[m]
      # Artist Followers
      uniq_artists$artist_followers[index] <- artists$followers.total[m]
      
      # Artist Genres
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
  
  # Neue Tabelle für Popularities erstellen
  metadata_frame <- data.frame(id = character(), popularity = numeric(), release_date = character(),
                               duration_ms = numeric(), stringsAsFactors = FALSE)
  
  # Alle Tracks werden durchlaufen und die API aufgerufen und die zusätzlichen Informationen abgerufen
    
  
  print(len_uniq_tracks)
  
  for(i in seq(1, len_uniq_tracks, 100)){
   
    index_end <- min(i + 99, len_uniq_tracks)
    # ids <- data_frame$id[i:index_end]
    ids <- unique_tracks[i:index_end]  # Jetzt nur noch über einzigartige IDs
    len_ids <- length(ids)
    # print(length(ids))
    
    
    # Die Popularity der Tracks zwischen 0 und 100 wird abgerufen
    
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
    
    # metadata zur Tabelle hinzufügen
    metadata_frame <- rbind(metadata_frame, metadata)
    
    
  } # Ende der for-Schleife
  
  # Die Tabellen werden wieder zusammengefügt anhand von zusammengehörigen Track ids und Künstlernamen
    
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
