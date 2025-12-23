# Distribution of the different albums of an artist

# > artist_albums(all_le, "RIN", exact = TRUE)
# Album Plays
# 1      Kleinstadt   277
# 2      Nimmerland   243
# 3     EROS (2018)   182
# 4 Planet Megatron   169
# 5     Boys Do Cry    58

artist_albums <- function(data_frame, artist_string, exact = FALSE, top_x = 5){
  data_frame <- subset(data_frame, ms_played > 30000, select = c(master_metadata_album_album_name, master_metadata_album_artist_name)) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s
  artist_string <- gsub("([()])", "\\\\\\1", artist_string)
  artist_string <- gsub("\\$", "\\\\\\$", artist_string)
  
  # Search for the given song, exact search possible
  if (exact == TRUE) {
    aux_table <- data_frame[(which(grepl(paste0("^", artist_string, "$"), data_frame$master_metadata_album_artist_name, ignore.case=TRUE))),]
  } else if (exact == FALSE) {
    aux_table <- data_frame[(which(grepl(artist_string, data_frame$master_metadata_album_artist_name, ignore.case=TRUE))),]
  }  
   
  # print(head(aux_table))
 
  aux_table  <- aux_table  %>% 
    rename(
      Album = master_metadata_album_album_name,
      Artist = master_metadata_album_artist_name
    ) %>% 
    group_by(Album) %>% 
    summarise(
      Plays = n()
    ) %>% 
    ungroup() %>% 
    arrange(desc(Plays)) %>% 
    as.data.frame() %>% 
    slice_head(n = top_x)
  print(aux_table)
}
