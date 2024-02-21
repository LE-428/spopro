# data_table Tabelle mit Rohdaten, top_x int Wert, ausgegeben werden die Top x Werte

# Details

# > top_artists_time(y2017, 5)
#         artist             playtime
# 679               Sa4      768
# 1               Drake      705
# 363 187 Strassenbande      629
# 342              Gzuz      554
# 208        RAF Camora      324

  top_artists_time <- function(data_table = all_data, top_x){
    
  data_table <- data.frame(master_metadata_album_artist_name = data_table$master_metadata_album_artist_name, ms_played = data_table$ms_played) 
  
  häufigkeiten_artists <- table(data_table$master_metadata_album_artist_name) # Um schnelle Ergebnisse zu bekommen werden nur die Artists mit vielen Streams berücktsichtigt
  sortierte_häufigkeiten <- sort(häufigkeiten_artists, decreasing = TRUE)
  x_häufigsten_artists <- head(sortierte_häufigkeiten, 10 * top_x)
  
  artist_table_unique <- data.frame(artist = attr(x_häufigsten_artists, "names"))
  
  artist_table <- data.frame(artist = data_table$master_metadata_album_artist_name)
  # artist_table_unique <- data.frame(artist = unique(artist_table$artist))
  
  top_artists_table <- data.frame(artist = artist_table_unique$artist)
  
  for(i in 1:length(artist_table_unique$artist)){
    logic <- grep(artist_table_unique$artist[i], artist_table$artist)
    
    one_artist_table <- data.frame(milliseconds = data_table$ms_played[logic])
    
    top_artists_table$playtime[i] = sum(one_artist_table$milliseconds)
    
  }
  
  top_artists_table <- top_artists_table[order(-top_artists_table$playtime),] ### hier minus
  top_artists_table$playtime <- round(top_artists_table$playtime / 60000, digits = 0)
  
  output <- head(top_artists_table, top_x)
  
  return(output)
  
  }
