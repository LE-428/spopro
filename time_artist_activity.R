# Find the top artist during each hour, late night Frank Ocean sessions?

# > time_artist_activity(extract_year(2023, all_le))
# [1] hour                              master_metadata_album_artist_name track_name                       
# <0 Zeilen> (oder row.names mit Länge 0)
# # A tibble: 24 × 3
# # Groups:   hour [24]
# hour master_metadata_album_artist_name     n
# <int> <chr>                             <int>
#   1     0 RIN                                  39
# 2     1 RIN                                  22
# 3     2 Pashanim                              7
# 4     3 MAJAN                                10
# 5     4 01099                                 3
# 6     5 Frank Ocean                           6
# 7     6 RIN                                  22
# 8     7 Brent Faiyaz                          6
# 9     8 Pashanim                              9
# 10     9 Playboi Carti                        20
# 11    10 RIN                                  39
# 12    11 RIN                                  10
# 13    12 RIN                                  18
# 14    13 Die drei ???                         37
# 15    14 Metro Boomin                         27
# 16    15 Die drei ???                         16
# 17    16 Die drei ???                         24
# 18    17 Yung Hurn                            26
# 19    18 RIN                                  45
# 20    19 RIN                                  61
# 21    20 RIN                                  69
# 22    21 RIN                                  56
# 23    22 RIN                                  75
# 24    23 RIN                                  45


{


time_artist_activity <- function(data_frame){
  data_frame <- subset(data_frame, ms_played > 30000 & conn_country == "DE", select=c(ts, master_metadata_album_artist_name, master_metadata_track_name)) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s
  #print(head(data_frame))
  
  # Stunde extrahieren
  matches <- as.integer(regmatches(data_frame$ts, regexpr("(?<=T)\\d{2}", data_frame$ts, perl = TRUE)))
  # print(head(matches))
  activity_hours <- data.frame(hour = matches, master_metadata_album_artist_name=data_frame$master_metadata_album_artist_name, track_name=data_frame$master_metadata_track_name)
  temp <- subset(activity_hours, hour %in% c(1, 2, 3, 4, 5, 6) & master_metadata_album_artist_name=="Frank Ocean")
  print(temp)
  
  # Häufigsten Künstler pro Stunde ermitteln
  most_frequent_artist <- activity_hours %>%
    group_by(hour) %>%  # Gruppierung nach Stunde
    count(master_metadata_album_artist_name) %>%  # Zählen, wie oft jeder Künstler vorkommt
    arrange(hour, desc(n)) %>%  # Sortieren nach Stunde und Häufigkeit (absteigend)
    slice(1)
  
  result <- most_frequent_artist %>%
    rename(Artist = master_metadata_album_artist_name, Plays = n) %>%
    ungroup()
  
  # Ausgabe des Ergebnisses
  print(result, n = 24) # print() gibt aus und returned gleichzeitig
  # return(result)
}


}
