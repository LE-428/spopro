# data_table Tabelle mit Rohdaten, top_x int Wert, ausgegeben werden die Top x Werte

# > top_tracks(all_mo)
# `summarise()` has grouped output by 'master_metadata_track_name'. You can override using the `.groups` argument.
# # A tibble: 5 × 3
# # Groups:   Track [5]
# Track                                     Artist       Häufigkeit
# <chr>                                     <chr>             <int>
#   1 Praise The Lord (Da Shine) (feat. Skepta) A$AP Rocky          353
# 2 Nirvana                                   RIN                 305
# 3 STARGAZING                                Travis Scott        301
# 4 Bye Bye aka Delicious                     Trettmann           235
# 5 Situation                                 Don Toliver         203

{
  library(dplyr)
  
  top_tracks <- function(data_table, top_x = 5) {
    data_table <-
      subset(data_table, ms_played > 30000) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s
    
    #häufigkeiten_track <- table(data_table$master_metadata_track_name)
    #sortierte_häufigkeiten <- sort(häufigkeiten_track, decreasing = TRUE)
    #x_häufigsten_tracks <- head(sortierte_häufigkeiten, top_x)
    
    #Ohne NA's
    
    
    grouped_df <-
      group_by(data_table,
               master_metadata_track_name,
               master_metadata_album_artist_name)
    
    häufigkeit <- summarise(grouped_df, frequency = n())
    häufigkeit <- arrange(häufigkeit, desc(frequency))
    colnames(häufigkeit) <- c("Track", "Artist", "Häufigkeit")
    
    häufigkeit <- na.omit(häufigkeit)
    x_häufigsten_tracks <- head(häufigkeit, top_x)
    
    print(x_häufigsten_tracks, n = top_x)
  }
  
}
