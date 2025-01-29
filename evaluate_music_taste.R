# Funktion, welche aus den Anzahl der Followern der gehörten Künstler und dem
# Beliebtheitsgrad berechnet, wie ausgewählt der Musikgeschmack ist

# input: data_table_ext: Tabelle mit erweiterten Spalten, siehe Funktion add_api_data.R

# > evaluate_music_taste(all_dave_ext)
# [1] "Average artist followers"
# [1] 5270170
# [1] "Average artist popularity"
# [1] 60
# [1] "Average track popularity"
# [1] 37
# [1] 1949963

{

evaluate_music_taste <- function(data_table_ext){
  data_table_ext <- subset(data_table_ext, ms_played > 30000) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s
  
  # Durchschnittswerte berechnen
  average_artist_followers <- round(sum(data_table_ext$artist_followers) / nrow(data_table_ext))
  average_artist_popularity <- round(sum(data_table_ext$artist_popularity) / nrow(data_table_ext))
  average_track_popularity <- round(sum(data_table_ext$popularity) / nrow(data_table_ext))
  
  print("Average artist followers")
  print(average_artist_followers)
  
  print("Average artist popularity")
  print(average_artist_popularity)
  
  print("Average track popularity")
  print(average_track_popularity)
  
  # Berechne einen Wert aus den Durchschnitten der Spalten
  
  music_taste_val <- average_artist_followers * ((3 * average_track_popularity + 1 * average_track_popularity) / 4 / 100)
  #print(music_taste_val)
  print("Overall value:")
  return(music_taste_val)
}



}
