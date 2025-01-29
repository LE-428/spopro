# Extrahiere die beliebtesten Genres, welche gehört wurden (nicht bei jedem Eintrag in der Zeile sind Genre-Informationen vorhanden)

# input: data_table_ext: Tabelle mit erweiterten Spalten, siehe Funktion add_api_data.R

# > top_genres(test, top_x = 5)
# all_genres Freq
# 1         german hip hop 1310
# 2                    rap  734
# 3                hip hop  498
# 4               rage rap  310
# 5            melodic rap  291

top_genres <- function(data_table_ext, top_x = 10){
  #data_table_ext$artist_genres <- as.character(data_table_ext$artist_genres)
  data_table_ext <- subset(data_table_ext, artist_genres != "" & ms_played > 30000)
  #print(head(data_table_ext, n = 50))
  #print(class(data_table_ext$artist_genres))
  
  # Die Liste mit Genres eines Liedes aufspalten, falls mehrere angegeben
  genre_list <- strsplit(data_table_ext$artist_genres, ",")
  all_genres <- unlist(genre_list)
  #print(all_genres)
  
  genre_counts <- table(all_genres)
  genre_counts <- sort(genre_counts, decreasing = TRUE)
  
  result <- as.data.frame(genre_counts)
  return(head(result, n = top_x))
}
