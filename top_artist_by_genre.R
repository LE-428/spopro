# Get the top artist from each of the most popular genres

# Requires extended dataframe

# > top_artist_by_genre(all_dave_ext)
# # A tibble: 10 × 3
# Genre          Artist        Plays
# <chr>          <chr>         <int>
#   1 cloud rap      RIN             742
# 2 drill          G Herbo         350
# 3 emo rap        Juice WRLD      426
# 4 german hip hop Gzuz           1047
# 5 hip hop        Drake          1032
# 6 melodic rap    Gunna           955
# 7 rage rap       Playboi Carti   959
# 8 rap            Travis Scott   1432
# 9 techno         Boris Brejcha    91
# 10 trap           Young Thug      590


top_artist_by_genre <- function(data_table_ext, top_x = 10){
  top_x_genres <- top_genres(data_table_ext = data_table_ext, top_x = top_x)$Genre 
  data_table_ext <- subset(data_table_ext, artist_genres != "" & ms_played > 30000, select = c(master_metadata_album_artist_name, artist_genres))
  # data_table_ext$artist_genres <- strsplit(data_table_ext$artist_genres, ",")
  
  #  Spalte die Genre-Liste in einzelne Genres
  data_table_ext <- data_table_ext %>%
    mutate(artist_genres = strsplit(artist_genres, ",")) %>%
    tidyr::unnest(artist_genres) %>%  # Jedes Genre als eigene Zeile
    mutate(artist_genres = str_trim(artist_genres)) # Leerzeichen entfernen
 
  #  Finde den beliebtesten Künstler für jedes Genre
  top_artists <- data_table_ext %>%
    filter(artist_genres %in% top_x_genres) %>%
    count(artist_genres, master_metadata_album_artist_name, sort = TRUE) %>%
    group_by(artist_genres) %>%
    slice_max(n, n = 1) %>%  # Nimm den Künstler mit den meisten Zeilen pro Genre
    ungroup() %>% 
    rename(
      Genre = artist_genres,
      Artist = master_metadata_album_artist_name,
      Plays = n
    )
  
  return(top_artists)
  
}
