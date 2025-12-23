# Return top albums by number of streams
# dataframe with raw data, top_x integer value, return top_x rows


# > top_albums(all_kathi, 5)
# Album    Artist Plays
# 1 Keine Nacht für Niemand Kraftklub  1520
# 2                   Mit K Kraftklub  1453
# 3                   Drama    Shindy  1307
# 4              In Schwarz Kraftklub  1253
# 5                    KIOX    KUMMER  1089


top_albums <- function(df, top_x = 10){
  # Sort out the tracks with <30 seconds
  df <- subset(df, ms_played > 30000, select=c(master_metadata_album_album_name, master_metadata_album_artist_name))
  
  df <- df %>% 
    group_by(master_metadata_album_album_name) %>% 
    
  # Count the frequency of each album
    count(master_metadata_album_album_name, master_metadata_album_artist_name) %>% 
    
  # Sort the frequencies in descending order
    arrange(desc(n)) %>% 
    rename(
      Artist = master_metadata_album_artist_name, Album = master_metadata_album_album_name, Plays = n
    ) %>%
  # Convert the result into a data frame
    as.data.frame()
  
  # Select the top x albums
  return(head(df, n = top_x))
}
