# Extract the first stream of every song using the combination of track name and
# artist (uri might be different for same track, album, single version etc.)


get_first_streams <- function(df){
  df <- drop_podcasts(df)
  df <- df %>% 
    group_by(master_metadata_album_artist_name, master_metadata_track_name) %>% 
    arrange(ts) %>% 
    slice(1) %>% 
    ungroup() %>% 
    as.data.frame()
  return(df)
}
