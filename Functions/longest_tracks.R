# Find the longest tracks that have been played

longest_tracks <- function(df_ext, top_x= 5){
  df_ext <- subset(df_ext, ms_played == duration_ms, select = c(master_metadata_track_name, master_metadata_album_artist_name, master_metadata_album_album_name, duration_ms))
  # print(head(df_ext))
  df_ext <- unique(df_ext)
  df_ext$duration_ms <- as.integer(round(df_ext$duration_ms / 1000))
  df_ext <- df_ext[order(-df_ext$duration_ms), ]
  colnames(df_ext) <- c("Track", "Artist", "Album", "Duration")
  return(head(df_ext, n = top_x))
}
