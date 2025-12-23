# Which are the most listened songs by an artist? Include only own songs or also features? (feature_search_bool)

# Beispiel:

# > artist_top_tracks(all_data, 5, "juice wrld", 1)
#     track         plays
# 1        Rider    58
# 2   Not Enough    47
# 3      Robbery    45
# 4 Wishing Well    37
# 5    Righteous    28


artist_top_tracks <- function(data_frame, top_x, artist_string, feature_search_bool = TRUE, exact = FALSE){
  data_frame <- subset(data_frame, ms_played > 30000) # Filter streams shorter than 30s; Spotify counts streams after 30s
  # Escape possible special characters in the string
  artist_string <- gsub("([()])", "\\\\\\1", artist_string)
  artist_string <- gsub("\\$", "\\\\\\$", artist_string)
  if (feature_search_bool == TRUE) {
    aux_table <- data_frame[union((which(grepl(artist_string, data_frame$master_metadata_album_artist_name, ignore.case=TRUE))), (which(grepl(artist_string, data_frame$master_metadata_track_name, ignore.case=TRUE)))),]
  } else if (feature_search_bool == FALSE) {
    aux_table <- data_frame[(which(grepl(artist_string, data_frame$master_metadata_album_artist_name, ignore.case=TRUE))),]
  }
  if (exact == TRUE) {
    aux_table <- aux_table[(which(grepl(paste0("^", artist_string, "$"), aux_table$master_metadata_album_artist_name, ignore.case=TRUE))),]
  }
  # aux_table <- data_frame[union((which(grepl(artist_string, data_frame$master_metadata_album_artist_name, ignore.case=TRUE))), (which(grepl(artist_string, data_frame$master_metadata_track_name, ignore.case=TRUE)))),]
  aux_table_slim <- data.frame(artist = aux_table$master_metadata_album_artist_name, track = aux_table$master_metadata_track_name)
  sorted_table <- sort(table(aux_table_slim$track), decreasing = TRUE)
  hottest_tracks_table <- head(sorted_table, top_x)
  top_tracks <- data.frame(track = NULL, plays = NULL)
  # for(i in 1:top_x){
  #   top_tracks$track[i] = attr(hottest_tracks_table[i], "names");
  #   top_tracks$plays[i] = hottest_tracks_table[i]
  # }
  # top_tracks <- top_tracks[order(-top_tracks$plays), ]
  # return(head(top_tracks, top_x))
  return(data.frame(track = attr(hottest_tracks_table, "names"), plays = as.integer(hottest_tracks_table)))

}
