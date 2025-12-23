# data_frame: table with raw data, top_x: integer, returns the top x tracks
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
  
  top_tracks <- function(data_frame, top_x = 5, filter_streams = TRUE) {
    if (filter_streams == TRUE) {
      data_frame <-
        subset(data_frame, ms_played > 30000) # Filter out streams shorter than 30s; for Spotify a stream counts after 30s
    }
    
    # Remove NAs and group data by track and artist
    grouped_df <-
      group_by(data_frame,
               master_metadata_track_name,
               master_metadata_album_artist_name)
    
    # Count frequency of each track
    frequency_df <- summarise(grouped_df, frequency = n(), .groups = "drop")
    frequency_df <- arrange(frequency_df, desc(frequency))
    colnames(frequency_df) <- c("Track", "Artist", "Plays")
    
    frequency_df <- na.omit(frequency_df)
    top_x_tracks <- head(frequency_df, top_x)
    return(top_x_tracks)
  }
  
}
