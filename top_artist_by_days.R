# Return the artists with the most days of playback
library(dplyr)

top_artist_by_days <- function(data_frame, top_x = 10) {
  data_frame$ts <- substr(data_frame$ts, 1, 10)
  top_artists <- data_frame %>% 
    dplyr::select(ts, master_metadata_album_artist_name) %>%
    group_by(master_metadata_album_artist_name) %>% 
    summarise(Days = n_distinct(ts)) %>% 
    rename(
      Artist = master_metadata_album_artist_name
    ) %>% 
    arrange(desc(Days)) %>% 
    slice_head(n = top_x)
  print(top_artists)
}