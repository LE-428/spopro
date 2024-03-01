add_api_data <- function(data_table = all_data, access = access_token) {
  
  uniq_t = unique(data_table)
  len_uniq = length(uniq_t$ts)
  
  uniq_t$spotify_track_uri <- substring(uniq_t$spotify_track_uri, first = 15)
  data_table$spotify_track_uri <- substring(data_table$spotify_track_uri, first = 15)
  names(data_table)[names(data_table) == "spotify_track_uri"] <- "id"
  
  output_table <- data.frame()
  
  for(i in seq(1, len_uniq, 100)){
    if (i == len_uniq - (len_uniq %% 100)) {
      index_end = len_uniq
    } else {
      index_end = i + 99
    }
    
    # ids <- substring(all_le$spotify_track_uri[i, index_end], first = 15)
    ids <- uniq_t$spotify_track_uri[i:index_end]
    
    api_table <- get_track_audio_features(id = ids, authorization = access_token)
    # test_table <- get_track(ids[1], authorization = access_token)
    
    output_table <- rbind(output_table, api_table)
    
  }
  
  return(merge(data_table, output_table, by = "id"))
}
