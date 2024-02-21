{
# install.packages('spotifyr')
}

{
  library(spotifyr)
  library(lubridate)
}

{
  SPOTIFY_CLIENT_ID = Sys.getenv('SPOTIFY_CLIENT_ID')
  SPOTIFY_CLIENT_SECRET = Sys.getenv('SPOTIFY_CLIENT_SECRET')
  access_token <- get_spotify_access_token()
}

add_api_data <- function(data_table = all_data, access = access_token){
  uniq_t = unique(data_table)

  # for(i in 1:length(days_table_unique$ts)){
  
  ids <- substring(all_le$spotify_track_uri[1:100], first = 15)
  
  output_table <- get_track_audio_features(id = ids, authorization = access_token)
  # test_table <- get_track(ids[1], authorization = access_token)


}
