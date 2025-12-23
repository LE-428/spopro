# Test if completion rate is lower among shuffled streams

shuffle_completion <- function(df_ext){
  df_ext <- drop_podcasts(df_ext)
  df_ext <- subset(df_ext, ms_played > 500, select = c(master_metadata_album_artist_name, duration_ms, ms_played, shuffle, reason_end))
  
  # df_ext$completion = as.double(df_ext$ms_played / df_ext$duration_ms)
  df_ext$completion = as.double(ifelse(df_ext$ms_played <= df_ext$duration_ms, df_ext$ms_played / df_ext$duration_ms, NA))
  # print(head(df_ext))
  # print(head(df_ext))
  df_ext <- subset(df_ext, !is.na(completion) & reason_end == "fwdbtn")
  # df_ext <- subset(df_ext, !is.na(completion))
  # print(head(df_ext))
  
  
  df_ext <- df_ext %>% 
    group_by(shuffle) %>% 
    summarise(
      mean_completion = mean(completion)
    ) %>% 
    ungroup() %>% 
    as.data.frame()
  
  print(df_ext)
  
}
