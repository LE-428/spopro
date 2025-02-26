# Sort the artists by the average completion rate of their songs

artist_completion_rate <- function(df_ext, top_x = 10) {
  df_ext <- drop_podcasts(df_ext)
  df_ext <- subset(df_ext, ms_played > 1000, select = c(master_metadata_album_artist_name, duration_ms, ms_played))
  
  # Calculate completion rate only for valid rows where ms_played <= duration_ms
  df_ext$completion = as.double(ifelse(df_ext$ms_played <= df_ext$duration_ms, df_ext$ms_played / df_ext$duration_ms, NA))
  # print(max(df_ext$completion))
  
  # Renaming the column for clarity
  df_ext <- df_ext %>% 
    rename(Artist = master_metadata_album_artist_name) %>%
    # Drop rows where completion is NA
    filter(!is.na(completion)) %>%
    group_by(Artist) %>% 
    # Exclude artists with less than 10 streams
    filter(n() >= 50) %>% 
    summarise(Avg_Completion = mean(completion)) %>%
    # ungroup() %>% 
    arrange(desc(Avg_Completion)) #%>%
    # slice_head(n = top_x) %>% 
    # as.data.frame()
  
  # Print the result
  print(df_ext, n = 500)
  # print(max(df_ext$Avg_Completion))
}
