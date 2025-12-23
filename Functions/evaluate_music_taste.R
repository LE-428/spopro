# Function that computes how selective the music taste is
# based on the number of followers of the listened artists
# and their popularity

# input: data_table_ext: table with extended columns, see function add_api_data.R

# > evaluate_music_taste(all_dave_ext)
# [1] "Average artist followers"
# [1] 5270170
# [1] "Average artist popularity"
# [1] 60
# [1] "Average track popularity"
# [1] 37
# [1] 1949963

evaluate_music_taste <- function(data_table_ext){
  data_table_ext <- subset(data_table_ext, ms_played > 30000)
  
  average_artist_followers <- round(sum(data_table_ext$artist_followers) / nrow(data_table_ext))
  average_artist_popularity <- round(sum(data_table_ext$artist_popularity) / nrow(data_table_ext))
  average_track_popularity <- round(sum(data_table_ext$popularity) / nrow(data_table_ext))
  
  music_taste_val <- as.integer(average_artist_followers * ((3 * average_track_popularity + 1 * average_track_popularity) / 4 / 100))
  
  result <- paste(
    "Average artist followers:", average_artist_followers, "\n",
    "Average artist popularity:", average_artist_popularity, "\n",
    "Average track popularity:", average_track_popularity, "\n",
    "Overall value:", music_taste_val
  )
  
  # If running in the console: print directly
  if (interactive()) cat(result, "\n")
  
  # For Shiny: return as a character string
  return(invisible(result))
}
