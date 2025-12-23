# Extract the most popular genres that were listened to (not every row contains genre information)

# input: data_table_ext: table with extended columns, see function add_api_data.R

# > top_genres(test, top_x = 5)
# all_genres Freq
# 1         german hip hop 1310
# 2                    rap  734
# 3                hip hop  498
# 4               rage rap  310
# 5            melodic rap  291

top_genres <- function(data_table_ext, top_x = 10){
  #data_table_ext$artist_genres <- as.character(data_table_ext$artist_genres)
  data_table_ext <- subset(data_table_ext, artist_genres != "" & ms_played > 30000)
  #print(head(data_table_ext, n = 50))
  #print(class(data_table_ext$artist_genres))
  
  # Split the list of genres for a track if multiple are specified
  genre_list <- strsplit(data_table_ext$artist_genres, ",")
  all_genres <- unlist(genre_list)
  #print(all_genres)
  
  genre_counts <- table(all_genres)
  genre_counts <- sort(genre_counts, decreasing = TRUE)
  
  result <- as.data.frame(genre_counts)
  colnames(result) <- c("Genre", "Count")
  
  result <- head(result, n = top_x)  # Select top_x genres
  
  return(result)
}
