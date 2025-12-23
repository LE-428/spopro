# Find the artists who appear most frequently as feature in dataset

# > top_featured_artists(all_mo, top_x = 5)
# Artist_name Plays
# 1     A$AP Rocky  1226
# 2  Playboi Carti   806
# 3   Travis Scott   644
# 4 Kendrick Lamar   622
# 5    Don Toliver   526

{
  library(stringr)
  
  top_featured_artists <- function(data_frame, top_x = 10) {
    feature_table <-
      subset(data_frame,
             data_frame$ms_played > 30000,
             select = c(master_metadata_track_name))
    matches <- feature_table$master_metadata_track_name |>
      # Search for text within [] or ()
      str_extract_all("\\[(.*?)\\]|\\((.*?)\\)") |>
      unlist() |>
      str_extract_all("(?<=feat\\.\\s|WITH\\s|ft\\.\\s)(?=[^\\)\\]]*)(.*?)(?=[\\)\\]])") |>
      unlist() 
    # print((matches))
    # Split matches by desired separators
    split_matches <- str_split(matches, "(\\s&\\swith\\s|\\s&\\sfeat.\\s|,\\sand\\s|,\\s|\\s&\\s|\\sand\\s|\\sfeat\\.\\s)", simplify = TRUE)
    # print(split_matches)
    # Filter out empty strings and return the vector
    non_empty_matches <- split_matches[split_matches != ""]
    
    # Print the vector of non-empty matches
    # print(unique(non_empty_matches))
    top_features <- sort(table(non_empty_matches), decreasing = TRUE)
    # print((top_features))
    top_x_features <- head(top_features, n = top_x)
    out <- data.frame(
      Artist_name = names(top_x_features), 
      Plays = as.integer(top_x_features)
    )
    print(out)
  }
  
}
