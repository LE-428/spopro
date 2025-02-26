# Die Künstler finden, welche am häufigsten in gehörten Features auftauchen
{
library(stringr)

top_featured_artists <- function(data_frame, top_x = 10) {
  feature_table <-
    subset(data_frame,
           data_frame$ms_played > 30000,
           select = c(master_metadata_track_name))
 matches <- feature_table$master_metadata_track_name |>
   # nach Text innerhalb von [] bzw. () suchen
   str_extract_all("\\[(.*?)\\]|\\((.*?)\\)") |>
   unlist() |>
   #str_extract_all("(?i)(?<=feat\\.\\s|WITH\\s).*") |>
   str_extract_all("(?<=feat\\.\\s|WITH\\s|ft\\.\\s)(?=[^\\)\\]]*)(.*?)(?=[\\)\\]])") |>
   unlist() 
 # print((matches))
 # Splitte die Matches nach den gewünschten Trennzeichen
 split_matches <- str_split(matches, "(\\s&\\swith\\s|\\s&\\sfeat.\\s|,\\sand\\s|,\\s|\\s&\\s|\\sand\\s|\\sfeat\\.\\s)", simplify = TRUE)
 # print(split_matches)
 # Filtere leere Strings aus und gebe den Vektor zurück
 non_empty_matches <- split_matches[split_matches != ""]
 
 # Gib den Vektor mit den nicht-leeren Matches aus
 # print(non_empty_matches)
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
 