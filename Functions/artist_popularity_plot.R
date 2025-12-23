# Plot the frequencies of artist popularities
# Requires extended table; the y-axis is scaled either by streams
# associated with artists of a certain popularity or by
# unique artists and absolute numbers

artist_popularity_plot <- function(data_table_ext, sort_by_streams = TRUE){
  if (sort_by_streams == TRUE){
    artist_popularities <- subset(data_table_ext, data_table_ext$ms_played > 30000, select = c(artist_popularity))
    artist_popularity_absolute <- table(artist_popularities)
    # print(artist_popularity_absolute)
    
    # Barplot of the percentage shares per year
    barplot(
      height = artist_popularity_absolute,                # Percentages as bar heights
      names.arg = names(artist_popularity_absolute),      # Years as X-axis labels
      col = "slateblue",                          # Bar color
      main = "Distribution of artist popularity",  # Plot title
      xlab = "Popularity",                            # X-axis label
      ylab = "Number of streams",         # Y-axis label
      ylim = c(0, max(artist_popularity_absolute) + 10)   # Adjust Y-axis
    )
    
  } else {
    artist_popularities <- subset(data_table_ext, data_table_ext$ms_played > 30000, select = c(artist_id, artist_popularity))
    unique_artists <- unique(artist_popularities)     
    # print(unique_artists)
    distribution <- table(unique_artists$artist_popularity)  
    
    # Barplot of the percentage shares per year
    barplot(
      height = distribution,                # Percentages as bar heights
      names.arg = names(distribution),      # Years as X-axis labels
      col = "slateblue",                          # Bar color
      main = "Distribution of artist popularity",  # Plot title
      xlab = "Popularity",                            # X-axis label
      ylab = "Number of artists",         # Y-axis label
      ylim = c(0, max(distribution) + 10)   # Adjust Y-axis
    ) 
  }
}
