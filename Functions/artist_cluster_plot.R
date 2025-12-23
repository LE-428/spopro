# Plot the average song length and popularity together with the genre of all artists

{
  
  library(ggplot2)
  library(RColorBrewer)
  
  artist_cluster_plot <- function(df_ext){
    top_20_genres <- top_genres(df_ext, top_x = 20)
    # print(top_20_genres)
    
    # Step 1: Create the small data frame and handle NA values
    small_df <- df_ext %>%
      subset(select = c(master_metadata_album_artist_name, duration_ms, artist_followers, artist_genres)) %>%
      mutate(artist_genres = ifelse(is.na(artist_genres), "", as.character(artist_genres))) %>%
      distinct()
    
    # Step 2: Group by artist name and compute the average song length
    df_artist_grouped <- small_df %>%
      group_by(master_metadata_album_artist_name) %>%
      summarise(
        avg_duration_ms = mean(duration_ms),  # Average song length
        artist_followers = first(artist_followers),  # Assumption: all entries have the same follower count
        artist_genres = first(artist_genres)  # Genre
      ) %>%
      arrange(desc(avg_duration_ms)) %>%
      ungroup()
    
    # Step 3: Rename columns
    df_artist_grouped <- df_artist_grouped %>%
      rename(
        Artist = master_metadata_album_artist_name
      )
    
    # Step 4: Convert milliseconds to seconds
    df_artist_grouped <- df_artist_grouped %>%
      mutate(avg_duration_ms = floor(avg_duration_ms / 1000))
    
    # print(head(df_artist_grouped))
    
    
    # PLOT 
    
    # Step 1: Filter for 'avg_duration_ms' below 800
    df_artist_grouped <- df_artist_grouped %>% 
      filter(avg_duration_ms < 600)
    
    # Step 2: Create the color coding for the genres
    # Assumed top_genres; adjust if necessary
    top_genres <- top_20_genres$Genre
    palette <- brewer.pal(12, "Paired")  # Use a palette with up to 12 colors
    
    # Create a custom color palette for the remaining genres
    additional_colors <- colorRampPalette(c("gray", "red", "blue", "green", "yellow", "purple", "orange"))(8)  # 8 additional colors
    
    # Combine the two palettes
    palette <- c(palette, additional_colors)
    
    # Genre color mapping
    genre_color_map <- setNames(palette, top_genres)
    
    # Function to assign a color based on genre
    get_artist_color <- function(genres) {
      genre_list <- strsplit(genres, ",")[[1]]
      for (genre in genre_list) {
        genre <- trimws(genre)
        if (genre %in% names(genre_color_map)) {
          return(genre)
        }
      }
      return("")  # Default color for genres that are not among the top genres
    }
    
    # Step 3: Add new column 'color'
    df_artist_grouped$top_genre <- sapply(df_artist_grouped$artist_genres, get_artist_color)
    
    # Step 4: Create two data frames for gray and colored points
    df_gray <- df_artist_grouped %>% filter(top_genre == "")
    df_colored <- df_artist_grouped %>% filter(top_genre != "")
    
    # print(head(df_colored))
    print(genre_color_map)
    
    
    # Step 5: Create the scatter plot
    ggplot() +
      # Gray points (for non-top genres) 
      geom_point(data = df_gray, aes(x = avg_duration_ms, y = artist_followers), 
                 shape = 1, color = "gray", size = 3, stroke = 0.5, alpha = 0.5) +
      # Colored points (for top genres)
      geom_point(data = df_colored, aes(x = avg_duration_ms, y = artist_followers, color = top_genre), 
                 size = 3, stroke = 0.5, alpha = 0.8) +
      # Line for the average song length
      geom_vline(aes(xintercept = mean(df_artist_grouped$avg_duration_ms)), color = "red") +
      # Line for the average follower count
      geom_hline(aes(yintercept = mean(df_artist_grouped$artist_followers)), color = "blue") +
      scale_y_log10(breaks = scales::log_breaks(base = 10)) +
      ggtitle("Scatter plot of average song length and followers") +
      # theme(plot.title = element_text(hjust = 1)) +
      labs(
        x = "Average song length in seconds",
        y = "Number of artist followers"
      ) +
      theme_minimal() +
      # Set color scale manually (only for top genres)
      scale_fill_identity() +  # Uses direct color values without defining a scale
      # scale_color_manual(values = c(
      #   "german hip hop" = "#A6CEE3",
      #   "rap" = "#1F78B4",
      #   "lo-fi" = "#B2DF8A",
      #   "lo-fi beats" = "#33A02C",
      #   "cloud rap" = "#FB9A99",
      #   "hip hop" = "#E31A1C",
      #   "melodic rap" = "#FDBF6F",
      #   "west coast hip hop" = "#FF7F00",
      #   "east coast hip hop" = "#CAB2D6",
      #   "turkish hip hop" = "#6A3D9A",
    #   "emo rap" = "#FFFF99",
    #   "gangster rap" = "#B15928",
    #   "german pop" = "#BEBEBE",
    #   "rage rap" = "#F51B1B",
    #   "trap" = "#4800B6",
    #   "chill beats" = "#00916D",
    #   "jazz rap" = "#6DFF00",
    #   "southern hip hop" = "#E3BF44",
    #   "drill" = "#AD33CD",
    #   "g-funk" = "#FFA500"
    # )) +
    scale_color_manual(values = genre_color_map) + 
      theme(legend.position = "right") +  # Legend placement
      # theme(legend.title = element_text(text = "Top 20 Genres"))
      # Add a border around the entire plot
      theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.ticks.y = element_line(color = "black", linewidth = 1),  # Ticks on the Y-axis
        axis.ticks.x = element_line(color = "black", linewidth = 1),  # Ticks on the X-axis
        plot.title = element_text(hjust = 0.5)
      )
  }
  
}
