# Get an extensive overview of your Spotify data

# your_dataframe <- data.frame()
# your_dataframe <- all_le
# your_extended_dataframe <- all_le_ext

wrapped <- function(your_dataframe, your_extended_dataframe = NULL) {
  
  {
    # Get a quick overview of the dataset
    quick_stats(your_dataframe)
  }
  
  {
    # The total listening time of all your recorded streams in minutes
    all_years <- unique(substr(your_dataframe$ts, 1, 4))
    for (year in all_years) {
      print(paste(year, ":", listening_time(extract_year(year, your_dataframe)), "min"))
    }
    print(paste("All time:", listening_time(your_dataframe), "min"))
  }
  
  {
    # The days that you have listened the most music
    top_days_time(your_dataframe, top_x = 10) 
  }
  
  {
    # Show the most listened albums
    print(top_albums(your_dataframe, top_x = 10))
  }
  
  {
    # Show the track listen counter for your favorite album
    top_album <- top_albums(your_dataframe, top_x = 1)$Album_Name
    print(top_album)
    print(top_album_tracks(your_dataframe, album_string = top_album, exact_search_bool = TRUE))
  }
  
  {
    # Show the tracks that you have listened to the most often within one day
    print(track_per_year(your_dataframe, top_x = 10))
  }
  
  {
    # Show your top artists by minutes played
    print(top_artists(your_dataframe, top_x = 10))
  }
  
  {
    # Show the top tracks from most played artist
    top_artist_name <- top_artists(your_dataframe, 1)$Artist_Name
    print(top_artist_name)
    print(artist_top_tracks(your_dataframe, top_x = 10, artist_string = top_artist_name))
  }
  
  {
    # Show the artists that appear most often in songs with features
    print(top_featured_artists(your_dataframe, top_x = 10))
  }
  
  {
    # Show the top artists of each month by minutes played (use extract_year())
    recent_year <- last(unique(substr(your_dataframe$ts, 1, 4)))
    print(recent_year)
    recent_year_dataframe <- extract_year(recent_year, your_dataframe)
    artist_month(recent_year_dataframe)
  }
  
  {
    # Show the top artists by time of day
    time_artist_activity(your_dataframe)
  }
  
  {
    # Show the tracks that have been played most often 
    print("All time:")
    print(top_tracks(your_dataframe, top_x = 10))
    print("Most recent year:")
    print(top_tracks(recent_year_dataframe, top_x = 5))
  }
  
  {
    # Show most popular tracks with incognito mode enabled
    top_tracks(incognito(your_dataframe), top_x = 10)
  }
  
  {
    # Plot the listening time over the years
    time_year_plot(your_dataframe)
  }
  
  {
    # Plot with the minutes played per month
    print("All time:")
    activity_month_plot(your_dataframe)
    print("Recent year:")
    activity_month_plot(recent_year_dataframe)
  }
  
  {
    # Plot with the minutes played per hour
    activity_time_plot(your_dataframe)
  }
  
  {
    # Plot with the countries you have listened music in (connecting country)
    country_plot(your_dataframe)
  }
  
  {
    # Creates plot/pdf with your Top 100 songs over the years
    top_tracks_over_time_plot(your_dataframe)
  }
  
  {
    # Tracks that have been skipped most often
    most_skipped(your_dataframe, top_x = 10)
  }
  
  # THE FOLLOWING STATS NEED THE EXTENDED DATAFRAME WITH THE API-STATS
  
  if (missing(your_extended_dataframe) || is.null(your_extended_dataframe)) {
    print("Skipping extended dataframe statistics")
  } else {
    
    {
      # Plot with the distribution of the streams and the follower number of the artist
      artist_follower_plot(your_extended_dataframe, TRUE)
    }
    
    {
      # Plot with popularity of artist over amount of streams
      artist_popularity_plot(your_extended_dataframe, TRUE)
    }
    
    {
      # Plot with distribution of song lengths of all streams
      track_duration_plot(your_extended_dataframe, TRUE)
    }
    
    {
      # Plot with the release years of the songs streamed
      release_years_plot(your_extended_dataframe)
    }
    
    {
      # Show the top genres
      top_genres(your_extended_dataframe, top_x = 10)
    }
    
    {
      # Show the favorite artist by genre
      top_artist_by_genre(your_extended_dataframe, top_x = 10)
    }
    
    {
      # Show top genre by month
      month_genre_activity(your_extended_dataframe)
    }
    
    {
      # Show top genre by daytime
      time_genre_activity(your_extended_dataframe)
    }
    
    {
      # Evaluate the exquisiteness of your music taste
      evaluate_music_taste(your_extended_dataframe)
    }
    
  }
  
  
}

