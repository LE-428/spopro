# Get an extensive overview of your Spotify data

# your_dataframe <- data.frame()
your_dataframe <- all_le
your_extended_dataframe <- all_le_ext

{
  # Show the years included in the dataset
  get_years(your_dataframe)
}

{
  # The total listening time of all your recorded streams in minutes
  listening_time(your_dataframe)
}

{
  # The days that you have listened the most music
  top_days_time(your_dataframe, top_x = 10) 
}

{
  # Show the most listened albums
  top_albums(your_dataframe, top_x = 10)
}

{
  # Show the tracks, that you have listened to the most often within one day
  track_per_year(your_dataframe, top_x = 10)
}

{
  # Show your top artists by minutes played
  top_artists(your_dataframe, top_x = 10)
}

{
  # Show the artists, that appear most often in songs with features
  top_featured_artists(your_dataframe, top_x = 10)
}

{
  # Show the top artists of each month by minutes played (use extract_year())
  artist_month(your_dataframe)
}

{
  # Show the tracks that have been played most often 
  top_tracks(your_dataframe, top_x = 10)
}

{
  # Plot with the minutes played per month
  activity_month_plot(your_dataframe) 
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
  # Creates pdf with your Top 100 songs over the years
  top_tracks_over_time(your_dataframe) 
}

{
  # Track that have been skipped most often
  most_skipped(your_dataframe, top_x = 10)
}

# THE FOLLOWING STATS NEED THE EXTENDED DATAFRAME WITH THE API-STATS

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