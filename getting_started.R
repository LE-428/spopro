# This is an introduction to this repository if you want to run it locally
# You can run every cell like a Jupyter Notebook
# Some of the functions in the Functions folder require a connection to the Spotify API

{
  # We need to load all functions into the workspace
  source("Run\\run_all.r")
}

{
  # First we are going to load a demo dataset
  demo_data_set_path = paste0(getwd(), "\\Demo")
  
  # You can add the path to your files here and uncomment the line
  # demo_data_set_path <- "absolute/path/to/your/json/files"
  
  # This function is used to create a dataframe from all .json files found in the specified directory
  demo_df <- load_YOUR_data(demo_data_set_path)
  
  # Open the dataframe in a new tab
  view(demo_df)
}

{
  # We now extract the data from the recent year into a new dataframe
  all_years <- sort(unique(substr(demo_df$ts, 1, 4)))
  recent_year <- all_years[length(all_years) - 1]
  print(recent_year)
  
  recent_year_df <- extract_year(recent_year, demo_df)
  
  # We plot the top artists by minutes played of the recent year
  top_artists_ranking_plot(recent_year_df)
}

{
  # Which songs were repeated most often within 24 hours
  track_per_year(demo_df)
}

{
  # Plot the listening time over the years
  time_year_plot(demo_df)
}

{
  # Songs that have the lowest skipping ratio
  # The top_x parameter can be used to control the number of returned rows
  least_skipped(demo_df, top_x = 5)
}

{
  # Show tracks with most streams
  # For all years contained in the dataset
  top_tracks(recent_year_df)
  
  # Only for the last completed year
  top_tracks(demo_df)
}

{
  # Show the top tracks from most played artist
  top_artist_name <- top_artists(demo_df, top_x = 1)$Artist
  # Print the most played artist
  print(top_artist_name)
  print(artist_top_tracks(demo_df, top_x = 10, artist_string = top_artist_name))
}

{
  # Show the number of streams for your most played artists albums
  favorite_artist_albums <- artist_albums(demo_df, artist_string = top_artist_name)
  favorite_artist_favorite_album <- head(favorite_artist_albums, n = 1)$Album
  print(favorite_artist_favorite_album)
}

{
  # Show the play counts for your favorite album
  top_album_tracks(demo_df, album_string = favorite_artist_favorite_album)
}

# For the following functions that require an extended dataframe we will need to
# add additional columns to the dataframe. We do this by setting up a connection
# to the Spotify Web API. It is required to obtain API credentials on this page:
# https://developer.spotify.com/documentation/web-api


{
  # install.packages('spotifyr')
  library(spotifyr)
  # We will only need one function that is sending API requests, but you can find
  # the documentation here: https://www.rdocumentation.org/packages/spotifyr/versions/2.2.4
  
  # Use the following function to save your credentials, uncomment and insert credentials in between ""
  # e.g. client_id = "9ssk1kd83n", client_secret = "2sw8sjrd1" (actual length is 32 symbols each)
  set_api_creds(client_id = "", client_secret = "")
  
  SPOTIFY_CLIENT_ID = Sys.getenv('SPOTIFY_CLIENT_ID')
  SPOTIFY_CLIENT_SECRET = Sys.getenv('SPOTIFY_CLIENT_SECRET')
  access_token <- get_spotify_access_token()
  
  # This token is needed when using functions that interact with the API
  # After some time the token expires and needs to be refreshed
  # refresh_token()
  
  # Once this is running, we can execute the following function
  d_ext_df <- add_api_data(demo_df, access = access_token, write_to_csv = TRUE)
  # This returns the extended datafrane d_ext_df that can be used with the respective functions
  # In addition, a csv 'your_extended_dataframe.csv' is saved to the project folder
  
  # This csv can be imported so there is no need to use the API again
  # For demonstration purposes, you can comment out lines 95-99 and 106 and instead load the demo .csv file by uncommenting the next line
  # d_ext_df <- read.csv(paste0(getwd(),"\\Demo\\demo.csv"))
  
  # Take a look at the new datframe with additional columns
  view(d_ext_df)
  
}


{
  # Show the favorite artist by genre
  top_artist_by_genre(d_ext_df, top_x = 10)
}

{
  # Create graph with artists as nodes and edges between them representing collaborations (features)
  artist_graph_plot(d_ext_df)
}

{
  # Open draggable and resizable 3D plot: release years on x-axis, months in dataset on y-axis
  # Distribution of streamed songs on z-axis
  release_years_over_months_plot(d_ext_df)
}
