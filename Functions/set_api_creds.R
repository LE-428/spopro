# Function to set the Spotify API credentials as environment variables

set_api_creds <- function(client_id, client_secret) {
  # Check whether the inputs are not empty
  if (missing(client_id) || missing(client_secret) || client_id == "" || client_secret == "") {
    stop("Both Client ID and Client Secret are required.")
  }
  
  # Set the system variables for the Spotify API credentials
  Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
  
  message("Spotify API credentials have been set.")
}
