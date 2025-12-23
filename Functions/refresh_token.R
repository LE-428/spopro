# Refresh the token used for requesting data via the API


refresh_token <- function(){
  access_token <<- get_spotify_access_token()
  return(access_token)
}
