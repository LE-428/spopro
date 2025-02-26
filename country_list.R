# Show table with countries and abbreviations

{

library(jsonlite)

country_list <- function(df, top_x = 5){
  # Load the dictionary mapping country codes to country names
  country_dict <- fromJSON("market_to_country.json")
  
  # Count how often each country appears in the 'conn_country' column
  countries <- table(df$conn_country)
  
  # Sort the countries by frequency in descending order
  countries <- sort(countries, decreasing = TRUE)
  
  # Extract the top_x most frequent countries and their codes
  top_countries <- head(countries, n = top_x)
  
  # Get the abbreviations (country codes) for the top countries
  abbreviations <- names(top_countries)
  
  # Create the result data frame with country names, abbreviations, and frequencies
  result <- data.frame(
    Country = unlist(country_dict[abbreviations]),    # Country name
    Abbreviation = abbreviations,                      # Country abbreviation
    Streams = as.integer(top_countries)                # Frequency of streams
  )
  
  # Display the result
  print(result)
  
  return(invisible(result))
}

}
