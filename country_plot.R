#In welchen Ländern wurde Musik gehört
{

library(dplyr)
library(jsonlite)

country_plot <- function(data = all_data, top_x = 5){
  country_dict <- fromJSON("market_to_country.json")
  countries <- table(data$conn_country)
  
  # countries$häufigkeit <- summarise(countries, frequency = n())
  countries <- sort(countries, decreasing = TRUE)
  result <- data.frame(
    Country = unlist(country_dict[names(head(countries, n = top_x))]),
    Streams = as.integer(head(countries, n = top_x))
  )
  print(result)
  # print(countries)
  pie(
    countries[-c(1:1)], 
    main = "Streams by country, skipping the first one"
  )
  
}

}
