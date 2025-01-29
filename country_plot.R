#In welchen Ländern wurde Musik gehört
{

library(dplyr)

country_plot <- function(data = all_data){
  land <- table(data$conn_country)
  
  # land$häufigkeit <- summarise(land, frequency = n())
  land <- sort(land, decreasing = TRUE)
  print(land)
  pie(land[-c(1:1)])
  
}

}
