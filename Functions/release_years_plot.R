# Returns the most frequently occurring years in the release_dates,
# requires an extended table (with API)

{
  
  
  release_years_plot <- function(data_table_ext){
    # Ensure that the release_date column is of type character
    data_table_ext$release_date <- as.character(data_table_ext$release_date)
    
    # Filter data based on ms_played
    data_table_ext <- subset(data_table_ext, ms_played > 30000)
    
    # Extract the year from the date string (YYYY-MM-DD)
    matches <- as.integer(sub("^([0-9]{4}).*", "\\1", data_table_ext$release_date))
    
    # Compute the most frequent years
    year_counts <- table(matches)
    
    
    # Compute percentages: frequency of each year / total number of rows * 100
    total_count <- sum(year_counts)  # Total number of rows
    year_percentages <- (year_counts / total_count) * 100
    
    # Print the percentage shares
    print(year_percentages)
    
    # Bar plot of the percentage distribution of years
    barplot(
      height = year_percentages,                # Percentages as bar heights
      names.arg = names(year_percentages),      # Years as x-axis labels
      col = "steelblue1",                          # Bar color
      main = "Percentage distribution of release years",  # Plot title
      xlab = "Year",                            # X-axis label
      ylab = "Percentage (%)",         # Y-axis label
      ylim = c(0, max(year_percentages) + 10)   # Adjust y-axis range
    ) 
  }
  
}
