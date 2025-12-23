{

{
  library(dplyr)
  library(jsonlite)
  # library(tidyverse)
}

# Specify directory with .json files
load_YOUR_data <- function(path){
  if (is.character(path) == TRUE) {
    json_files_directory <- path
  } else {
    json_files_directory <- "path/to/your/json/files"
  }
  
  # Create list for loaded data
  loaded_data_list <- list()
  
  # Loop over every .json file in the directory
  json_file_list <- list.files(path = json_files_directory, pattern = "\\.json$", full.names = TRUE)
  for (json_file in json_file_list) {
    loaded_data <- fromJSON(json_file)
    loaded_data_list[[json_file]] <- loaded_data
  }
  
  your_dataframe <- bind_rows(loaded_data_list)
}


{
  # y2014 <- extract_year("2014", your_dataframe)
  # y2015 <- extract_year("2015", your_dataframe)
  # y2016 <- extract_year("2016", your_dataframe)
  # y2017 <- extract_year("2017", your_dataframe)
  # y2018 <- extract_year("2018", your_dataframe)
  # y2019 <- extract_year("2019", your_dataframe)
  # y2020 <- extract_year("2020", your_dataframe)
  # y2021 <- extract_year("2021", your_dataframe)
  # y2022 <- extract_year("2022", your_dataframe)
  # y2023 <- extract_year("2023", your_dataframe)
}

}
