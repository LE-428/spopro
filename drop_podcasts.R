# Drop the rows with NA values in the Artist, Track and Album columns

drop_podcasts <- function(data_frame){
  data_frame <- subset(data_frame, master_metadata_track_name != "")
  return (data_frame)
}
