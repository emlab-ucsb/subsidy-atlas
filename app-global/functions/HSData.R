
LoadHSData <- function(input_selected_regions){
  
  # Find matching data file and load
  all_data_files <- list.files(path = "./data/region-effort-subs/", pattern = "*.csv")
  
  hs_codes <- unique(str_replace(all_data_files, "\\_.*", ""))
  matching_files <- all_data_files[hs_codes %in% input_selected_regions]
  
  out <- map_df(matching_files, function(x){read_csv(paste0("./data/region-effort-subs/", x))})
  
  out
  
}