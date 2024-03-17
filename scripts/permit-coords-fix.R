setwd(dirname(rstudioapi::getActiveDocumentContext()[['path']]))

library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(googlesheets4)
library(sf)
library(ggplot2)


convert_ucm_to_latlong <- function(file, sheet_url) {

  features_df <- read_csv(paste0("../data/permits/csv_raw/",file,".csv"))
  
  features_df <- features_df %>% 
    separate(`UTM Coordinates (Centroid)`, into = c("x.ucm", "y.ucm"), sep = ", Y = ") %>%
    separate(x.ucm, into = c("discard", "x.ucm"), sep = "= ") %>%
    select(-discard) %>% 
    mutate(
      x.ucm = as.numeric(x.ucm),
      y.ucm = as.numeric(y.ucm)
    )
  
  
  features_sf <- st_as_sf(x = features_df,                         
                          coords = c("x.ucm", "y.ucm"),
                          crs = "+proj=utm +zone=15")
  
  #Projection transformation
  features_sf = st_transform(features_sf, crs = "+proj=longlat +datum=WGS84")
  
  #Convert it to data frame
  features_df <- as.data.frame(features_sf) %>% 
    mutate(
      longitude = st_coordinates(geometry)[,"X"],
      latitude = st_coordinates(geometry)[,"Y"]
    ) %>% 
    select(-geometry)
  
  write_csv(features_df, paste0("../data/permits/csv/",file,".csv"))
  
  write_sheet(features_df, sheet_url, sheet=file)
  
  return(features_df)
}

