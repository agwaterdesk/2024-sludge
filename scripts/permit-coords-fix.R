setwd(dirname(rstudioapi::getActiveDocumentContext()[['path']]))

library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(googlesheets4)
library(sf)
library(ggplot2)


convert_utm_to_latlong <- function(file, sheet_url) {

  features_df <- read_csv(paste0("../data/permits/csv_raw/",file,".csv"))
  
  features_df <- features_df %>% 
    separate(`UTM Coordinates (Centroid)`, into = c("x.utm", "y.utm"), sep = ", Y = ") %>%
    separate(x.utm, into = c("discard", "x.utm"), sep = "= ") %>%
    select(-discard) %>% 
    mutate(
      x.utm = as.numeric(x.utm),
      y.utm = as.numeric(y.utm)
    )
  
  
  features_sf <- st_as_sf(x = features_df,                         
                          coords = c("x.utm", "y.utm"),
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

