setwd(dirname(rstudioapi::getActiveDocumentContext()[['path']]))

library(googlesheets4)
library(readr)
library(tigris)
library(tidyverse)
library(sf)

missouri_counties <- counties(state="Missouri")

biosolid_facilities <- read_sf("../data/ECHO Biosolids Facilities/all-facilities.json")

# Check if CRS matches and transform if necessary
if (!identical(st_crs(missouri_counties), st_crs(biosolid_facilities))) {
  print("CRS does not match. Transforming points to match counties CRS...")
  biosolid_facilities <- st_transform(biosolid_facilities, crs = st_crs(missouri_counties))
}



ggplot() +
  geom_sf(data = missouri_counties) +
  geom_sf(data = biosolid_facilities, color = "red", size = 2) +
  theme_minimal()


st_geometry(biosolid_facilities)
