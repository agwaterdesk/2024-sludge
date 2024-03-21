setwd(dirname(rstudioapi::getActiveDocumentContext()[['path']]))
library(fs)
library(dplyr)
library(googlesheets4)
library(tigris)

source("./permit-scrape.R")
source("./permit-coords-fix.R")


# Directory path
folder_path <- "../data/permits/pdf/"

sheet_url <- "https://docs.google.com/spreadsheets/d/1IS6n1DquKQFY33QBYERcH1Y-xE5NpeSEN2wJqTh5FP0/edit#gid=735693088"

# List files in the folder
files <- list.files(folder_path, full.names = TRUE)


all_permits <- NULL

# Loop over each file
for (file in files) {
  print(file)
  file <- tools::file_path_sans_ext(basename(file))
  scrape_pdf(file)
  df <- convert_utm_to_latlong(file, sheet_url)
  all_permits <- bind_rows(all_permits, df)
}


write_sheet(all_permits, sheet_url, sheet="All permits")


# Check what counties points are in

all_permits <- read_sheet(sheet_url, sheet="All permits")

mo <- counties(state="Missouri")

all_permits_sf <- st_as_sf(all_permits,                         
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84"
                        )

bbox <- st_bbox(filter(mo, NAME %in% unique(x$NAME)))

ggplot() +
  geom_sf(data = mo) +
  geom_sf(data = mutate(all_permits_sf,
    permit = case_when(
      permit == "mo0140244-synagro-sw-missouri-land-application-program-20240214-draft-pn-barry-cw" ~ "synagro",
      permit == "mo0140490-hydroag-environmental-llc-20240214-draft-pn-barry-cw"  ~ "hydroag"
    )
  ), aes(color = permit)) +
  theme_minimal() +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"]))



all_permits_sf <- st_transform(all_permits_sf, st_crs(mo))


x <- st_join(all_permits_sf, select(mo, NAME), join = st_within)

write_clip(x)




