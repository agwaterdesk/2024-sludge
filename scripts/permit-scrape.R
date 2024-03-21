setwd(dirname(rstudioapi::getActiveDocumentContext()[['path']]))

library(stringr)
library(dplyr)
library(tidyr)
library(pdftools)
library(readr)

scrape_pdf <- function(file) {
  
  # Read pages of PDF
  pages <- pdf_text(paste0("../data/permits/pdf/",file,".pdf"))
  
  # Get start and end page containing permits, subset, collapse, and convert to lines
  start_page <- which(grepl('PERMITTED FEATURE #001', pages))[1]
  end_page <- which(grepl('A. EFFLUENT LIMITATIONS AND MONITORING REQUIREMENTS', pages)) - 1
  permit_pages <- paste(pages[start_page:end_page], collapse = '')
  permit_lines <- strsplit(permit_pages, "\n") %>%  unlist()
  
  # Find indices of each feature start and end
  start_indices <- grep("PERMITTED FEATURE", permit_lines)
  end_indices <- grep("total available acres", permit_lines)
  
  # Initialize an empty list to store extracted blocks
  features <- list()
  
  # Iterate over each pair of start and end indices
  for (i in seq_along(start_indices)) {
    start_index <- start_indices[i]
    end_index <- end_indices[i]
    
    # Extract text block between start and end indices
    feature <- permit_lines[start_index:end_index]
    
    # Append block to the list
    features[[i]] <- feature
  }
  
  
  # Define regex patterns for each property
  patterns <- c(
    feature = "PERMITTED FEATURE #(\\d+)",
    description = "Land Application Field (.+?);",
    legal_description = "Legal Description:\\s+(.+)",
    utm_coordinates = "UTM Coordinates \\(Centroid\\):\\s+(.+)",
    usgs_basin = "USGS Basin & Sub-watershed No.:\\s+(.+)",
    application_rate_basis = "Application Rate Basis:\\s+(.+)",
    vegetation_type = "Vegetation Type:\\s+(.+)",
    equipment_type = "Equipment Type:\\s+(.+)",
    field_slope_max = "Field Slope, Maximum:\\s+(.+)",
    application_area = "Application Area:\\s+(.+)"
  )
  
  
  # Create empty df to store values
  features_df <- NULL
  
  # Loop over features
  for (feature in features) {
    
    # Split the text block into lines
    feature_lines <- strsplit(feature, "\n")
    
    # Extract data using regex patterns
    feature_values <- sapply(names(patterns), function(pattern_name) {
      pattern <- patterns[pattern_name]
      match <- grep(pattern, feature_lines, value = TRUE)
      if (length(match) == 0) {
        return("")
      } else {
        if (pattern_name %in% c('feature', 'description') ) {
          return(str_extract(match, pattern)[1])
        } else {
          return(trimws(unlist(str_split(str_extract(match, pattern)[1], ":", n = 2))[2]))
        }
        
      }
    })
    
    # Create a dataframe
    row <- tibble(
      feature = feature_values["feature"],
      description = feature_values["description"],
      `Legal Description` = feature_values["legal_description"],
      `UTM Coordinates (Centroid)` = feature_values["utm_coordinates"],
      `USGS Basin & Sub-watershed No.` = feature_values["usgs_basin"],
      `Application Rate Basis` = feature_values["application_rate_basis"],
      `Vegetation Type` = feature_values["vegetation_type"],
      `Equipment Type` = feature_values["equipment_type"],
      `Field Slope, Maximum` = feature_values["field_slope_max"],
      `Application Area` = feature_values["application_area"],
      permit = file
    )
    
    # Add row to features dataframe
    features_df <- bind_rows(features_df, row)
  }
  
  write_csv(features_df, paste0("../data/permits/csv_raw/",file,".csv"))
  
}


