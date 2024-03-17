library(dplyr)
library(rvest)
library(googlesheets4)
library(readr)

sheet_url <- 'https://docs.google.com/spreadsheets/d/1ZxK1iGJSpqtj6exll48HdJsKvMJszAX6WSUIzWIqqAM/edit#gid=27358595'


layers <- c("sdwis_05FEB2024_above", "sdwis_05FEB2024_below", "Others_2020JULY14", "Military_2023JUL26")
params <- c("pws_id", "pws_id", "ewg_id", "ewg_id")


i <- 3
layer_name <- layers[2]
param <- params[2]

scrapeSiteData("sdwis_05FEB2024_above", "popup_pfas_drinking_water", "pws_id")
scrapeSiteData("sdwis_05FEB2024_below", "popup_pfas_drinking_water", "pws_id")
scrapeSiteData("Others_2020JULY14", "popup_northeastern", "ewg_id")
scrapeSiteData("Military_2023JUL26", "popup_military", "ewg_id")

scrapeSiteData <- function(layer_name, endpoint, param) {
  
  sites_points <- read_sheet(sheet_url, sheet=paste0("points: ", layer_name))
  
  ids <- pull(sites_points, eval(param))
  
  sites_details <- NULL
  sites_chemicals <- NULL
  
  index <- 1
  
  for (id in ids) {
    tryCatch({
      print(paste0("Getting data for ", id, "... (" , index, "/", length(ids), ")"))
      index <- index + 1
      
      url <- paste0( 'https://www.ewg.org/interactive-maps/pfas_contamination/map/',endpoint,'.php?', param,'=', id)
      html <- read_html(url)
      
      # Extract list items
      list_items <- html_nodes(html, "ul.list-group li")
      list_data <- html_text(list_items)
      
      # Clean up list data
      list_data <- gsub("\n", "", list_data)
      list_data <- trimws(list_data)
      
      details <- tibble(
        "tmp" = '',
        !!param := id,
        "name"=strsplit(list_data[1], split=': ')[[1]][2],
        "state"=strsplit(list_data[2], split=': ')[[1]][2],
        "population_served"=strsplit(list_data[3], split=': ')[[1]][2]
      ) %>% 
        mutate(across(where(is.numeric), as.character))
      
      # Extract table data
      chemicals <- html_table(html_node(html, "table")) %>%
        mutate(
          "tmp" = '',
          !!param := id
        ) %>% 
        relocate(!!param) %>% 
        relocate("tmp") %>% 
        mutate(across(where(is.numeric), as.character))
      
      sites_details <- bind_rows(sites_details, details)
      sites_chemicals <- bind_rows(sites_chemicals, chemicals)
      
      
    }, error = function(e) {
      message("An error occurred:", e$message)
    })
    
  }
  
  
  write_csv(sites_details, paste0("../data/PFAS Contamination Crisis map/csv/details_", layer_name,".csv"))
  write_csv(sites_chemicals, paste0("../data/PFAS Contamination Crisis map/csv/chemicals_", layer_name,".csv"))
  
  write_sheet(sites_details, sheet_url, sheet=paste0("details: ", layer_name))
  write_sheet(sites_chemicals, sheet_url, sheet=paste0("chemicals: ", layer_name))
  
}
Ï






