## C0046 - University of Striling ##
## P0071 - TreE PlaNat  Bioacoustic data ##
## Missing audios ##
## 07 / March / 2025 ##

## Libraries ####

library(tidyverse)
library(exifr)
library(readxl)
library(exifr)

## API functions ####

source("R/api.R")

## API Key ####

# Cristobal's API key used for authenticating requests to the Okala API
api_key <- "ihFv9bE5gqeH4XXuaoHvmbkjcuYw34N0et0paNCnFlh0WrP9DjyGI2YlZB02NRCDfJCA0qOPckQkrL3ivO6o0zDCIhfXN60xy3yJ"

## Pull from API ####

# Authenticate API requests with headers
headers <- auth_headers(api_key, okala_url = "https://api.dashboard.okala.io/api/")

# Retrieve project name
get_project()

# Fetch station information for the project
# datatype can be "audio", "video" or "image" depending on the media type
stations <- get_station_info(hdr = headers, datatype = "audio")

# Visualize the station data on an interactive map
plot_stations(stations)

## Extract media labels ####

# datatype can be "audio", "video" or "image" depending on the media type
media_labels <- get_media_assets(hdr = headers,
                                 datatype = "audio",
                                 psrID = stations$project_system_record_id)

## Extract file list ####

file_path <- "/Volumes/Staging area/C0046_University_of_Stirling/P0071_TreE_PlaNat_bioacoustic_data/Bioacoustics/"

file_list <- list.files(file_path, full.names = T, recursive = T)

exif_data <- read_exif(file_list,tags = c("SourceFile","FileModifyDate","Comment"), quiet = F)

bioacoustic_data <- exif_data %>%
  separate(SourceFile, into = c(NA,NA,NA,NA,NA,NA,NA,"qr_code","file_name"),sep = "/",remove = F) %>%
  mutate(media_file_reference_location = str_remove(SourceFile, "/Volumes/Staging area/C0046_University_of_Stirling/P0071_TreE_PlaNat_bioacoustic_data/Bioacoustics/")) %>%
  left_join(media_labels %>% select(media_file_reference_location, segment_start_timestamp)) %>%
  relocate(Comment, .after = segment_start_timestamp) %>%
  distinct(media_file_reference_location, .keep_all = T)

bioacoustic_data %>%
  group_by(qr_code) %>%
  summarize(
    total_files = n(),  # Total number of files
    na_count = sum(!is.na(segment_start_timestamp))  # Count of NA in segment_start_timestamp
  ) %>%
  arrange((qr_code))  %>%
  print( n = 30)



