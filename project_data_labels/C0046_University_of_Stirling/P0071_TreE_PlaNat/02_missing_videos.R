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
api_key <- "wBBR9NRSAVSV6jhdE3QN21px3WWbIkUnS7YpP8bcuBscL3IFHgi9hO4VxGgrpLO5rLRaNgyM5No5yuqsJpgeJHGOm5dWstpCkDbw"

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

media_labels <- media_labels %>%
  mutate(media_file_reference_location = str_remove(media_file_reference_location, "/Bioacoustics")) %>%
  select(media_file_reference_location, segment_start_timestamp)

## Extract file list ####

species_list <- read_csv("/Volumes/Staging area/C0046_University_of_Stirling/P0071_TreE_PlaNat_bioacoustic_data/Classified_species_list/P0071_bioacoustics_species_list.csv") %>%
  distinct(qr_code, .keep_all = T)

file_path <- "/Volumes/Staging area/C0046_University_of_Stirling/P0071_TreE_PlaNat_bioacoustic_data/Bioacoustics/"

file_list <- list.files(file_path, full.names = T, recursive = T)

bioacoustic_data <- file_list %>%
  as_tibble() %>%
  separate(value, into = c(NA,NA,NA,NA,NA,NA,NA,"qr_code","file_name"),sep = "/",remove = F) %>%
  mutate(media_file_reference_location = str_remove(value, "/Volumes/Staging area/C0046_University_of_Stirling/P0071_TreE_PlaNat_bioacoustic_data/Bioacoustics/")) %>%
  left_join(media_labels) %>%
  distinct(media_file_reference_location, .keep_all = T)

metadata <- read_csv("/Volumes/Staging area/C0046_University_of_Stirling/P0071_TreE_PlaNat_bioacoustic_data/Metadata/AM_Days_to_check.csv") %>%
  mutate(qr_code = paste0(site.code, AR.code)) %>%
  select(qr_code) %>%
  left_join(bioacoustic_data)

metadata %>%
  group_by(qr_code) %>%
  summarize(
    total_files = n(),  # Total number of files
    na_count = sum(!is.na(segment_start_timestamp)),   # Count of NA in segment_start_timestamp
  ) %>%
  arrange((qr_code))  %>%
  print( n = 30)



