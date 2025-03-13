## C0046 - University of Striling ##
## P0030 - UoS Pilot Project ##
## Label Update ##
## 13 / Mar / 2025 ##

## Libraries ####

library(tidyverse)
library(readxl)
library(jsonlite)
library(jsonify)
library(leaflet)

## API Functions ####

# Loading custom API functions from a local script
source("R/api.R")

## Project Key ####

# Cristobal's API key used for authenticating requests to the Okala API
api_key <- "2rIl4CxT1gxnXqVD6vyCSRqB53D3c7NmDlSAKW5o3cCBlBieoHzxPEW1voGP9lv4McH9CIF2LHHqp4Vi8aBlyFEWiEnUrFYbyRfw"

## Pull from API ####

# Authenticate API requests with headers
headers <- auth_headers(api_key, okala_url = "https://api.dashboard.okala.io/api/")

# Retrieve project name
get_project()

# Fetch station information for the project
# datatype can be "audio", "video" or "image" depending on the media type
stations <- get_station_info(hdr = headers, datatype = "video")

# Visualize the station data on an interactive map
plot_stations(stations)

## Extract media labels ####

# datatype can be "audio", "video" or "image" depending on the media type
media_labels <- get_media_assets(hdr = headers,
                                 datatype = "video",
                                 psrID = stations$project_system_record_id)

# Fetch project-specific labels
# labeltype can be "Bioacoustic" or "Camera"
project_camera_labels <- get_project_labels(hdr = headers, labeltype = 'Camera')

## Modify media_labels to update labels ####

# Load a CSV file containing a list of updated species

species_list <- read_csv("/Volumes/Archive storage/Clients/C0046_University_of_Stirling/P0030_UoS_Pilot_Project/Classified_species_list/UOS_PILOT_CLASSIFIED_SPECIES_LIST.csv") %>%
  rename_with(~ gsub(" ", "_", tolower(.x)))

# Find the label_id for each new species from "project_camera_labels"

labels_ids <- project_camera_labels %>%
  select(label,label_id)

species_list_id <- species_list %>%
  separate(species_name, into = c(NA, "label"), sep = "/", remove = F) %>%
  mutate(label = case_when(
    species_name == "Bird_sp" ~ "Aves",
    species_name == "Wood_pigeon" ~ "Columba palumbus",
    species_name == "Blank" ~ "Blank",
    T ~ label
  )) %>%
  mutate(media_file_reference_location = str_remove(file_path, "gs://okala-dmz-stg/okala-labelling-projects/uk-species-species/Stirling_university_campus")) %>%
  left_join(labels_ids) %>%
  mutate(label_id = case_when(species_name == "Blank" ~ -1,
                              T ~ label_id)) %>%
  select(media_file_reference_location,label,label_id)

# Prepare a subset of columns from `media_labels` for modification
# The only columns need it at the end to update labels are segment_record_id, label_record_id, number_of_individuals, label_id and prediction_accuracy

modified_media_labels <- media_labels %>%
  select(segment_record_id, label_record_id, number_of_individuals, media_file_reference_location) %>%
  mutate(media_file_reference_location = str_remove(media_file_reference_location,"/Cameras"),
         prediction_accuracy = 100) %>%
  left_join(species_list_id)

modified_media_labels_2 <- modified_media_labels %>%
  drop_na()

# label_id: 106735 -> label: Aves
# label_id: -1 -> label: Blank

# Prepare data for uploading by renaming and filtering relevant columns
# I recommend to upload only the labels updated to avoid any mistakes

data_to_upload <- modified_media_labels_2 %>%
  rename("segment_record_id_fk" = segment_record_id,
         "label_id_fk" = label_id) %>%
  filter(!is.na(label_id_fk)) %>%  # Only keep rows where the label is "Aves"
  select(segment_record_id_fk, label_record_id, number_of_individuals, label_id_fk, prediction_accuracy) # This are the only columns need it for the API

# Create a sample of the data for testing purposes (first 10 rows)
test <- data_to_upload %>%
  slice(1:10)

## Push new labels ####

# Upload the modified labels to the API in chunks of 2000 records
push_new_labels(hdr = headers, submission_records = test, chunksize = 5) # The chunksize has to be lower than the total number of rows and up to 5000

