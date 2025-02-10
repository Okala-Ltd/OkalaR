## C0132 - Nature Plus ##
## P0075 - CIB Olam ##
## Blank species ##
## 29 / Jan / 2025 ##

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
api_key <- "6BGqxoESP6m21UvVP4Ov4GwP967lQqqLGuBx2euBvPpX0FnGFRLSMtNmczJn1wrOuzXllQGed2VfA1k3BATt1w72j405gLKCneK4"

## Pull from API ####

# Authenticate API requests with headers
headers <- auth_headers(api_key, okala_url = "https://api.dashboard.okala.io/api/")

# Retrieve project name
get_project()

# Fetch station information for the project
# datatype can be "audio", "video" or "images" depending on the media type
stations <- get_station_info(hdr = headers, datatype = "audio")

# Visualize the station data on an interactive map
plot_stations(stations)

## Extract media labels ####

# datatype can be "audio", "video" or "images" depending on the media type
media_labels <- get_media_assets(hdr = headers,
                                 datatype = "audio",
                                 psrID = stations$project_system_record_id)

# Retrieve IUCN species labels from the project
# Usually the amount of species on one project is more than a 10000, so in that case don't increase the "limit" but create a loop to extract all the species
labelled_data <- getIUCNLabels(hdr = headers,
                               limit = 10000,
                               offset = 0)

# Fetch project-specific labels
# labeltype can be "Bioacoustic" or "Camera"
project_camera_labels <- get_project_labels(hdr = headers, labeltype = 'Bioacoustic')

## Modify media_labels to update labels ####

# Load a CSV file containing a list of species to be labeled as "Aves"
# This file was provided by Robin
to_remove <- read_csv("/Volumes/Temporary drop/Okala_API/P0075_CIB_Olam_remove_birds.csv")

# Prepare a subset of columns from `media_labels` for modification
# The only columns need it at the end to update labels are segment_record_id, label_record_id, number_of_individuals, label_id and prediction_accuracy

modified_media_labels <- media_labels %>%
  select(segment_record_id, label_record_id, label_id, number_of_individuals, label, prediction_accuracy)

# label_id: 106735 -> label: Aves
# label_id: -1 -> label: Blank
# For bioacoustics don't blank them, it cause an error, update the label to "Aves"
# Update `label_id` to 106735 and `prediction_accuracy` to 100 for specified species given by Robin
modified_media_labels <- modified_media_labels %>%
  mutate(
    label_id = if_else(label %in% to_remove$label, 106735, label_id),
    prediction_accuracy = if_else(label %in% to_remove$label, 100, prediction_accuracy)
  ) %>%
  arrange(label)  # Sort the modified dataset by label for easier inspection

# Check that only the wanted labels were updated to "Aves"
modified_media_labels %>%
  filter(label_id == 106735) %>%
  distinct(label)

# Prepare data for uploading by renaming and filtering relevant columns
# I recommend to upload only the labels updated to avoid any mistakes
data_to_upload <- modified_media_labels %>%
  rename("segment_record_id_fk" = segment_record_id,
         "label_id_fk" = label_id) %>%
  filter(label_id_fk == 106735) %>%  # Only keep rows where the label is "Aves"
  select(segment_record_id_fk, label_record_id, number_of_individuals, label_id_fk, prediction_accuracy) # This are the only columns need it for the API

# Create a sample of the data for testing purposes (first 10 rows)
test <- data_to_upload %>%
  slice(1:10)

## Push new labels ####

# Upload the modified labels to the API in chunks of 2000 records
push_new_labels(hdr = headers, submission_records = data_to_upload, chunksize = 2000) # The chunksize has to be lower than the total number of rows and up to 5000

