## C0047 - Forest LAB  ##
## P0019 Grand Mayumba ##
## Blank species ##
## 16 / Jan / 2025 ##

## Libraries ####

library(tidyverse)
library(readxl)
library(jsonlite)
library(jsonify)
library(leaflet)

## API Functions ####

source("~/Documents/RStudio/OkalaR/R/api.R")

## Project Key ####

api_key <- "HC5X8XlwZjGmam1Bz3AVWW8w71vYOm9iaT3Fx5PCI707Me5libkWxesPj7BmSMrN5BtyoHS4ryhewPG6biBKokgRtBCYesES7Zpk"

## Pull from API ####

headers <- auth_headers(api_key,okala_url="https://api.dashboard.okala.io/api/")

get_project()

stations <- get_station_info(hdr=headers,datatype="audio")

plot_stations(stations)

media_labels <- tibble()
i=1

for (i in seq(1, 300, by = 5)) {

  # Select the current group of 5 elements
  group_end <- min(i + 4, nrow(stations))  # Ensure no out-of-bounds error
  psr_group <- stations$project_system_record_id[i:group_end]

  # Call the function with the current group
  media_labels_sample <- get_media_assets(hdr = headers,
                                   datatype = "audio",
                                   psrID = psr_group)

  media_labels <- media_labels_sample %>%
    bind_rows(media_lables)


}


labelled_data <-  getIUCNLabels(hdr=headers,
                                         limit=10000,
                                         offset=0)


project_camera_labels <- get_project_labels(hdr=headers,labeltype='Bioacoustic')

# label_id: 106735 - label: Aves
modified_media_labels <- media_lables %>%
  mutate(label_id = case_when(order == "Charadriiformes" ~ 106735,
                              species == "Strix woodfordii" ~ 106735,
                              species == "Gallinula chloropus" ~ 106735,
                              T ~ label_id)) %>%
  select(media_file_reference_location,segment_record_id,label_record_id,label,label_id, number_of_individuals) %>%
  mutate(prediction_accuracy = 100)

remove_data <- read_csv("/Volumes/Temporary drop/Remove_labels/P0019_Grand_Mayumba/data-2025-01-20T15_17_15.103Z.CSV") %>%
  filter( label == "Smithornis rufolateralis" & prediction_accuracy < 40) %>%
  select(media_file_reference_location, data_type)

modified_media_labels <- modified_media_labels %>%
  left_join(remove_data, by = "media_file_reference_location") %>%
  mutate(label_id = case_when(!is.na(data_type) & label == "Smithornis rufolateralis" ~ 106735,
                              T ~ label_id)) %>%
  filter(label_id == 106735)

modified_media_labels_test <- modified_media_labels %>%
  filter(!is.na(data_type))

data_to_upload <- modified_media_labels %>%
  rename("segment_record_id_fk" = segment_record_id,
         "label_id_fk" = label_id) %>%
 # filter(label == "Actitis hypoleucos") %>%
  select(-c(label,media_file_reference_location,data_type))

data_to_upload <- data_to_upload %>%
  slice(1:100)

## Push new labels ####

push_new_labels(hdr=headers, submission_records = data_to_upload, chunksize=1000)

## segment_record_id is from the data set on the dashboard, I need to match the old file_path with the new one
## I need to find the proper label from project_camera_label (IUCN) to match it with the label_id
## add a new label_id to the actual video from the server
