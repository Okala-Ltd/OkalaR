## C1111 - Fauna Photonics ##
## P1111 - Pilot Project ##
## Blank species ##
## 05 / Feb / 2025 ##

## Libraries ####

library(tidyverse)
library(readxl)
library(jsonlite)
library(jsonify)
library(leaflet)

## API Functions ####

source("~/Documents/RStudio/OkalaR/R/api.R")

## Project Key ####

api_key <- "xlGFBMOFsElGcSdsemiuMjY6QWDwIQYpDdcNiIDtwzzlZZRBTgJADQiuNZeJZzPELQUHFGE5CPpdPKO7uKTAJ7j0DagnzlPI1y5q"

## Pull from API ####

headers <- auth_headers(api_key,okala_url="https://api.dashboard.okala.io/api/")

get_project()

stations <- get_station_info(hdr=headers,datatype="audio")

plot_stations(stations)

media_labels <- get_media_assets(hdr=headers,
                                 datatype="audio",
                                 psrID=stations$project_system_record_id)

media_labels %>%
  filter(label_id == "-1")

labelled_data <-  getIUCNLabels(hdr=headers,
                                         limit=10000,
                                         offset=0)


project_camera_labels <- get_project_labels(hdr=headers,labeltype='Bioacoustic')

## Media Labels manipulation ####

# label_id: 106735 - label: Aves

modified_media_labels <- media_labels %>%
  select(media_file_reference_location,segment_record_id,label_record_id,common_name,number_of_individuals)

remove_data_2 <- remove_data %>%
  select(common_name) %>%
  left_join(modified_media_labels) %>%
  mutate(label_id = "106735") %>%
  mutate(prediction_accuracy = 100)

data_to_upload <- remove_data_2 %>%
  select(-c(common_name,media_file_reference_location)) %>%
  rename("segment_record_id_fk" = segment_record_id,
         "label_id_fk" = label_id)

data_to_upload <- data_to_upload %>%
  slice(1:100)

## Push new labels ####

push_new_labels(hdr=headers, submission_records = modified_media_labels, chunksize=1000)

media_labels %>% filter(media_file_reference_location == "Cameras/PL9CT2/IMG_0608")

## segment_record_id is from the data set on the dashboard, I need to match the old file_path with the new one
## I need to find the proper label from project_camera_label (IUCN) to match it with the label_id
## add a new label_id to the actual video from the server


