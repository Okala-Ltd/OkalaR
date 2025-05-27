## Client - University of Stirling ##
## Webinar Okala  ##
## 26 / May / 2025 ##

## Libraries ####

library(tidyverse)
library(readxl)
library(jsonlite)
library(jsonify)
library(leaflet)
library(exifr)

## API Functions ####

source("R/api.R")

## Project Key ####

api_key <- "opsBo5Hfoo5OBpJLokwNnVJjYXbvDggHspCLIiZFGQYxkkufRmpADoLoIbZnPbnWoq4kbtSFrkjjgTCB7ML53b4VBLunfZmlPNE8"

## Pull from API ####

770621

headers <- auth_headers(api_key,okala_url="https://api.dashboard.okala.io/api/")

get_project()

stations <- get_station_info(hdr=headers,datatype="video")

plot_stations(stations)

media_labels <- get_media_assets(hdr=headers,
                                  datatype="video",
                                  psrID=video_stations$project_system_record_id)

labelled_data <-  getIUCNLabels(hdr=headers,
                                limit=10000,
                                offset=0)

project_camera_labels <- get_project_labels(hdr = headers, labeltype = 'Camera')

## Update Labels #####

update_labels <- media_labels %>%
  separate(media_file_reference_location, into = c(NA,NA,"device_id","file_name"), sep = "/", remove = F) %>%
  filter(device_id == "01127" & label == "Meles meles") %>%
  mutate(label_id = "770621") %>%                   # Label ID for Human
  rename("segment_record_id_fk" = segment_record_id,
         "label_id_fk" = label_id) %>%
  select(segment_record_id_fk,label_id_fk,label_record_id,number_of_individuals,prediction_accuracy)


## Push new labels ####

push_new_labels(hdr=headers, submission_records = update_labels, chunksize=20)



