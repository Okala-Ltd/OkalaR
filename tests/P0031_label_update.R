## WCS label update ##
##

## Libraries ####

library(tidyverse)

## Directory ####

## API Functions ####

source("./R/api.R")

## API Key ####

api_key <- "s92N22o5qqxGzoXW2qBgS8yTNMym6ktM3EUJtOtqh16hl6jy4vV0Jkw5JUdo3Be32STYFHLfuMFRXceF12vAwEPIkSgtMzg2vU3B"

## Pull from API ####

headers <- auth_headers(api_key,okala_url="http:localhost:8000/api")

get_project()

stations <- get_station_info(hdr=headers,datatype= "video")

plot_stations(stations)

media_labels <- get_media_assets(hdr=headers,
                                 datatype="video",
                                 psrID=stations$project_system_record_id)

labelled_data <-  getIUCNLabels(hdr=headers,
                                limit=2000,
                                offset=0)

project_camera_labels <- get_project_labels(hdr=headers,labeltype='Camera')

## Label and file path match ####

wcs_media_labels <- media_labels %>%
  select(media_file_reference_location,segment_record_id,label_record_id, number_of_individuals)

new_labels <- read_csv("./data/data-2024-12-19T13_55_40.946Z.CSV") %>%
  select(media_file_reference_location,label)

labels <- read_csv("P0032_species_ID_list_FINAL.csv")

human_labels <- labels %>%
  select(new_vid_id,species_label,latin_name) %>%
  filter(species_label == "Human" | species_label == "Blank") %>%
  left_join(new_labels, by = c("new_vid_id" = "media_file_reference_location")) %>%
  filter(label == "Mammalia") %>%
  mutate(label =  case_when(species_label == "Human" ~ "Homo sapien",
                            T ~ label)) %>%
  select(new_vid_id,label) %>%
  mutate(label_id = case_when(label == "Homo sapien" ~ 106753,
                              label == "Mammalia" ~ -1)) %>%
  left_join(wcs_media_labels, by = c("new_vid_id" = "media_file_reference_location")) %>%
  drop_na()

species_to_upload <- human_labels %>%
  mutate(prediction_accuracy = 100) %>%
  select(-new_vid_id,-label) %>%
  rename("segment_record_id_fk" = segment_record_id,
         "label_id_fk" = label_id) %>%
  mutate_all(as.character)

test <- species_to_upload[1:100,] #%>%
filter(label_id_fk == 106753)

push_new_labels(hdr=headers, submission_records = test, chunksize=50)





