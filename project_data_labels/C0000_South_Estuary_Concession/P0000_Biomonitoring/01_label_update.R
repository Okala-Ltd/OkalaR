## C0000 - South Estuary Concession ##
## P0000 - Biomonitoring ##
## Update species ##
## 05 / Feb / 2025 ##

## Libraries ####

library(tidyverse)
library(readxl)
library(jsonlite)
library(jsonify)
library(leaflet)

## API Functions ####

source("R/api.R")

## Project Key ####

api_key <- "KsUDGC29OwWePOLxYDuJjXWgNBSYNuleOtVD1ZbWgVZSO9wbKCHClMJXMt9eq1N7NrSTnBUrwawIapXLYm0mjUZrhruXddzhTMHj"

## Pull from API ####

headers <- auth_headers(api_key,okala_url="https://api.dashboard.okala.io/api/")

get_project()

stations <- get_station_info(hdr=headers,datatype="video")

plot_stations(stations)

media_labels <- get_media_assets(hdr=headers,
                                 datatype="video",
                                 psrID=stations$project_system_record_id)

labelled_data <-  getIUCNLabels(hdr=headers,
                                         limit=10000,
                                         offset=0)

project_camera_labels <- get_project_labels(hdr=headers,labeltype='Camera')

## Media Labels manipulation ####

bio_2023 <- read_csv("/Volumes/Staging area/C0000_South_Estuary_Concession/P0000_Biomonitoring_2023/Classified_species_list/P0000_Biomonitoring_2023_Classified_Species.csv") %>%
  rename_with(~ gsub(" ", "_", tolower(.x)))

gorilla <- read_csv("/Volumes/Staging area/C0000_South_Estuary_Concession/P0000_Gorilla_Cam/Classified_species_list/P0000_Gorille_Cam_Classifies_Species.csv") %>%
  rename_with(~ gsub(" ", "_", tolower(.x)))

south_estuary <- bio_2023 %>%
  bind_rows(gorilla)

south_estuary <- south_estuary %>%
  separate(species_name, into = c(NA,"label"), sep = "/",remove = F) %>%
  mutate(label = case_when(
           label == "Loxodonta africana" ~ "Loxodonta cyclotis",
           species_name == "Chimpanzee" ~ "Pan troglodytes",
           species_name == "Bay_Duiker" ~ "Cephalophus dorsalis",
           species_name == "Buffalo" ~ "Syncerus caffer",
           species_name == "Unknown" ~ "Mammalia",
           species_name == "Bat_sp" ~ "Chiroptera",
           species_name == "Bird sp" ~ "Aves",
           species_name == "Bird_sp" ~ "Aves",
           species_name == "Gorilla" ~ "Gorilla gorilla",
           species_name == "Water_Chevrotain" ~ "Hyemoschus aquaticus",
           species_name == "Rodent" ~ "Rodentia",
           species_name == "Genet" ~ "Genetta",
           species_name == "African_Golden_Cat" ~ "Caracal aurata",
           species_name == "Primate" ~ "Primates",
           species_name == "Squirrel" ~ "Sciuridae",
           species_name == "Turtle" ~ "Blank",
           TRUE ~ label
           )) %>%
  mutate(media_file_reference_location = str_remove_all(file_path, "Biomonitoring 6X6") %>%
           str_remove_all("Gorille_cam") %>%
           str_remove_all("gs://okala-dmz-stg/SouthEstuary")
  )

labels_ids <- project_camera_labels %>%
  select(label, label_id)

south_estuary_2 <- south_estuary %>%
  left_join(labels_ids, by = "label") %>%
  mutate(label_id = case_when(label == "Blank" ~ -1,
                              T ~ label_id)) %>%
  select(media_file_reference_location, label_id)

modified_media_labels <- media_labels %>%
  select(-label_id) %>%
  mutate(media_file_reference_location = str_remove(media_file_reference_location, "/P0000_Biomonitoring_2023/Cameras"),
         media_file_reference_location = str_remove(media_file_reference_location, "/P0000_Gorilla_Cam/Cameras"),
         media_file_reference_location= str_remove(media_file_reference_location, "/20230121")) %>%
  left_join(south_estuary_2, by = "media_file_reference_location") %>%
  select(media_file_reference_location,segment_record_id,label_id,label_record_id,number_of_individuals)

data_to_upload <- modified_media_labels %>%
  mutate(prediction_accuracy = 100) %>%
  select(-media_file_reference_location) %>%
  rename("segment_record_id_fk" = segment_record_id,
         "label_id_fk" = label_id) %>%
  drop_na()

test <- data_to_upload %>%
  slice(1:300)

## Push new labels ####

push_new_labels(hdr=headers, submission_records = data_to_upload, chunksize=1000)


## segment_record_id is from the data set on the dashboard, I need to match the old file_path with the new one
## I need to find the proper label from project_camera_label (IUCN) to match it with the label_id
## add a new label_id to the actual video from the server


