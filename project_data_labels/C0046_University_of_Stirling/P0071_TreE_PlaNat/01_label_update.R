## C0046 - University of Striling ##
## P0071 TreE PlaNat  Bioacoustic data ##
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

api_key <- "wBBR9NRSAVSV6jhdE3QN21px3WWbIkUnS7YpP8bcuBscL3IFHgi9hO4VxGgrpLO5rLRaNgyM5No5yuqsJpgeJHGOm5dWstpCkDbw"

## remove data file ####

remove_data <- read_csv("/Volumes/Temporary drop/Remove_labels/P0071_TreE_PlaNat/remove_data.csv")

## Pull from API ####

headers <- auth_headers(api_key,okala_url="https://api.dashboard.okala.io/api/")

get_project()

stations <- get_station_info(hdr=headers,datatype="audio")

plot_stations(stations)

media_labels <- get_media_assets(hdr=headers,
                                 datatype="audio",
                                 psrID=stations$project_system_record_id)


# label_id: 106735 - label: Aves

labelled_data <- tibble()

labelled_data <-  getIUCNLabels(hdr=headers,
                                  limit=10000,
                                  offset=0)

project_camera_labels <- get_project_labels(hdr=headers,labeltype='Bioacoustic')



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
         "label_id_fk" = label_id) %>%
  drop_na()

test <- data_to_upload %>%
  slice(1:10)

## Blank species on 2nd instance ####

modified_media_labels <- media_labels %>%
  select(media_file_reference_location,segment_record_id,label_record_id,family,label,common_name,label_id, number_of_individuals) %>%
  mutate( label_id = case_when(family == "Rallidae" ~ 106735,
                               family == "Scolopacidae" ~ 106735,
                               label == "Oriolus oriolus" ~ 106735,
                               T ~ label_id),
    prediction_accuracy = 100) %>%
  rename("segment_record_id_fk" = segment_record_id,
         "label_id_fk" = label_id) %>%
  filter(label_id_fk == 106735) %>%
  select(segment_record_id_fk,label_record_id,number_of_individuals,label_id_fk,prediction_accuracy)

test <- modified_media_labels %>%
  slice(1:10)

## Push new labels ####

push_new_labels(hdr=headers, submission_records = data_to_upload, chunksize=50)

## segment_record_id is from the data set on the dashboard, I need to match the old file_path with the new one
## I need to find the proper label from project_camera_label (IUCN) to match it with the label_id
## add a new label_id to the actual video from the server
