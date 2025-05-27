## C0050 - The Biodiversity Consultancy ##
## P0077 - GAC Wet Season##
## Missing videos ##
## 01 / Apr / 2025 ##

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

api_key <- "rGJH0z9uzyc85x0tZqzTiKdbX4L0fbcvZIo6zIm0iEUGSfVkzVuTkPVK8mtWaDFMZEGeqi0nSWQJCUVNHQVvZCQ5SNZZ7nyMaih5"

## Pull from API ####

headers <- auth_headers(api_key,okala_url="https://api.dashboard.okala.io/api/")

get_project()

video_stations <- get_station_info(hdr=headers,datatype="video")

plot_stations(stations)

video_labels <- get_media_assets(hdr=headers,
                                 datatype="video",
                                 psrID=video_stations$project_system_record_id)

labelled_data <-  getIUCNLabels(hdr=headers,
                                limit=10000,
                                offset=0)

project_camera_labels <- get_project_labels(hdr = headers, labeltype = 'Camera')

## Compare against videos on server ####

file_path <- "/Volumes/Staging area/C0050_The_Biodiversity_Consultancy/P0077_GAC_Wet_Season/Cameras/"

file_list <- list.files(file_path, full.names = T, recursive = T)

exif_data <- read_exif(file_list,tags = c("SourceFile","DateTimeOriginal"), quiet = F)

video_metadata <- exif_data %>%
  separate(SourceFile, into = c(NA,NA,NA,NA,NA,NA,NA,"qr_code","folder","file_name"), sep = "/",remove = F) %>%
  group_by(qr_code) %>%
  mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal),
         instalation_timestamp = min(DateTimeOriginal),
         removal_timesttamp = max(DateTimeOriginal)) %>%
  ungroup()

## Blank videos ####

to_blank <- media_labels %>%
  separate(media_file_reference_location, into = c(NA,NA,"qr_code","folder","file_name"), sep = "/", remove = F) %>%
  filter(qr_code == "GAC_103" & folder != "102RECNX") %>%
  mutate(label_id = 33030) %>%
  select(segment_record_id,label_id,label_record_id,number_of_individuals)

data_to_upload <- to_blank %>%
  mutate(prediction_accuracy = 100,
         number_of_individuals = 1) %>%
  rename("segment_record_id_fk" = segment_record_id,
         "label_id_fk" = label_id) %>%
  drop_na()

test <- data_to_upload %>%
  slice(1:5)

## Push new labels ####

push_new_labels(hdr=headers, submission_records = data_to_upload, chunksize=3000)

## Check videos uploaded to the server ####

images_uploaded <- media_labels %>%
  separate(media_file_reference_location, into = c(NA,NA,"qr_code","folder","file_name"), sep = "/", remove = F) %>%
  filter(qr_code != "nas") %>%
  select(media_file_reference_location,qr_code,folder,file_name)

media_uploaded <- video_labels %>%
  separate(media_file_reference_location, into = c(NA,NA,"qr_code","folder","file_name"), sep = "/", remove = F) %>%
  filter(qr_code != "nas") %>%
  select(media_file_reference_location,qr_code,folder,file_name) %>%
  bind_rows(images_uploaded)

check <- video_metadata %>%
  select(SourceFile,qr_code,folder,file_name) %>%
  left_join(media_uploaded)

sum(is.na(check))

check %>%
  group_by(qr_code) %>%
  summarise(media_on_server = n_distinct(SourceFile),
            media_uploaded = sum(!is.na(media_file_reference_location)),
            media_missing = sum(is.na(media_file_reference_location))) %>%
  print(n = 100)



