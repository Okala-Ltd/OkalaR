## C0050 - The Biodiversity Consultancy ##
## P0073 - CBG ##
## Missing videos ##
## 20 / May / 2025 ##

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

api_key <- "dpLxPpGLjPAJS4uuhEXKOn0E6Th8OJubTDHXcx9DibkouZVzmMY5GGB9R8xHq1qg3bVRgmmARwg0qkDvSFExk1BxLRaXbpd67zWM"

## Pull from API ####

headers <- auth_headers(api_key,okala_url="https://api.dashboard.okala.io/api/")

get_project()

video_stations <- get_station_info(hdr=headers,datatype="image")

plot_stations(stations)

images_labels <- get_media_assets(hdr=headers,
                                 datatype="image",
                                 psrID=video_stations$project_system_record_id)

labelled_data <-  getIUCNLabels(hdr=headers,
                                limit=10000,
                                offset=0)

project_camera_labels <- get_project_labels(hdr = headers, labeltype = 'Camera')

## Compare against videos on server ####

cbg <- read_csv("/Volumes/Staging area/C0050_The_Biodiversity_Consultancy/P0073_CBG_Camera_Trap_Analysis/Metadata/exif_data.csv") %>%
  separate(SourceFile, into = c(NA,NA,NA,NA,NA,NA,NA,"season","qr_code","file_name"), sep = "/", remove = F) %>%
  select(SourceFile,season,qr_code,file_name) %>%
  mutate(qr_code = str_replace(qr_code," ","_"))


## Check videos uploaded to the server ####

dashboard <- media_labels %>%
  separate(media_file_reference_location, into = c(NA,"season","qr_code","file_name"), sep = "/", remove = F) %>%
  select(media_file_reference_location,season,qr_code,file_name)

check <- cbg %>%
  left_join(dashboard)

sum(is.na(check))

check %>%
  group_by(season,qr_code) %>%
  summarise(media_on_server = n_distinct(SourceFile),
            media_uploaded = sum(!is.na(media_file_reference_location)),
            media_missing = sum(is.na(media_file_reference_location)),
            percentage_missing = sum(is.na(media_file_reference_location))*100/media_on_server) %>%
  print(n = 100)



