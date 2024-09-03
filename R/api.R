library(tidyverse)

rm(list=ls())


okala_key <- Sys.getenv("OKALA_API_KEY")

okala_key <- 'D2lfE2pxnrWI83daSqYqPcZDDSpwEIGT4lgNrOtv7ML5Qkk7qORwBmgvg7e46wd5MTuaVRwAMzaDuycrfH6Wuxy1Ti0PSFnHFeIF'

# okala_url <- "https://dev.api.dashboard.okala.io/api/"
okala_url <- "http://localhost:8000/api/"

##### Functions #####

auth_headers <- function(url, key){
  root <- httr2::request(okala_url)
  key
  d = list(key = key,root = root)
  return(d)
}

headers <- auth_headers(okala_url, okala_key)

### Get summary station data frame

get_station_info <- function(headers,
                             datatype=c("video","audio","image","eDNA"),
                             showURL=F){

  urlreq_ap <- httr2::req_url_path_append(headers$root,"getStations",datatype,headers$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)
  # Geojson to sf
  geojson_response = geojsonsf::geojson_sf(resp)
  return(geojson_response)

}

stations <- get_station_info(headers,datatype = "video")

### Media assets for a single station

get_media_assets <- function(key,
                             datatype=c("video","audio","image"),
                             project_system_record_id,
                             showURL=F){

  urlreq_ap <- httr2::req_url_path_append(root,"getMediaAssets",datatype,project_system_record_id,key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))

}

media_labels <- get_media_assets(okala_key,datatype="video", project_system_record_id=stations$project_system_record_id[1])

### Get project labels

get_project_labels <- function(label_type = c('Bioacoustic','Camera'),
                               key,
                               showURL=F){

  urlreq_ap <- httr2::req_url_path_append(root,"getProjectLabels",label_type, key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))

}

project_camera_labels <- get_project_labels(label_type = 'Camera' ,okala_key)

test_labels <- media_labels %>% select(label_id, label_record_id, media_file_reference_location) %>% jsonlite::toJSON()

push_new labels <- function(key,
                            showURL=F){

  urlreq_ap <- httr2::req_url_path_append(root,"updateMediaLabels", key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("PUT")  |> httr2::req_body_json(jsonlite::fromJSON(test_labels))

  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))

}






