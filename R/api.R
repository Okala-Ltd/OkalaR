library(tidyverse)
# okala_url <- "https://dev.api.dashboard.okala.io/api/"

api_key <- "D2lfE2pxnrWI83daSqYqPcZDDSpwEIGT4lgNrOtv7ML5Qkk7qORwBmgvg7e46wd5MTuaVRwAMzaDuycrfH6Wuxy1Ti0PSFnHFeIF"
#' Initiate root url and with api
#'
#' Creates a base URL object that can be used as base to call enpoints
#' You need o get a project API key from our dashboard to use this function
#'
#' @param api_key a valid API key
#' @return a list containing the root url and the api key
#' @export
auth_headers <- function(api_key){
  okala_url <- "http://localhost:8000/api/"
  root <- httr2::request(okala_url)
  d = list(key = api_key,root = root)
  return(d)
}

Çheaders <- auth_headers(api_key)

### Get summary station data frame
#' Get project station metadata
#'
#' Get all of the station data assocated with your project
#' For data types c("video","audio","image","eDNA")
#'
#' @param headers a valid API key
#' @param datatype a character vector of data types c("video","audio","image")
#' @return an sf object containing station metadata
#' @export
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

# stations <- get_station_info(headers,datatype = "video")

### Get media assets for a given station
#' Get media assets for a given project_system_record_id
#'
#' Get all of the station data assocated with your project
#' For data types c("video","audio","image")
#'
#' @param headers containing a valid API key
#' @param datatype a character vector of data types c("video","audio","image")
#' @return tibble of media assets for the given project_system_record
#' @export
get_media_assets <- function(headers,
                             datatype=c("video","audio","image"),
                             project_system_record_id,
                             showURL=F){

  urlreq_ap <- httr2::req_url_path_append(headers$root,"getMediaAssets",datatype,project_system_record_id,headers$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())

}

# media_labels <- get_media_assets(headers,datatype="video", project_system_record_id=stations$project_system_record_id[1])

### Get project labels for a given station
#' Get project labels for either bioacoustics or camera
#'
#' Labels are derived by using either suggested labels on the platform or by manulaly
#' adding labels from the wider database
#'
#' @param headers containing a valid API key
#' @param label_type a character vector dictating the label type c('Bioacoustic','Camera')
#' @return tibble of project labels
#' @export

get_project_labels <- function(headers,label_type = c('Bioacoustic','Camera'),

                               showURL=F){

  urlreq_ap <- httr2::req_url_path_append(headers$root,"getProjectLabels",label_type,headers$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())

}

project_camera_labels <- get_project_labels(headers,label_type = 'Camera')

test_labels <- media_labels %>% select(label_id, label_record_id, media_file_reference_location) %>% jsonlite::toJSON()



push_new_labels <- function(header,
                            showURL=F){

  urlreq_ap <- httr2::req_url_path_append(header$root,"updateMediaLabels", header$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("PUT")  |> httr2::req_body_json(jsonlite::fromJSON(test_labels))

  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))

}

devtools::load_all()




