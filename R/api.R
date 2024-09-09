library(tidyverse)
#okala_URL <- "https://dev.api.dashboard.okala.io/api/"
api_key <- "D2lfE2pxnrWI83daSqYqPcZDDSpwEIGT4lgNrOtv7ML5Qkk7qORwBmgvg7e46wd5MTuaVRwAMzaDuycrfH6Wuxy1Ti0PSFnHFeIF"

#' Initiate root URL with API key
#'
#' Creates a base URL object that can be used as a root to call endpoints.This requires a project API key, which can be obtained directly from the Okala dashboard.
#'
#' @param api_key A valid API key
#'
#' @return This function returns a list containing the root URL and the API key
#'
#' @export
auth_headers <- function(api_key){
  #okala_url <- "http://localhost:8000/api/"
  okala_url <- "https://dev.api.dashboard.okala.io/api/"
  root <- httr2::request(okala_url)
  d = list(key=api_key,
           root=root)
  return(d)
}

headers <- auth_headers(api_key)

#' Get project station metadata
#'
#' Retrieve all of the station data associated with your project, including video, audio, image, and eDNA data types.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param datatype A character vector of data types c("video","audio","image","eDNA")
#'
#' @return This function returns an sf object containing station metadata and geometry
#' @export
get_station_info <- function(hdr,
                             datatype=c("video","audio","image","eDNA")){
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getStations",datatype,hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)
  geojson_response = geojsonsf::geojson_sf(resp)
  return(geojson_response)
}

stations <- get_station_info(hdr=headers,datatype="video")

#' Retrieve media assets for a given project system record ID
#'
#' Get all of the station data associated with your project. For data types c("video","audio","image")
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param datatype A character vector of data types c("video","audio","image","eDNA")
#' @param psrID Unique project system ID for which the media assets will be retrieved
#'
#' @return This function returns a tibble of media assets for the specified project system record
#'
#' @export
get_media_assets <- function(hdr,
                             datatype=c("video","audio","image","eDNA"),
                             psrID){

  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getMediaAssets",datatype,psrID,hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())

}

media_labels <- get_media_assets(hdr=headers,
                                 datatype="video",
                                 psrID=stations$project_system_record_id[1])

#' Get project labels for either bioacoustics or camera
#'
#' Labels are derived by using either suggested labels on the platform or by manually
#' adding labels from the wider database
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param labeltype A character vector specifying the label type ('Bioacoustic' or 'Camera')
#'
#' @return This function returns a tibble containing project labels
#'
#' @export
get_project_labels <- function(hdr,
                               labeltype = c('Bioacoustic','Camera')){
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getProjectLabels",labeltype,hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())
}

project_camera_labels <- get_project_labels(hdr=headers,labeltype='Camera')

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




