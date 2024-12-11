library(tidyverse)
library(jsonlite)
library(jsonify)
library('leaflet')

#okala_URL <- "https://dev.api.dashboard.okala.io/api/"

#' Initiate root URL with API key
#'
#' Creates a base URL object that can be used as a root to call endpoints.This requires a project API key, which can be obtained directly from the Okala dashboard.
#'
#' @param api_key A valid API key
#'
#' @return This function returns a list containing the root URL and the API key
#'
#' @export
auth_headers <- function(api_key, okala_url="https://dev.api.dashboard.okala.io/api/"){
  root <- httr2::request(okala_url)
  d = list(key=api_key,
           root=root)
  return(d)
}

get_project <- function(hdr=headers){
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getProject",hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp_str <- httr2::resp_body_json(preq)
  message('Setting your active project as - ',resp_str$boundary$features[[1]]$properties$project_name)
}



#' Get project station metadata
#'
#' Retrieve all of the station data associated with your project, including video, audio, image, and eDNA data types.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param datatype A character vector of data types c("video","audio","image","eDNA")
#'
#' @return This function returns an sf object containing station metadata and geometry
#' @export
#'
get_station_info <- function(hdr,
                             datatype=c("video","audio","image","eDNA")){
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getStations",datatype,hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)
  geojson_response = geojsonsf::geojson_sf(resp)

  return(geojson_response)
}

plot_stations <- function(geojson_response){
    message('Plotting stations')
    leaflet::leaflet(data = geojson_response) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(lat=sf::st_coordinates(geojson_response)[,2],
                                lng=sf::st_coordinates(geojson_response)[,1],
                                label = ~paste(qr_code),
                                popup = ~paste("QR code: ",qr_code, "<br>",
                                               "Start time: ",project_system_record_start_timestamp, "<br>",
                                               "End time: ",project_system_record_end_timestamp, "<br>",
                                               "No. media files: ",record_count, "<br>"
                                ),
                                color = "red",
                                opacity = 0.2,
                                stroke = T,
                                fillOpacity = 0.6,
                                radius = ~ scales::rescale(record_count, c(5,15))
                                )

}



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
#'
get_media_assets <- function(hdr,
                             datatype=c("video","audio","image","eDNA"),
                             psrID){

  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getMediaAssets",datatype,hdr$key) %>%
    httr2::req_method("POST") %>% httr2::req_body_json(data=psrID)

  preq <- httr2::req_perform(urlreq_ap,verbosity=3)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())

}



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

#' Add project labels for either bioacoustics or camera so labeller have access to them in the Dashboard
#'
#' Labels are derived by using either suggested labels on the platform or by manually
#' adding labels from the wider database
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param labeltype A character vector specifying the label type ('Bioacoustic' or 'Camera')
#' @param labels A label object list like specifying the labels to be added
#'
#' @return This function returns a tibble containing project labels
#'
#' @export A success message as a list
add_project_labels <- function(hdr,
                               labeltype = c('Bioacoustic','Camera'),labels){
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"addProjectLabels",labeltype,hdr$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("POST") |> httr2::req_body_json(data=labels)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)

  return(resp)
}


replace_nas <- function(df){
  df[sapply(df,function(x) is.null(x))] = NA
  return(df)
}


#' Get labels from the wider IUCN database (all species)
#'
#' Labels are derived by using either suggested labels on the platform or by manually
#' adding labels from the wider database
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param offset An integer specifying the offset for the query
#' @param limit An integer specifying the limit for the query
#' @param search_term A character vector specifying the search term to be used (Can be left our for ful search)
#'
#' @return a list containing tabular data and pagination information for iterative calls
#'
#' @export A success message as a list
getIUCNLabels <- function(hdr, offset, limit,search_term=NULL){
  if (is.null(search_term)){
    search_term = ""
  }
  if (limit > 20000){
    stop("Limit cannot be greater than 20000")
  }
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getIUCNLabels",hdr$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("GET") |> httr2::req_url_query("offset" = offset, "limit" = limit,'search_term'= search_term)

  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)
  resp_table <- lapply(resp$table, function(x) x %>% replace_nas() %>%  tibble::as_tibble()) %>% bind_rows()

  return (list(data=resp_table, total=resp$pagination_state$total, offset=resp$pagination_state$offset, limit=resp$pagination_state$limit))
}


#' Add labels from the wider IUCN database (all species)
#'
#' adding labels from the wider database
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param offset An integer specifying the offset for the query
#' @param limit An integer specifying the limit for the query
#' @param search_term A character vector specifying the search term to be used (Can be left our for ful search)
#'
#' @return a list containing tabular data and pagination information for iterative calls
#'
#' @export A success message as a list
chunksize=200
add_IUCN_labels <- function(hdr,labels,chunksize){

  if(nrow(labels) < 100){
    message('Data is too small to chunk, submitting all data')
    chunksize = nrow(labels)
    spl.dt = list(labels)
  } else {

    if(chunksize > nrow(labels)){
      message('chunksize is bigger than length of data altering chunkszie to ', nrow(labels))
      chunksize = nrow(labels)/2
    } else {
      spl.dt <- split( labels , cut(1:nrow(labels), round(nrow(labels)/chunksize)))

    }
  }


  i = 31
  for (i in 1:length(spl.dt)){

    sub <- spl.dt[[i]]

    urlreq_ap <- httr2::req_url_path_append(hdr$root,"addIUCNLabels",hdr$key)
    urlreq_ap <- urlreq_ap |>  httr2::req_method("POST") |> httr2::req_body_json(data=spl.dt[[i]])

    preq <- httr2::req_perform(urlreq_ap,verbosity=3)
    resp <- httr2::resp_body_json(preq)

    message('submitted ',i*chunksize,' labels of ', nrow(nrow(labels)))
  }

  return(resp)
}


#' sendupdated labels a subfunction used by push_new_labels
#'
#' adding labels from the wider database
#'
#' @param  A tibble containing the records to be submitted
#' @param hdr hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param limit An integer specifying the limit for the query
#' @param search_term A character vector specifying the search term to be used (Can be left our for ful search)
#'
#' @return a list containing tabular data and pagination information for iterative calls
#'
#' @export A success message as a list

sendupatedlabels <- function(hdr,datachunk) {

  datachunk = jsonlite::toJSON(datachunk,pretty=TRUE)

  urlreq_ap <- httr2::req_url_path_append(hdr$root,"updateMediaLabels", hdr$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("PUT")  |> httr2::req_body_json(jsonlite::fromJSON(datachunk))
  #
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))
}

#' Push new labels using a chunked process
#'
#' Labels are derived by using either suggested labels on the platform or by manually
#' adding labels from the wider database
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param submission_records A tibble containing the records to be submitted
#'
#' [
#'   {
#'     "segment_record_id_fk": 0,
#'     "label_id_fk": 0,
#'     "number_of_individuals": 1,
#'     "prediction_accuracy": 100,
#'    "label_record_id": 0
#'   }
#' ]
#'
#' @param chunksize An integer specifying the chunk size for the submission

#' @return a list containing tabular data and pagination information for iterative calls
#'
#' @export A success message as a list

push_new_labels <- function(hdr,submission_records,chunksize){

  if(chunksize > nrow(submission_records)){
    message('chunksize is bigger than length of data altering chunkszie to ', nrow(labels))
    chunksize = nrow(submission_records)
  }

  spl.dt <- split( submission_records , cut(1:nrow(submission_records), round(nrow(submission_records)/chunksize)))

  for (i in 1:length(spl.dt)){

    sendupatedlabels(hdr,datachunk=spl.dt[[i]])
    message('submitted ',i*chunksize,' labels of ', nrow(submission_records))
  }
}



#' Update label list labels for either bioacoustics or camera
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

urlreq_ap <- httr2::req_url_path_append(header$root,"updateMediaLabels", header$key)
urlreq_ap <- urlreq_ap |>  httr2::req_method("PUT")  |> httr2::req_body_json(jsonlite::fromJSON(datachunk))
#
preq <- httr2::req_perform(urlreq_ap)
resp <- httr2::resp_body_string(preq)

return(jsonlite::fromJSON(resp))

#







